//! Join files together by matching column values

use crate::column::{OutCol, ScopedValue};
use crate::comp::CompMaker;
use crate::prelude::*;
use crate::util::Outfile;

/// How to search and combine input files
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum JoinType {
    /// All files sorted. Files 2-N never have repeated keys
    #[default]
    Quick,
    /// All files sorted. Files 2-N may have repeated keys up to "limit", giving `NxM` output lines
    Full,
    /// All files sorted. Files 2-N never have repeated keys.
    /// One output line per unique key from any file
    /// no "unmatching file" output
    Poly,
    /// First file not sorted, binary search remaining files
    /// One output line per match in each file separately.
    Binsearch,
    /// No files are sorted, Make hash table from all but first
    /// One output line per match in each file separately.
    Hash,
}

/// An output file name, matched to an input file number
#[derive(Debug, Clone, Default)]
pub struct NoMatch {
    /// zero-based file number, i.e. index into infiles
    pub file_num: usize,
    /// file name, should be `OsString`
    pub file_name: String,
}

impl NoMatch {
    /// new
    #[must_use]
    pub fn new(file_num: usize, file_name: &str) -> Self {
        Self { file_num, file_name: file_name.to_string() }
    }
}

/// output column
#[derive(Debug, Default, Clone)]
pub struct OutColSpec {
    /// file number of source column
    file: usize,
    /// source columns
    cols: ColumnSet,
}

impl OutColSpec {
    /// new
    #[must_use]
    pub const fn new(file: usize, cols: ColumnSet) -> Self {
        Self { file, cols }
    }
}

/// All the settings needed to join some files
#[derive(Debug, Default, Clone)]
#[non_exhaustive]
pub struct JoinConfig {
    /// the type, default Quick
    pub jtype: JoinType,
    /// input file names
    pub infiles: Vec<String>,
    /// primary output file name, default "-"
    pub match_out: String,
    /// output files for unmatched lines
    pub unmatch_out: Vec<NoMatch>,
    /// output columns, e.g. 0,1.1-2,2.title-author. Default is joined cols, everything from
    /// file 1 except joined cols, everything from file 2 except joined cols, ...
    /// if empty, create a reasonable default
    pub col_specs: Vec<OutColSpec>,
    /// skip sortedness check, normally join fails if input file is not sorted
    pub skip_check: bool,
    /// output delimiter, default tab
    pub out_delim: u8,
    /// lookback limit for `JoinType::Full`. Default 100.
    pub lookback_limit: u32,
    /// Comparator - default is "1", that is first column plain comparison
    pub keys: Vec<String>,
    /// What to do if join would generate duplicate column names. Default is Fail.
    pub dups: DupColHandling,
    /// if true, replace empty column with value from other file
    pub use_other: bool,
    /// default value for non-matching lines. Scoped wrt the output columns.
    pub empty: ScopedValue,
}

#[derive(Clone, Default, Debug)]
struct OneOutCol {
    file: usize,
    col: OutCol,
}

impl OneOutCol {
    fn new(file: usize, col: &OutCol) -> Self {
        Self { file, col: col.clone() }
    }
    const fn new_plain(file: usize, col: usize) -> Self {
        Self { file, col: OutCol::from_num(col) }
    }
    fn write(&self, mut w: impl Write, f: &[Reader]) -> Result<()> {
        w.write_all(f[self.file].curr().get(self.col.num))?;
        Ok(())
    }
    fn write_head(&self, mut w: impl Write, f: &[Reader]) -> Result<()> {
        w.write_all(f[self.file].header().get(self.col.num).as_bytes())?;
        Ok(())
    }
}

impl JoinConfig {
    /// create new `JoinConfig`. Note that derived 'default' is sub-optimal
    #[must_use]
    pub fn new() -> Self {
        Self {
            match_out: "-".to_string(),
            out_delim: b'\t',
            lookback_limit: 100,
            ..Self::default()
        }
    }
    /// perform the join
    pub fn join(&self) -> Result<()> {
        Joiner::new(self)?.join(self)
    }
}

/// does the actual joining
struct Joiner {
    r: Vec<Reader>,
    comp: LineCompList,
    yes_match: Outfile,
    no_match: Vec<Option<Outfile>>,
    out_cols: Vec<OneOutCol>,
}

impl Joiner {
    fn new(config: &JoinConfig) -> Result<Self> {
        Ok(Self {
            r: Vec::new(),
            comp: LineCompList::new(),
            yes_match: get_writer(&config.match_out)?,
            no_match: Vec::new(),
            out_cols: Vec::new(),
        })
    }
    fn join(&mut self, config: &JoinConfig) -> Result<()> {
        if config.infiles.len() < 2 {
            return err!("Join requires at least two input files, {} found", config.infiles.len());
        }

        for x in &config.infiles {
            self.r.push(Reader::new_open2(x)?);
        }

        for _x in 0..config.infiles.len() {
            self.no_match.push(None);
        }
        for x in &config.unmatch_out {
            if (x.file_num < 1) || (x.file_num > config.infiles.len()) {
                return err!(
                    "Join had {} input files, but requested non matching lines from file {}",
                    config.infiles.len(),
                    x.file_num
                );
            }
            let num = x.file_num - 1;
            if self.no_match[num].is_none() {
                let mut w = get_writer(&x.file_name)?;
                self.r[num].write_header(&mut *w)?;
                self.no_match[num] = Some(w);
            } else {
                return err!("Multiple uses of --also for file {}", x.file_num);
            }
        }

        if config.keys.is_empty() {
            self.comp.push(CompMaker::make_line_comp("1")?);
        } else {
            for x in &config.keys {
                self.comp.push(CompMaker::make_line_comp(x)?);
            }
        }
        for i in 0..self.r.len() {
            self.comp.lookup_n(&self.r[i].names(), i)?;
        }

        if config.col_specs.is_empty() {
            for f in 0..self.r.len() {
                let used = self.comp.used_cols(f);
                for x in 0..self.r[f].names().len() {
                    if (f == 0) || !used.contains(&x) {
                        self.out_cols.push(OneOutCol::new_plain(f, x));
                    }
                }
            }
        } else {
            for x in &config.col_specs {
                let mut x = x.clone();
                if x.file >= self.r.len() {
                    return err!(
                        "{} input files, but file {} referred to as an output column",
                        self.r.len(),
                        x.file
                    );
                }
                x.cols.lookup(&self.r[x.file].names())?;
                for y in x.cols.get_cols() {
                    self.out_cols.push(OneOutCol::new(x.file, y));
                }
            }
        }
        if self.out_cols.is_empty() {
            return err!("No output columns specified");
        }

        if self.r[0].has_header() {
            self.yes_match.write_all(b" CDX")?;
            for x in &self.out_cols {
                self.yes_match.write_all(&[config.out_delim])?;
                x.write_head(&mut *self.yes_match, &self.r)?;
            }
            self.yes_match.0.write_all(b"\n")?;
        }
        if config.jtype == JoinType::Quick {
            self.join_quick(config)
        } else {
            err!("Only quick supported")
        }
    }
    fn join_quick(&mut self, config: &JoinConfig) -> Result<()> {
        if !self.r[0].is_done() && !self.r[1].is_done() {
            let mut cmp = self.comp.comp_cols_n(self.r[0].curr(), self.r[1].curr(), 0, 1);
            'outer: loop {
                match cmp {
                    Ordering::Equal => loop {
                        self.out_cols[0].write(&mut *self.yes_match, &self.r)?;
                        for x in &self.out_cols[1..] {
                            self.yes_match.write_all(&[config.out_delim])?;
                            x.write(&mut *self.yes_match, &self.r)?;
                        }
                        self.yes_match.write_all(b"\n")?;
                        if self.r[0].getline()? {
                            self.r[1].getline()?;
                            break 'outer;
                        }
                        cmp = self.comp.comp_cols_n(self.r[0].curr(), self.r[1].curr(), 0, 1);
                        if cmp != Ordering::Equal {
                            if self.r[1].getline()? {
                                break 'outer;
                            }
                            cmp = self.comp.comp_cols_n(self.r[0].curr(), self.r[1].curr(), 0, 1);
                            break;
                        }
                    },
                    Ordering::Less => {
                        if let Some(x) = &mut self.no_match[0] {
                            self.r[0].write(&mut x.0)?;
                        }
                        if self.r[0].getline()? {
                            break;
                        }
                        cmp = self.comp.comp_cols_n(self.r[0].curr(), self.r[1].curr(), 0, 1);
                    }
                    Ordering::Greater => {
                        if let Some(x) = &mut self.no_match[1] {
                            self.r[1].write(&mut x.0)?;
                        }
                        if self.r[1].getline()? {
                            break;
                        }
                        cmp = self.comp.comp_cols_n(self.r[0].curr(), self.r[1].curr(), 0, 1);
                    }
                }
            }
        }
        while !self.r[0].is_done() {
            if let Some(x) = &mut self.no_match[0] {
                self.r[0].write(&mut x.0)?;
            }
            self.r[0].getline()?;
        }
        while !self.r[1].is_done() {
            if let Some(x) = &mut self.no_match[1] {
                self.r[1].write(&mut x.0)?;
            }
            self.r[1].getline()?;
        }
        Ok(())
    }
}
