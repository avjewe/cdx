//! Join files together by matching column values

use crate::column::{self, ColumnSet, OutCol, ScopedValue};
use crate::comp::make_comp;
use crate::{err, get_writer, Error, Outfile, Reader, Result};
use std::cmp::Ordering;
use std::fmt;
use std::io::Write;

/// How to search and combine input files
#[derive(Debug, Copy, Clone)]
pub enum JoinType {
    /// 2 files, first never has repeated keys
    Quick,
    /// 2 files, either may have repeated keys up to "limit"
    Full,
    /// N files. Only first may have repeated keys. One output line per unique key.
    Poly,
    /// First file not sorted, binary search remaining files
    /// One output line per match in each file separately.
    Binsearch,
    /// No files are sorted, Make hash table from all but first
    /// One output line per match in each file separately.
    Hash,
}

impl Default for JoinType {
    fn default() -> Self {
        JoinType::Quick
    }
}

/// What to do if there would be duplicate column names
pub enum DupColHandling {
    /// all the usual stuff
    Std(column::DupColHandling),
    /// Any would-be duplicate names are change based on this spec
    /// spec is comma delimited list, first applies to first file, second to the second, ...
    /// last also applies to all following files.
    /// ':' is replaced with the original named and '#' is replaced with the file number
    MapIf(String),
    /// As MapIf, but always apply the mapping, even if not a duplicate
    MapAll(String),
}

impl fmt::Debug for DupColHandling {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DupColHandling")
    }
}

impl Default for DupColHandling {
    fn default() -> Self {
        DupColHandling::Std(column::DupColHandling::Fail)
    }
}

/// An output file name, matched to an input file number
#[derive(Debug, Clone, Default)]
pub struct NoMatch {
    /// zero-based file number, i.e. index into infiles
    pub file_num: usize,
    /// file name, should be OsString
    pub file_name: String,
}

impl NoMatch {
    /// new
    pub fn new(file_num: usize, file_name: &str) -> Self {
        Self {
            file_num,
            file_name: file_name.to_string(),
        }
    }
}

/// output column
#[derive(Debug, Clone, Default)]
pub struct OutColSpec {
    /// file number of source column
    file: usize,
    /// source columns
    cols: ColumnSet,
}

/// All the settings needed to join some files
#[derive(Debug, Default)]
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
    pub out_cols: Vec<OutColSpec>,
    /// skip sortedness check, normally join fails if input file is not sorted
    pub skip_check: bool,
    /// output delimiter, default tab
    pub out_delim: u8,
    /// lookback limit for JoinType::Full. Default 100.
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
        Self {
            file,
            col: col.clone(),
        }
    }
    fn new_plain(file: usize, col: usize) -> Self {
        Self {
            file,
            col: OutCol::from_num(col),
        }
    }
    fn write(&self, mut w: impl Write, f1: &Reader, f2: &Reader) -> Result<()> {
        if self.file == 0 {
            w.write_all(f1.curr().get(self.col.num))?;
        } else {
            w.write_all(f2.curr().get(self.col.num))?;
        }
        Ok(())
    }
    fn write_head(&self, mut w: impl Write, f1: &Reader, f2: &Reader) -> Result<()> {
        if self.file == 0 {
            w.write_all(f1.header().get(self.col.num).as_bytes())?;
        } else {
            w.write_all(f2.header().get(self.col.num).as_bytes())?;
        }
        Ok(())
    }
}

impl JoinConfig {
    /// create new JoinConfig. Nore thst derived 'default' is sub-optimal
    pub fn new() -> Self {
        Self {
            match_out: "-".to_string(),
            out_delim: b'\t',
            lookback_limit: 100,
            ..Self::default()
        }
    }
    /// perform the join
    pub fn join(&mut self) -> Result<()> {
        match self.jtype {
            JoinType::Quick => self.quick_join(),
            _ => err!("Not implemented"),
        }
    }
    /// perform quick_join
    /// each input line appears no more than once in an output file
    pub fn quick_join(&mut self) -> Result<()> {
        if self.infiles.len() != 2 {
            return err!(
                "{} files specified, exactly two required",
                self.infiles.len()
            );
        }

        let mut f1 = Reader::new();
        let mut f2 = Reader::new();
        f1.open(&self.infiles[0])?;
        f2.open(&self.infiles[1])?;
        if f1.is_empty() && f2.is_empty() {
            return Ok(());
        }

        let mut comp = if self.keys.is_empty() {
            make_comp("1")?
        } else if self.keys.len() == 1 {
            make_comp(&self.keys[0])?
        } else {
            make_comp(&self.keys[1])?
            /*
                    let mut cc = CompareList::new();
                    for x in &self.keys {
                    cc.push(Box::new(make_comp(x)?));
                    }
                    cc
            */
        };

        let mut nomatch: Vec<Option<Outfile>> = vec![None, None];
        for x in &self.unmatch_out {
            if x.file_num == 1 {
                if nomatch[0].is_none() {
                    let mut w = get_writer(&x.file_name)?;
                    f1.write_header(&mut w)?;
                    nomatch[0] = Some(w);
                } else {
                    return err!("Multiple uses of --also for file 1");
                }
            } else if x.file_num == 2 {
                if nomatch[1].is_none() {
                    let mut w = get_writer(&x.file_name)?;
                    f2.write_header(&mut w)?;
                    nomatch[1] = Some(w);
                } else {
                    return err!("Multiple uses of --also for file 2");
                }
            } else {
                return err!(
                    "Join had 2 input files, but requested non matching lines from file {}",
                    x.file_num
                );
            }
        }

        let mut w = get_writer(&self.match_out)?;

        // FIXME -- needs f2.names as well
        comp.lookup(&f1.names())?;

        let mut out_cols: Vec<OneOutCol> = Vec::new();
        if self.out_cols.is_empty() {
            for x in 0..f1.names().len() {
                out_cols.push(OneOutCol::new_plain(0, x));
            }
            for x in 1..f2.names().len() {
                out_cols.push(OneOutCol::new_plain(1, x));
            }
        } else {
            for x in &mut self.out_cols {
                if x.file == 0 {
                    x.cols.lookup(&f1.names())?;
                } else if x.file == 1 {
                    x.cols.lookup(&f2.names())?;
                } else {
                    return err!(
                        "Two input files, but file {} referred to as an output column",
                        x.file
                    );
                }
                for y in x.cols.get_cols() {
                    out_cols.push(OneOutCol::new(x.file, y));
                }
            }
        }
        if out_cols.is_empty() {
            return err!("No output columns specified");
        }
        w.write_all(b" CDX")?;
        for x in &out_cols {
            w.write_all(&[self.out_delim])?;
            x.write_head(&mut w, &f1, &f2)?;
        }
        w.write_all(&[b'\n'])?;

        loop {
            if f1.is_done() || f2.is_done() {
                break;
            }
            match comp.comp_cols(f1.curr(), f2.curr()) {
                Ordering::Equal => {
                    out_cols[0].write(&mut w, &f1, &f2)?;
                    for x in &out_cols[1..] {
                        w.write_all(&[self.out_delim])?;
                        x.write(&mut w, &f1, &f2)?;
                    }
                    w.write_all(&[b'\n'])?;

                    f1.getline()?;
                    f2.getline()?;
                }
                Ordering::Less => {
                    if let Some(x) = &mut nomatch[0] {
                        f1.write(x)?;
                    }
                    f1.getline()?;
                }
                Ordering::Greater => {
                    if let Some(x) = &mut nomatch[1] {
                        f2.write(x)?;
                    }
                    f2.getline()?;
                }
            }
        }
        while !f1.is_done() {
            if let Some(x) = &mut nomatch[0] {
                f1.write(x)?;
            }
            f1.getline()?;
        }
        while !f2.is_done() {
            if let Some(x) = &mut nomatch[1] {
                f2.write(x)?;
            }
            f2.getline()?;
        }
        // drain f1 or f2 as needed
        Ok(())
    }
}
