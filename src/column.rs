//! Handles conversion between named column sets and lists of column numbers
//! Also helps with selecting those columns from a line of text

use crate::expr::Expr;
use crate::matcher::MatchMaker;
use crate::text::Text;
use crate::trans::TransList;
use crate::util::{err, find_close, Error, Result, StringLine, TextLine};
use std::collections::HashSet;
use std::io::Write;
use std::{fmt, str};

/// What to do if there would be duplicate column names
#[derive(Debug, Copy, Clone)]
pub enum DupColHandling {
    /// Fail if there are duplicate columns names
    Fail,
    /// Allow duplicate columns names
    Allow,
    /// Allow duplicate columns names, but write a message to stderr
    Warn,
    /// if "foo" is taken, try "foo1", then "foo2" until it's unique
    Numeric,
}

impl DupColHandling {
    /// new
    pub fn new(spec: &str) -> Result<Self> {
        if spec.to_ascii_lowercase() == "fail" {
            Ok(Self::Fail)
        } else if spec.to_ascii_lowercase() == "allow" {
            Ok(Self::Allow)
        } else if spec.to_ascii_lowercase() == "warn" {
            Ok(Self::Warn)
        } else if spec.to_ascii_lowercase() == "numeric" {
            Ok(Self::Numeric)
        } else {
            err!("Duplicate Column Mode must be one of : Fail, Allow, Warn, Numeric")
        }
    }
}

impl Default for DupColHandling {
    fn default() -> Self {
        Self::Fail
    }
}

/// build the CDX header for an output file
#[derive(Debug, Default, Clone)]
pub struct ColumnHeader {
    cols: Vec<String>,
    dups: DupColHandling,
}

/// is 'name' a valid column name
pub fn is_valid_column_name(name: &str) -> bool {
    get_col_name(name) == name.len()
}

/// throw an error is name is not a valid column name
pub fn validate_column_name(name: &str) -> Result<()> {
    if !is_valid_column_name(name) {
        err!("Invalid column name {}", name)
    } else {
        Ok(())
    }
}

/// extract column from line
pub fn get_col(data: &[u8], col: usize, delim: u8) -> &[u8] {
    for (n, s) in data.split(|ch| *ch == delim).enumerate() {
        if n == col {
            return if !s.is_empty() && s.last().unwrap() == &b'\n' {
                &s[0..s.len() - 1]
            } else {
                s
            };
        }
    }
    &data[0..0]
}

impl ColumnHeader {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// set the DupColHandling mode
    pub fn set_handling(&mut self, dups: DupColHandling) {
        self.dups = dups;
    }
    /// clear
    pub fn clear(&mut self) {
        self.cols.clear();
    }
    /// add all of the columns from 'cols'
    pub fn push_all(&mut self, cols: &StringLine) -> Result<()> {
        for x in cols.into_iter() {
            self.push(x)?;
        }
        Ok(())
    }

    /// do we already have this column name
    pub fn contains(&self, name: &str) -> bool {
        for x in &self.cols {
            if x == name {
                return true;
            }
        }
        false
    }

    /// add new column name
    pub fn push(&mut self, name: &str) -> Result<()> {
        validate_column_name(name)?;
        if self.contains(name) {
            match self.dups {
                DupColHandling::Fail => return err!("Duplicate column name {}", name),
                DupColHandling::Allow => self.cols.push(name.to_string()),
                DupColHandling::Warn => {
                    self.cols.push(name.to_string());
                    eprintln!("Warning : creating duplicate column name {}", name);
                }
                DupColHandling::Numeric => {
                    for x in 1..10000 {
                        let new_name = format!("name{}", x);
                        if !self.contains(&new_name) {
                            self.cols.push(new_name);
                            break;
                        }
                    }
                }
            }
        } else {
            self.cols.push(name.to_string());
        }
        Ok(())
    }

    /// get new string, which is the full CDX header, including newline
    pub fn get_head(&self, delim: u8) -> String {
        let mut res = String::with_capacity(self.get_size() + 6);
        res.push_str(" CDX");
        res.push(delim as char);
        self.add_head(&mut res, delim);
        res.push('\n');
        res
    }

    /// get new string, which is the column names joined
    pub fn get_head_short(&self, delim: u8) -> String {
        let mut res = String::with_capacity(self.get_size());
        self.add_head(&mut res, delim);
        res
    }

    fn get_size(&self) -> usize {
        self.cols.iter().map(|v| v.len()).sum::<usize>() + self.cols.len() - 1
    }
    /// append column names, joined
    fn add_head(&self, res: &mut String, delim: u8) {
        if self.cols.is_empty() {
            return;
        }
        res.push_str(&self.cols[0]);
        for x in self.cols.iter().skip(1) {
            res.push(delim as char);
            res.push_str(x);
        }
    }
}

/// A column set is a collection of column specifictions, which when interpreted
/// in the context of a set of named columns, produces a list of column numbers
/// It contains a bunch of "yes" specs and "no" specs.
/// If there are no "yes" specs, all columns are marked "yes"
/// The resulting list of column numbers is all of the "yes" columns in order,
/// minus any "no" columns. Thus if a column appears in both, it will be excluded.
/// # Examples
///
/// ```
///    use cdx::column::ColumnSet;
///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
///
///    let mut s = ColumnSet::new();
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[0,1,2,3,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes("-");
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[0,1,2,3,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes("one-three");
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[1,2,3]);
///
///    let mut s = ColumnSet::new();
///    s.add_no("three,one");
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[0,2,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_no("+4-+2");
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[0,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes("(range,>s<=two)");
///    s.lookup(&header);
///    assert_eq!(s.get_cols_num(), &[2,3]);
/// ```
#[derive(Debug, Default, Clone)]
pub struct ColumnSet {
    pos: Vec<ColSetPart>,
    neg: Vec<ColSetPart>,
    columns: Vec<OutCol>,
    did_lookup: bool,
    trans: Vec<TransList>,
}

#[derive(Debug, Clone, Default)]
struct ColSetPart {
    val: String,
    trans: Option<usize>,
}
impl ColSetPart {
    fn new(spec: &str, trans: Option<usize>) -> Self {
        Self {
            val: spec.to_string(),
            trans,
        }
    }
}

/// named output columns
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct OutCol {
    /// column number
    pub num: usize,
    /// column name, might be empty if no name available
    pub name: String,
    /// which TransList to use
    trans: Option<usize>,
}

impl OutCol {
    /// new
    pub fn new(num: usize, name: &str) -> Self {
        Self {
            num,
            name: name.to_string(),
            trans: None,
        }
    }
    /// new with name and trans
    pub fn new_trans(num: usize, name: &str, trans: usize) -> Self {
        Self {
            num,
            name: name.to_string(),
            trans: Some(trans),
        }
    }
    /// new with empty name
    pub const fn from_num(num: usize) -> Self {
        Self {
            num,
            name: String::new(),
            trans: None,
        }
    }
    /// new with empty name but a trans
    pub const fn from_num_trans(num: usize, trans: usize) -> Self {
        Self {
            num,
            name: String::new(),
            trans: Some(trans),
        }
    }
}

/// A ColumnSet with associated string
#[derive(Debug, Default, Clone)]
pub struct ScopedValue {
    cols: ColumnSet,
    value: String,
}

impl ScopedValue {
    /// one string
    pub fn new(spec: &str, del: char) -> Result<Self> {
        let mut s = Self::default();
        if spec.is_empty() {
            return Ok(s);
        }
        let mut spec = spec;
        let ch = spec.first();
        if ch == del {
            spec.skip_first();
            if spec.is_empty() {
                s.value.push(del);
                return Ok(s);
            }
            let ch = spec.first();
            if ch == del {
                s.value.push(del);
            }
        }
        while !spec.is_empty() {
            let ch = spec.take_first();
            if ch == del {
                s.cols.add_yes(spec)?;
                return Ok(s);
            }
            s.value.push(ch);
        }
        Ok(s)
    }
    /// two strings
    pub fn new2(value: &str, cols: &str) -> Result<Self> {
        let mut s = Self::default();
        s.cols.add_yes(cols)?;
        s.value = value.to_string();
        Ok(s)
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.cols.lookup(fieldnames)
    }
}

/// A per-column value
#[derive(Debug, Default, Clone)]
pub struct ScopedValues {
    default: String,
    data: Vec<ScopedValue>,

    has_value: Vec<bool>,
    strings: Vec<String>,
    ints: Vec<i64>,
    floats: Vec<f64>,
}

impl ScopedValues {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// value for column not otherwise assigned a value
    pub fn set_default(&mut self, d: &str) {
        self.default = d.to_string();
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.has_value.clear();
        self.has_value.resize(fieldnames.len(), false);
        self.strings.clear();
        self.strings.resize(fieldnames.len(), self.default.clone());
        for x in &mut self.data {
            x.lookup(fieldnames)?;
            for i in 0..x.cols.columns.len() {
                self.strings[i] = x.value.clone();
                self.has_value[i] = true;
            }
        }
        Ok(())
    }
    /// do the work so that get_int() will work properly
    pub fn make_ints(&mut self) -> Result<()> {
        self.ints.clear();
        for i in 0..self.strings.len() {
            self.ints[i] = self.get(i).parse::<i64>()?;
        }
        Ok(())
    }
    /// do the work so that get_int() will work properly
    pub fn make_floats(&mut self) -> Result<()> {
        self.floats.clear();
        for i in 0..self.strings.len() {
            self.floats[i] = self.get(i).parse::<f64>()?;
        }
        Ok(())
    }

    /// add one ScopedValue
    pub fn add(&mut self, spec: &str, del: char) -> Result<()> {
        self.data.push(ScopedValue::new(spec, del)?);
        Ok(())
    }
    /// add one ScopedValue
    pub fn add2(&mut self, value: &str, cols: &str) -> Result<()> {
        self.data.push(ScopedValue::new2(value, cols)?);
        Ok(())
    }
    /// get appropriate value for the column
    pub fn get(&self, col: usize) -> &str {
        if col < self.strings.len() {
            &self.strings[col]
        } else {
            self.strings.last().unwrap()
        }
    }
    /// get appropriate int value for the column
    pub fn get_int(&self, col: usize) -> i64 {
        if col < self.ints.len() {
            self.ints[col]
        } else {
            *self.ints.last().unwrap()
        }
    }
    /// get appropriate int value for the column
    pub fn get_float(&self, col: usize) -> f64 {
        if col < self.floats.len() {
            self.floats[col]
        } else {
            *self.floats.last().unwrap()
        }
    }
    /// has this column been assigned a value?
    pub fn has_value(&self, col: usize) -> bool {
        if col < self.has_value.len() {
            self.has_value[col]
        } else {
            false
        }
    }
    /// Have any column been assigned values?
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// Write some output
pub trait ColumnFun {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()>;
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, delim: u8) -> Result<()>;
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()>;
}

/// write a single column
#[derive(Debug, Default)]
pub struct ColumnExpr {
    name: String,
    expr: Expr,
}

impl ColumnExpr {
    /// new from Expr or Name:Expr
    pub fn new(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(':') {
            Ok(Self {
                name: a.to_string(),
                expr: Expr::new(b)?,
            })
        } else {
            Ok(Self {
                name: String::new(),
                expr: Expr::new(spec)?,
            })
        }
    }
}

impl ColumnFun for ColumnExpr {
    fn add_names(&self, w: &mut ColumnHeader, _head: &StringLine) -> Result<()> {
        w.push(&self.name)
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        write!(w, "{}", self.expr.eval(line))?;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.expr.lookup(fieldnames)
    }
}

/// write a single column
#[derive(Debug, Default)]
pub struct ColumnSingle {
    col: NamedCol,
    new_name: String,
}

impl ColumnSingle {
    /// new from NamedCol
    pub fn with_named_col(col: &NamedCol) -> Self {
        Self {
            col: col.clone(),
            new_name: String::new(),
        }
    }
    /// new from name
    pub fn with_name(col: &str) -> Result<Self> {
        Ok(Self {
            col: NamedCol::new_from(col)?,
            new_name: String::new(),
        })
    }
}

impl ColumnFun for ColumnSingle {
    fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()> {
        if self.new_name.is_empty() {
            w.push(&head[self.col.num])
        } else {
            w.push(&self.new_name)
        }
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        w.write_all(&line[self.col.num])?;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}

/// write the whole line
#[derive(Debug, Default, Copy, Clone)]
pub struct ColumnWhole;

impl ColumnFun for ColumnWhole {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()> {
        w.push_all(head)
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        w.write_all(&line.line[0..line.line.len() - 1])?;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
}

/// write an increasing count, e.g. cat -n
#[derive(Debug, Default, Clone)]
pub struct ColumnCount {
    num: i64,
    name: String,
}

impl ColumnCount {
    /// new
    pub fn new(num: i64, name: &str) -> Self {
        Self {
            num,
            name: name.to_string(),
        }
    }
}

impl ColumnFun for ColumnCount {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, _head: &StringLine) -> Result<()> {
        w.push(&self.name)
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, _line: &TextLine, _delim: u8) -> Result<()> {
        w.write_all(self.num.to_string().as_bytes())?;
        self.num += 1;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
}

/// write a fixed value
#[derive(Debug, Default, Clone)]
pub struct ColumnLiteral {
    value: Vec<u8>,
    name: String,
}

impl ColumnLiteral {
    /// new
    pub fn new(value: &[u8], name: &str) -> Self {
        Self {
            value: value.to_vec(),
            name: name.to_string(),
        }
    }
}

impl ColumnFun for ColumnLiteral {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, _head: &StringLine) -> Result<()> {
        w.push(&self.name)
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, _line: &TextLine, _delim: u8) -> Result<()> {
        w.write_all(&self.value)?;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
}

impl fmt::Debug for dyn ColumnFun {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ColumnFun")
    }
}

impl ColumnSet {
    /// Create an empty column set, which selects all columns.
    pub fn new() -> Self {
        Self::default()
    }
    /// is empty?
    pub fn is_empty(&self) -> bool {
        self.pos.is_empty() && self.neg.is_empty()
    }

    /// Create an column set from a spec, e.g. "1-3"
    pub fn from_spec(spec: &str) -> Result<Self> {
        let mut s = Self::default();
        s.add_yes(spec)?;
        Ok(s)
    }

    /// Add some columns to be selected
    pub fn add_yes(&mut self, spec: &str) -> Result<()> {
        self.add(spec, false)
    }

    /// Add some columns to be rejected.
    pub fn add_no(&mut self, spec: &str) -> Result<()> {
        self.add(spec, true)
    }

    /// Add a single range, "yes" or "no" based on a flag.
    pub fn add_one(&mut self, s: &str, negate: bool, trans: Option<usize>) {
        if let Some(stripped) = s.strip_prefix('~') {
            let st = ColSetPart::new(stripped, trans);
            if negate {
                self.pos.push(st);
            } else {
                self.neg.push(st);
            }
        } else {
            let st = ColSetPart::new(s, trans);
            if negate {
                self.neg.push(st);
            } else {
                self.pos.push(st);
            }
        }
    }

    fn find_trans(spec: &str) -> Option<(&str, &str)> {
        let mut last_was_plus = false;
        for (i, x) in spec.char_indices() {
            if last_was_plus {
                if x.is_alphabetic() {
                    return Some((&spec[0..i - 1], &spec[i..]));
                } else {
                    last_was_plus = false;
                }
            } else if x == '+' {
                last_was_plus = true;
            }
        }
        None
    }
    /// Add a list or ranges, "yes" or "no" based on a flag.
    pub fn add(&mut self, spec: &str, negate: bool) -> Result<()> {
        let mut spc = spec;
        let trans = if let Some((a, b)) = Self::find_trans(spec) {
            spc = a;
            self.trans.push(TransList::new(b)?);
            Some(self.trans.len() - 1)
        } else {
            None
        };
        loop {
            if spc.first() == '(' {
                let pos = find_close(spc)?;
                self.add_one(&spc[..pos], negate, trans);
                spc = &spc[pos..];
                if spc.is_empty() {
                    break;
                }
            } else if let Some((a, b)) = spc.split_once(',') {
                self.add_one(a, negate, trans);
                spc = b;
            } else {
                self.add_one(spc, negate, trans);
                break;
            }
        }
        Ok(())
    }

    /// Turn column name into column number
    /// To also handle numbers and ranges, use `lookup1`
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    assert_eq!(ColumnSet::lookup_col(&["zero", "one", "two"], "one").unwrap(), 1);
    /// ```
    pub fn lookup_col(fieldnames: &[&str], colname: &str) -> Result<usize> {
        for f in fieldnames.iter().enumerate() {
            if f.1 == &colname {
                return Ok(f.0);
            }
        }
        err!("{} not found", colname)
    }
    /*
        /// turn a u8 column name into a column number
        pub fn lookup_col2(fieldnames: &[&str], colname: &[u8]) -> Result<usize> {
        let name = str::from_utf8_lossy(colname);
            for f in fieldnames.iter().enumerate() {
                if *f.1 == name {
                    return Ok(f.0);
                }
            }
            err!("{:?} not found", &colname)
        }
    */
    /// resolve a single name or number into a column number
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    assert_eq!(ColumnSet::single(&["zero", "one", "two"], "+1").unwrap(), 2);
    /// ```
    pub fn single(fieldnames: &[&str], colname: &str) -> Result<usize> {
        if let Some(stripped) = colname.strip_prefix('+') {
            let n = stripped.to_usize_whole()?;
            let len = fieldnames.len();
            if n > len {
                err!("Column {} out of bounds", colname)
            } else {
                Ok(len - n)
            }
        } else {
            let ch = colname.first();
            if ch.is_ascii_digit() && ch != '0' {
                let n = colname.to_usize_whole()?;
                if n < 1 {
                    err!("Column {} out of bounds", colname)
                } else {
                    Ok(n - 1)
                }
            } else {
                Self::lookup_col(fieldnames, colname)
            }
        }
    }

    /// Return all the fields that match the Matcher
    fn match_range(fieldnames: &[&str], rng: &str) -> Result<Vec<usize>> {
        let mut ret = Vec::new();
        let c = MatchMaker::make(rng)?;
        for f in fieldnames.iter().enumerate() {
            if c.smatch(f.1) {
                // allow case insensitive?
                ret.push(f.0);
            }
        }
        Ok(ret)
    }
    fn to_outcols(data: &[usize], name: &str) -> Vec<OutCol> {
        let mut ret = Vec::with_capacity(data.len());
        for x in data {
            ret.push(OutCol::new(*x, name));
        }
        ret
    }

    /// turn comma delimited list of ranges into list of possibly named column numbers
    pub fn ranges(fieldnames: &[&str], rng: &str) -> Result<Vec<OutCol>> {
        let mut c = Self::new();
        c.add_yes(rng)?;
        c.lookup(fieldnames)?;
        Ok(c.get_cols_full())
    }
    /// turn range into list of column numbers
    fn range(fieldnames: &[&str], rng: &str) -> Result<Vec<OutCol>> {
        if rng.is_empty() {
            return err!("Empty Range {}", rng);
        }
        let (name, range) = match rng.split_once(':') {
            None => ("", rng),
            Some(x) => x,
        };

        let ch = range.first();
        if ch == '(' {
            return Ok(Self::to_outcols(
                &Self::match_range(fieldnames, &range[1..range.len() - 1])?,
                name,
            ));
        }
        let mut parts: Vec<&str> = range.split('-').collect();
        if parts.len() > 2 {
            return err!("Malformed Range {}", rng);
        }
        if parts[0].is_empty() {
            parts[0] = "1";
        }

        let start: usize;
        let end = if parts.len() == 1 {
            start = Self::single(fieldnames, parts[0])?;
            start
        } else {
            if parts[1].is_empty() {
                parts[1] = "+1";
            }
            start = Self::single(fieldnames, parts[0])?;
            Self::single(fieldnames, parts[1])?
        };

        if start > end {
            // throw new CdxException("start > end, i.e. {" + parts.get(0) + " > " + parts.get(1));
            return err!("Start greater than end : {}", rng);
        }
        let mut res = Vec::new();
        for i in start..=end {
            res.push(OutCol::new(i, name));
        }
        Ok(res)
    }

    /// resolve `ColumnSet` in context of the column names from an input file
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
    ///    let mut s = ColumnSet::new();
    ///    s.add_no("two");
    ///    s.lookup(&header);
    ///    assert_eq!(s.get_cols_num(), &[0,1,3,4]);
    /// ```
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.did_lookup = true;
        self.columns = Vec::new();
        let mut no_cols: HashSet<usize> = HashSet::new();

        for s in &self.neg {
            for x in Self::range(fieldnames, &s.val)? {
                no_cols.insert(x.num);
            }
        }

        if self.pos.is_empty() {
            for x in 0..fieldnames.len() {
                if !no_cols.contains(&x) {
                    self.columns.push(OutCol::from_num(x));
                }
            }
        } else {
            for s in &self.pos {
                for mut x in Self::range(fieldnames, &s.val)? {
                    if !no_cols.contains(&x.num) {
                        x.trans = s.trans;
                        self.columns.push(x);
                    }
                }
            }
        }
        Ok(())
    }

    /// Select columns from input line based on `ColumnSet`. Fail if input is too short. """
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
    ///    let mut s = ColumnSet::new();
    ///    s.add_no("two");
    ///    s.lookup(&header);
    ///    let v = ["Zeroth", "First", "Second", "Third", "Fourth", "Fifth"];
    ///    let mut res = Vec::new();
    ///    s.select(&v, &mut res);
    ///    assert_eq!(res, vec!["Zeroth", "First", "Third", "Fourth"]);
    /// ```
    pub fn select<T: AsRef<str> + Clone>(&self, cols: &[T], result: &mut Vec<T>) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        result.clear();
        for x in &self.columns {
            if x.num < cols.len() {
                result.push(cols[x.num].clone());
            } else {
                return err!(
                    "Line has only {} columns, but column {} was requested.",
                    cols.len(),
                    x.num + 1
                );
            }
        }
        Ok(())
    }

    /// write the appropriate selection from the given columns
    /// trying to write a non-existant column is an error
    pub fn write(&self, w: &mut dyn Write, cols: &[&str], delim: &str) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        let mut iter = self.columns.iter();
        match iter.next() {
            None => {}
            Some(first) => {
                w.write_all(cols[first.num].as_bytes())?;

                for x in iter {
                    w.write_all(delim.as_bytes())?;
                    if x.num < cols.len() {
                        w.write_all(cols[x.num].as_bytes())?;
                    } else {
                        return err!(
                            "Line has only {} columns, but column {} was requested.",
                            cols.len(),
                            x.num + 1
                        );
                    }
                }
            }
        };
        w.write_all(&[b'\n'])?;
        Ok(())
    }

    /// write the appropriate selection from the given columns
    pub fn write2(&self, w: &mut dyn Write, cols: &TextLine, delim: u8) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        let mut iter = self.columns.iter();
        match iter.next() {
            None => {}
            Some(first) => {
                w.write_all(&cols[first.num])?;
                for x in iter {
                    w.write_all(&[delim])?;
                    w.write_all(cols.get(x.num))?;
                }
            }
        };
        w.write_all(&[b'\n'])?;
        Ok(())
    }

    fn fetch<'a>(&'a mut self, cols: &'a TextLine, col: usize) -> &'a [u8] {
        let col = &self.columns[col];
        if let Some(trans) = col.trans {
            self.trans[trans].trans(cols.get(col.num), cols).unwrap()
        } else {
            cols.get(col.num)
        }
    }

    /// write the appropriate selection from the given columns, but no trailing newline
    pub fn write3(&mut self, w: &mut dyn Write, cols: &TextLine, delim: u8) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        if self.columns.is_empty() {
            return Ok(());
        }
        w.write_all(self.fetch(cols, 0))?;
        for i in 1..self.columns.len() {
            w.write_all(&[delim])?;
            w.write_all(self.fetch(cols, i))?;
        }
        Ok(())
    }

    /// write the appropriate selection from the given columns, but no trailing newline
    pub fn write3s(&self, w: &mut dyn Write, cols: &StringLine, delim: u8) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        let mut iter = self.columns.iter();
        match iter.next() {
            None => {}
            Some(first) => {
                w.write_all(cols.get(first.num).as_bytes())?;
                for x in iter {
                    w.write_all(&[delim])?;
                    w.write_all(cols.get(x.num).as_bytes())?;
                }
            }
        };
        Ok(())
    }

    /// write the appropriate selection from the given columns, but no trailing newline
    /// trying to write a non-existant column writes the provided default value
    pub fn write_sloppy(&self, cols: &[&str], rest: &str, w: &mut dyn Write) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        for x in &self.columns {
            if x.num < cols.len() {
                w.write_all(cols[x.num].as_bytes())?;
            } else {
                w.write_all(rest.as_bytes())?;
            }
        }
        Ok(())
    }

    /// Select columns from input line based on `ColumnSet`, fill in default if line is too short """
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
    ///    let mut s = ColumnSet::new();
    ///    s.add_no("two");
    ///    s.lookup(&header);
    ///    let v = ["Zeroth", "First", "Second", "Third"];
    ///    let mut res = Vec::new();
    ///    s.select_sloppy(&v, &"extra", &mut res);
    ///    assert_eq!(res, vec!["Zeroth", "First", "Third", "extra"]);
    /// ```
    pub fn select_sloppy<T: AsRef<str> + Clone>(
        &self,
        cols: &[T],
        restval: &T,
        result: &mut Vec<T>,
    ) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        result.clear();
        for x in &self.columns {
            if x.num < cols.len() {
                result.push(cols[x.num].clone());
            } else {
                result.push(restval.clone());
            }
        }
        Ok(())
    }

    /// return owned columns by const reference
    pub const fn get_cols(&self) -> &Vec<OutCol> {
        &self.columns
    }

    /// steal owned columns by value
    #[allow(clippy::missing_const_for_fn)] // clippy bug
    pub fn get_cols_full(self) -> Vec<OutCol> {
        self.columns
    }

    /// get owned column numbers
    pub fn get_cols_num(&self) -> Vec<usize> {
        let mut v = Vec::with_capacity(self.columns.len());
        for x in &self.columns {
            v.push(x.num);
        }
        v
    }

    /// Shorthand to look up some columns
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
    ///    assert_eq!(ColumnSet::lookup_cols("~2-3", &header).unwrap(), &[0,3,4]);
    /// ```
    pub fn lookup_cols(spec: &str, names: &[&str]) -> Result<Vec<usize>> {
        let mut s = Self::new();
        s.add_yes(spec)?;
        s.lookup(names)?;
        Ok(s.get_cols_num())
    }

    /// Shorthand to look up some columns, with names
    pub fn lookup_cols_full(spec: &str, names: &[&str]) -> Result<Vec<OutCol>> {
        let mut s = Self::new();
        s.add_yes(spec)?;
        s.lookup(names)?;
        Ok(s.get_cols_full())
    }

    /// Shorthand to look up a single column
    /// To look up a single named column, `lookup_col` is also available
    /// # Examples
    ///
    /// ```
    ///    use cdx::column::ColumnSet;
    ///    let header: [&str; 5] = ["zero", "one", "two", "three", "four"];
    ///    assert_eq!(ColumnSet::lookup1("three", &header).unwrap(), 3);
    /// ```
    pub fn lookup1(spec: &str, names: &[&str]) -> Result<usize> {
        let mut s = Self::new();
        s.add_yes(spec)?;
        s.lookup(names)?;
        if s.get_cols().len() != 1 {
            return err!(
                "Spec {} resolves to {} columns, rather than a single column",
                spec,
                s.get_cols().len()
            );
        }
        Ok(s.get_cols_num()[0])
    }
}

/// Write the columns with a custom delimiter
pub struct ColumnClump {
    cols: Box<dyn ColumnFun>,
    name: String,
    delim: u8,
}
impl fmt::Debug for ColumnClump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ColumnClump")
    }
}

impl ColumnClump {
    /// new ColumnClump from parts
    pub fn new(cols: Box<dyn ColumnFun>, name: &str, delim: u8) -> Self {
        Self {
            cols,
            name: name.to_string(),
            delim,
        }
    }
    /// new ColumnClump from spec : DelimOutcol:Columns
    /// e.g. ,group:1-3
    pub fn from_spec(orig_spec: &str) -> Result<Self> {
        let mut spec = orig_spec;
        if spec.is_empty() {
            return err!("Can't construct a group from an empty string");
        }
        let delim = spec.take_first();
        if delim.len_utf8() != 1 {
            return err!("Delimiter must be a plain ascii character : {}", orig_spec);
        }
        let parts = spec.split_once(':');
        if parts.is_none() {
            return err!(
                "No colon found. Group format is DelimOutname:Columns : {}",
                orig_spec
            );
        }
        let parts = parts.unwrap();
        let mut g = ColumnSet::new();
        g.add_yes(parts.1)?;
        let cols = Box::new(ReaderColumns::new(g));
        Ok(Self {
            cols,
            name: parts.0.to_string(),
            delim: delim as u8,
        })
    }
}

impl ColumnFun for ColumnClump {
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        self.cols.write(w, line, self.delim)?;
        Ok(())
    }

    fn add_names(&self, w: &mut ColumnHeader, _head: &StringLine) -> Result<()> {
        w.push(&self.name)
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.cols.lookup(fieldnames)
    }
}

/// Selection of columns from a Reader
#[derive(Debug)]
pub struct ReaderColumns {
    columns: ColumnSet,
}

impl ReaderColumns {
    /// new ReaderColumns
    pub const fn new(columns: ColumnSet) -> Self {
        Self { columns }
    }
}

/// Write column name : col.name if non empty, else head[col.num]
pub fn write_colname(w: &mut dyn Write, col: &OutCol, head: &StringLine) -> Result<()> {
    if col.name.is_empty() {
        w.write_all(head.get(col.num).as_bytes())?;
    } else {
        w.write_all(col.name.as_bytes())?;
    }
    Ok(())
}

impl ColumnFun for ReaderColumns {
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, delim: u8) -> Result<()> {
        self.columns.write3(w, line, delim)?;
        Ok(())
    }

    fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()> {
        for x in self.columns.get_cols() {
            if x.name.is_empty() {
                w.push(head.get(x.num))?;
            } else {
                w.push(&x.name)?;
            }
        }
        Ok(())
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.columns.lookup(fieldnames)
    }
}

/// A collection of ColumnFun writers
pub struct Writer {
    v: Vec<Box<dyn ColumnFun>>,
    delim: u8,
}
impl fmt::Debug for Writer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Writer")
    }
}

impl Writer {
    /// new Writer
    pub fn new(delim: u8) -> Self {
        Self {
            v: Vec::new(),
            delim,
        }
    }
    /// is it empty
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    /// resolve column names
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in self.v.iter_mut() {
            x.lookup(fieldnames)?
        }
        Ok(())
    }
    /// Add a new writer
    pub fn push(&mut self, x: Box<dyn ColumnFun>) {
        self.v.push(x);
    }
    /// Write the column names
    pub fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()> {
        for x in self.v.iter() {
            x.add_names(w, head)?;
        }
        Ok(())
    }
    /// Write the column values
    pub fn write(&mut self, w: &mut dyn Write, line: &TextLine) -> Result<()> {
        let mut iter = self.v.iter_mut();
        match iter.next() {
            None => {}
            Some(first) => {
                first.write(w, line, self.delim)?;
                for x in iter {
                    w.write_all(&[self.delim])?;
                    x.write(w, line, self.delim)?;
                }
            }
        };
        w.write_all(&[b'\n'])?;
        Ok(())
    }
}

impl Default for Writer {
    fn default() -> Self {
        Self::new(b'\t')
    }
}

/// a column, by name or number
#[derive(Debug, Clone, Default)]
pub struct NamedCol {
    /// the column name. Empty if using column by number
    pub name: String,
    /// the columnn number, either as originally set or after lookup
    pub num: usize,
    /// if non-zero, input was "+2" so num should be calculated as (num_cols - from_end)
    pub from_end: usize,
}

impl fmt::Display for NamedCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Column {}", self.num + 1)?;
        if !self.name.is_empty() {
            write!(f, " ({})", self.name)
        } else if self.from_end != 0 {
            write!(f, " (+{})", self.from_end)
        } else {
            Ok(())
        }
    }
}

impl NamedCol {
    /// new named column
    pub fn new() -> Self {
        Self::default()
    }
    /// new column from spec
    pub fn new_from(spec: &str) -> Result<Self> {
        let mut x = Self::default();
        let rest = x.parse(spec)?;
        if rest.is_empty() {
            Ok(x)
        } else {
            err!("Extra stuff {} at the end of column spec {}", rest, spec)
        }
    }
    /// Resolve the column name
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        if !self.name.is_empty() {
            self.num = ColumnSet::lookup_col(fieldnames, &self.name)?
        } else if self.from_end > 0 {
            if self.from_end > fieldnames.len() {
                return err!(
                    "Requested column +{}, but there are only {} columns",
                    self.from_end,
                    fieldnames.len()
                );
            }
            self.num = fieldnames.len() - self.from_end
        }
        Ok(())
    }
    /// Extract a column name or number, return the unused part of the slice
    pub fn parse<'a>(&mut self, orig_spec: &'a str) -> Result<&'a str> {
        let mut spec = orig_spec;
        self.num = 0;
        self.name.clear();
        if spec.is_empty() {
            return err!("Empty string found where column name expected");
        }
        let mut ch = spec.first();
        let was_plus = ch == '+';
        if was_plus {
            ch = spec.take_first();
        }
        if ch.is_ascii_digit() && ch != '0' {
            let (a, b) = spec.to_usize();
            self.num = a;
            spec = b;
            if was_plus {
                self.from_end = self.num;
            } else {
                self.num -= 1;
            }
            Ok(spec)
        } else if ch.is_alphabetic() {
            if was_plus {
                return err!("'+' must be follwed by a number : {}", orig_spec);
            }
            let pos = get_col_name(spec);
            self.name.clear();
            self.name += &spec[0..pos];
            Ok(&spec[pos..])
        } else {
            err!("Bad parse of compare spec for {}", spec)
        }
    }
}

/// return number of bytes used by the column name
/// zero means no name present, return < str.len() if there's extra stuff
pub fn get_col_name(spec: &str) -> usize {
    if spec.is_empty() {
        return 0;
    }
    let mut sp = spec;
    let mut ch = sp.take_first();
    if !ch.is_alphabetic() {
        return 0;
    }
    while !sp.is_empty() {
        ch = sp.first();
        if ch.is_alphanumeric() || ch == '_' {
            sp = sp.skip_first();
        } else {
            break;
        }
    }
    spec.len() - sp.len()
}

#[derive(Debug, Clone, Default)]
struct CompositeColumnPart {
    prefix: String,
    col: NamedCol,
}

impl CompositeColumnPart {
    fn new() -> Self {
        Self::default()
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}

/// Create a string from a bunch of static stings and column values
#[derive(Debug, Clone, Default)]
pub struct CompositeColumn {
    parts: Vec<CompositeColumnPart>,
    suffix: String,
    name: String,
}

impl CompositeColumn {
    /// new
    pub fn new(s: &str) -> Result<Self> {
        let mut c = Self::default();
        c.set(s)?;
        Ok(c)
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in &mut self.parts {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
    /// contruct string from column values
    pub fn get(&self, t: &mut Vec<u8>, fields: &[&[u8]]) {
        t.clear();
        for x in &self.parts {
            t.extend(x.prefix.as_bytes());
            t.extend(fields[x.col.num]);
        }
        t.extend(self.suffix.as_bytes());
    }
    /// configure from spec
    pub fn set(&mut self, spec: &str) -> Result<()> {
        let name = spec.split_once(':');
        if name.is_none() {
            return err!("Composite Column Spec must start with ColName: : {}", spec);
        }
        let name = name.unwrap();
        self.name = name.0.to_string();
        let mut curr = name.1;
        self.suffix.clear();
        self.parts.clear();
        const TAG: char = '^';
        while !curr.is_empty() {
            let ch = curr.take_first();
            if ch == TAG {
                if curr.is_empty() {
                    self.suffix.push(TAG);
                } else if curr.first() == TAG {
                    self.suffix.push(TAG);
                    curr = curr.skip_first();
                } else {
                    let mut p = CompositeColumnPart::new();
                    std::mem::swap(&mut p.prefix, &mut self.suffix);
                    if curr.first() == '{' {
                        curr = curr.skip_first();
                        curr = p.col.parse(curr)?;
                        if curr.is_empty() || curr.first() != '}' {
                            return err!("Closing bracket not found : {}", spec);
                        }
                        curr = curr.skip_first();
                    } else {
                        curr = p.col.parse(curr)?;
                    }

                    self.parts.push(p);
                }
            } else {
                self.suffix.push(ch);
            }
        }
        Ok(())
    }
}

impl ColumnFun for CompositeColumn {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, _head: &StringLine) -> Result<()> {
        w.push(&self.name)
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        for x in &self.parts {
            w.write_all(x.prefix.as_bytes())?;
            w.write_all(line.get(x.col.num))?;
        }
        w.write_all(self.suffix.as_bytes())?;
        Ok(())
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        Self::lookup(self, fieldnames)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_err {
	($expression:expr, $($pattern:tt)+) => {
            match $expression {
		$($pattern)+ => (),
		ref e => panic!("expected `{}` but got `{:?}`", stringify!($($pattern)+), e),
            }
	}
    }

    #[test]
    fn range() -> Result<()> {
        let f: [&str; 5] = ["zero", "one", "two", "three", "four"];
        let res: [OutCol; 5] = [
            OutCol::from_num(0),
            OutCol::from_num(1),
            OutCol::from_num(2),
            OutCol::from_num(3),
            OutCol::from_num(4),
        ];
        assert_eq!(
            ColumnSet::range(&f, "(range,<p)")?,
            [OutCol::from_num(1), OutCol::from_num(4)]
        );
        assert_eq!(ColumnSet::range(&f, "2-+2")?, res[1..=3]);
        assert_eq!(ColumnSet::range(&f, "-")?, res);
        assert_eq!(ColumnSet::range(&f, "2-")?, res[1..]);
        assert_eq!(ColumnSet::range(&f, "-2")?, res[..2]);
        assert_err!(ColumnSet::range(&f, "1-2-3"), Err(Error::Error(_)));
        return Ok(());
    }

    #[test]
    fn named_range() -> Result<()> {
        let f: [&str; 5] = ["zero", "one", "two", "three", "four"];
        let res: [OutCol; 5] = [
            OutCol::new(0, "stuff"),
            OutCol::new(1, "junk"),
            OutCol::new(2, "this"),
            OutCol::new(3, "that"),
            OutCol::new(4, "other"),
        ];
        assert_eq!(ColumnSet::range(&f, "stuff:1")?, res[0..1]);
        assert_eq!(ColumnSet::ranges(&f, "stuff:1,junk:2")?, res[0..2]);
        return Ok(());
    }

    #[test]
    fn do_get_col_name() {
        assert_eq!(get_col_name(""), 0);
        assert_eq!(get_col_name("..."), 0);
        assert_eq!(get_col_name("_aaa"), 0);
        assert_eq!(get_col_name("1aaa"), 0);
        assert_eq!(get_col_name("abc"), 3);
        assert_eq!(get_col_name("abc,"), 3);
        assert_eq!(get_col_name("abc,aaa"), 3);
        assert_eq!(get_col_name("a_b_c"), 5);
        assert_eq!(get_col_name("a_ñ_c"), 6);
    }
}
