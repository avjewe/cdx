//! Handles conversion between named column sets and lists of column numbers
//! Also helps with selecting those columns from a line of text

use crate::{err, Error, Result, TextLine};
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::io::Write;
use std::{fmt, str};

/// A column set is a collection of column specifictions, which when interpreted
/// in the context of a set of named columns, produces a list of column numbers
/// It contains a bunch of "yes" specs and "no" specs.
/// If there are no "yes" specs, all columns are marked "yes"
/// The resulting list of column numbers is all of the "yes" columns in order,
/// minus any "no" columns. Thus if a column appears in both, it will be excluded.
/// # Examples
///
/// ```
///    use cdx::ColumnSet;
///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
///
///    let mut s = ColumnSet::new();
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[0,1,2,3,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes("-");
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[0,1,2,3,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes("one-three");
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[1,2,3]);
///
///    let mut s = ColumnSet::new();
///    s.add_no("three,one");
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[0,2,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_no("+4-+2");
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[0,4]);
///
///    let mut s = ColumnSet::new();
///    s.add_yes(">s<=two");
///    s.lookup(&header);
///    assert_eq!(s.get_cols(), &[2,3]);
/// ```
#[derive(Debug, Clone)]
pub struct ColumnSet {
    pos: Vec<String>,
    neg: Vec<String>,
    columns: Vec<usize>,
    did_lookup: bool,
}

/// Write some output
pub trait ColumnFun {
    /// write the column names (called once)
    fn write_names(&self, w: &mut dyn Write, head: &TextLine, delim: u8) -> Result<()>;
    /// write the column values (called many times)
    fn write(&self, w: &mut dyn Write, line: &TextLine, delim: u8) -> Result<()>;
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()>;
}

impl fmt::Debug for dyn ColumnFun {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ColumnFun")
    }
}

impl ColumnSet {
    /// Create an empty column set, which selects all columns.
    pub fn new() -> Self {
        Self {
            pos: Vec::new(),
            neg: Vec::new(),
            columns: Vec::new(),
            did_lookup: false,
        }
    }

    /// Add some columns to be selected
    pub fn add_yes(&mut self, spec: &str) {
        self.add(spec, false);
    }

    /// Add some columns to be rejected.
    pub fn add_no(&mut self, spec: &str) {
        self.add(spec, true);
    }

    /// Add a spec, "yes" or "no" based on a flag.
    pub fn add(&mut self, spec: &str, negate: bool) {
        for s in spec.split(',') {
            if let Some(stripped) = s.strip_prefix('~') {
                let st = stripped.to_string();
                if negate {
                    self.pos.push(st);
                } else {
                    self.neg.push(st);
                }
            } else {
                let st = s.to_string();
                if negate {
                    self.neg.push(st);
                } else {
                    self.pos.push(st);
                }
            }
        }
    }

    /// Turn column name into column number
    /// To also handle numbers and ranges, use `lookup1`
    /// # Examples
    ///
    /// ```
    ///    use cdx::ColumnSet;
    ///    assert_eq!(ColumnSet::lookup_col(&[b"zero", b"one", b"two"], "one").unwrap(), 1);
    /// ```
    pub fn lookup_col(fieldnames: &[&[u8]], colname: &str) -> Result<usize> {
        for f in fieldnames.iter().enumerate() {
            if str::from_utf8(f.1).unwrap() == colname {
                return Ok(f.0);
            }
        }
        err!("{} not found", colname)
    }
    /// turn a u8 column name into a column number
    pub fn lookup_col2(fieldnames: &[&[u8]], colname: &[u8]) -> Result<usize> {
        for f in fieldnames.iter().enumerate() {
            if *f.1 == colname {
                return Ok(f.0);
            }
        }
        err!("{:?} not found", &colname)
    }

    /// resolve a single name or number into a column number
    /// # Examples
    ///
    /// ```
    ///    use cdx::ColumnSet;
    ///    assert_eq!(ColumnSet::single(&[b"zero", b"one", b"two"], "+1").unwrap(), 2);
    /// ```
    pub fn single(fieldnames: &[&[u8]], colname: &str) -> Result<usize> {
        if let Some(stripped) = colname.strip_prefix('+') {
            match stripped.parse::<usize>() {
                Ok(n) => {
                    let len = fieldnames.len();
                    if n > len {
                        err!("Column {} out of bounds", colname)
                    } else {
                        Ok(len - n)
                    }
                }
                Err(e) => Err(Error::ParseIntError(e)),
            }
        } else {
            let ch = colname.chars().next().unwrap();
            if ch.is_digit(10) {
                match colname.parse::<usize>() {
                    Ok(n) => {
                        if n < 1 {
                            err!("Column {} out of bounds", colname)
                        } else {
                            Ok(n - 1)
                        }
                    }
                    Err(e) => Err(Error::ParseIntError(e)),
                }
            } else {
                Self::lookup_col(fieldnames, colname)
            }
        }
    }

    /// determine if two strings match, given an operator
    fn do_compare(left: &str, right: &str, operator: &str) -> Result<bool> {
        let val = left.cmp(right);
        if operator == "<" {
            return Ok(val == Ordering::Less);
        }
        if operator == "<=" {
            return Ok(val != Ordering::Greater);
        }
        if operator == ">" {
            return Ok(val == Ordering::Greater);
        }
        if operator == ">=" {
            return Ok(val != Ordering::Less);
        }
        err!(operator.to_string())
    }

    /// determine if a name is within a range
    fn do_compare2(name: &str, key1: &str, op1: &str, key2: &str, op2: &str) -> Result<bool> {
        Ok(Self::do_compare(name, key1, op1)? && Self::do_compare(name, key2, op2)?)
    }

    /// Return all the field numbers that match a textual spec like <foo or <foo>bar """
    fn text_range(fieldnames: &[&[u8]], rng: &str) -> Result<Vec<usize>> {
        let mut ret = Vec::new();
        lazy_static! {
            static ref RE1: Regex = Regex::new("^([<>]=?)([^<>]+)$").unwrap();
            static ref RE2: Regex = Regex::new("^([<>]=?)([^<>]+)([<>]=?)([^<>]+)$").unwrap();
        }

        if let Some(caps) = RE1.captures(rng) {
            for f in fieldnames.iter().enumerate() {
                if Self::do_compare(
                    str::from_utf8(f.1).unwrap(),
                    caps.get(2).unwrap().as_str(),
                    caps.get(1).unwrap().as_str(),
                )? {
                    ret.push(f.0);
                }
            }
            return Ok(ret);
        }

        if let Some(caps) = RE2.captures(rng) {
            for f in fieldnames.iter().enumerate() {
                if Self::do_compare2(
                    str::from_utf8(f.1).unwrap(),
                    caps.get(2).unwrap().as_str(),
                    caps.get(1).unwrap().as_str(),
                    caps.get(4).unwrap().as_str(),
                    caps.get(3).unwrap().as_str(),
                )? {
                    ret.push(f.0);
                }
            }
            return Ok(ret);
        }

        err!("Malformed Range {}", rng)
    }

    /// turn range into list of column numbers
    fn range(fieldnames: &[&[u8]], rng: &str) -> Result<Vec<usize>> {
        if rng.is_empty() {
            return err!("Empty Range {}", rng);
        }
        let ch = rng.chars().next().unwrap();
        if ch == '<' || ch == '>' || ch == '=' {
            return Self::text_range(fieldnames, rng);
        }
        let mut parts: Vec<&str> = rng.split('-').collect();
        if parts.len() > 2 {
            return err!("Malformed Range {}", rng);
        }
        if parts[0].is_empty() {
            parts[0] = "1";
        }

        let start: usize;
        let end: usize;
        if parts.len() == 1 {
            start = Self::single(fieldnames, parts[0])?;
            end = start;
        } else {
            if parts[1].is_empty() {
                parts[1] = "+1";
            }
            start = Self::single(fieldnames, parts[0])?;
            end = Self::single(fieldnames, parts[1])?;
        }

        if start > end {
            // throw new CdxException("start > end, i.e. {" + parts.get(0) + " > " + parts.get(1));
            return err!("Start greater than end : {}", rng);
        }
        let mut res = Vec::new();
        for i in start..=end {
            res.push(i);
        }
        Ok(res)
    }

    /// resolve `ColumnSet` in context of the column names from an input file
    /// # Examples
    ///
    /// ```
    ///    use cdx::ColumnSet;
    ///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
    ///    let mut s = ColumnSet::new();
    ///    s.add_no("two");
    ///    s.lookup(&header);
    ///    assert_eq!(s.get_cols(), &[0,1,3,4]);
    /// ```
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.did_lookup = true;
        self.columns = Vec::new();
        let mut no_cols: HashSet<usize> = HashSet::new();

        if !self.neg.is_empty() {
            for s in &self.neg {
                for x in Self::range(fieldnames, s)? {
                    no_cols.insert(x);
                }
            }
        }

        if self.pos.is_empty() {
            for x in 0..fieldnames.len() {
                if !no_cols.contains(&x) {
                    self.columns.push(x);
                }
            }
        } else {
            for s in &self.pos {
                for x in Self::range(fieldnames, s)? {
                    if !no_cols.contains(&x) {
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
    ///    use cdx::ColumnSet;
    ///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
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
            if *x < cols.len() {
                result.push(cols[*x].clone());
            } else {
                return err!(
                    "Line has only {} columns, but column {} was requested.",
                    cols.len(),
                    *x + 1
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
        let mut need_delim = false;
        for x in &self.columns {
            if *x < cols.len() {
                if need_delim {
                    w.write_all(delim.as_bytes())?;
                }
                w.write_all(cols[*x].as_bytes())?;
                need_delim = true;
            } else {
                return err!(
                    "Line has only {} columns, but column {} was requested.",
                    cols.len(),
                    *x + 1
                );
            }
        }
        w.write_all("\n".as_bytes())?;
        Ok(())
    }

    /// write the appropriate selection from the given columns
    pub fn write2(&self, w: &mut dyn Write, cols: &TextLine, delim: u8) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        let mut need_delim = false;
        for x in &self.columns {
            if need_delim {
                w.write_all(&[delim])?;
            }
            need_delim = true;
            w.write_all(cols.get(*x))?;
        }
        w.write_all(&[b'\n'])?;
        Ok(())
    }

    /// write the appropriate selection from the given columns, but no trailing newline
    pub fn write3(&self, w: &mut dyn Write, cols: &TextLine, delim: u8) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        let mut need_delim = false;
        for x in &self.columns {
            if need_delim {
                w.write_all(&[delim])?;
            }
            need_delim = true;
            w.write_all(cols.get(*x))?;
        }
        Ok(())
    }

    /// write the appropriate selection from the given columns, but no trailing newline
    /// trying to write a non-existant column writes the provided default value
    pub fn write_sloppy(&self, cols: &[&str], rest: &str, w: &mut dyn Write) -> Result<()> {
        if !self.did_lookup {
            return Err(Error::NeedLookup);
        }
        for x in &self.columns {
            if *x < cols.len() {
                w.write_all(cols[*x].as_bytes())?;
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
    ///    use cdx::ColumnSet;
    ///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
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
            if *x < cols.len() {
                result.push(cols[*x].clone());
            } else {
                result.push(restval.clone());
            }
        }
        Ok(())
    }

    /// return owned columns by const reference
    pub fn get_cols(&self) -> &Vec<usize> {
        &self.columns
    }

    /// steal owned columns by value
    pub fn get_cols_full(self) -> Vec<usize> {
        self.columns
    }

    /// Shorthand to look up some columns
    /// # Examples
    ///
    /// ```
    ///    use cdx::ColumnSet;
    ///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
    ///    assert_eq!(ColumnSet::lookup_cols("~2-3", &header).unwrap(), &[0,3,4]);
    /// ```
    pub fn lookup_cols(spec: &str, names: &[&[u8]]) -> Result<Vec<usize>> {
        let mut s = ColumnSet::new();
        s.add_yes(spec);
        s.lookup(names)?;
        Ok(s.get_cols_full())
    }

    /// Shorthand to look up a single column
    /// To look up a single named column, `lookup_col` is also available
    /// # Examples
    ///
    /// ```
    ///    use cdx::ColumnSet;
    ///    let header: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
    ///    assert_eq!(ColumnSet::lookup1("three", &header).unwrap(), 3);
    /// ```
    pub fn lookup1(spec: &str, names: &[&[u8]]) -> Result<usize> {
        let mut s = ColumnSet::new();
        s.add_yes(spec);
        s.lookup(names)?;
        if s.get_cols().len() != 1 {
            return err!(
                "Spec {} resolves to {} columns, rather than a single column",
                spec,
                s.get_cols().len()
            );
        }
        Ok(s.get_cols()[0])
    }
}

impl Default for ColumnSet {
    fn default() -> Self {
        Self::new()
    }
}
/*
impl ColumnFun for ReaderFull<'_> {
    fn write(&self, w: &mut dyn Write, line: &TextLine, delim: u8) -> Result<()> {
        let mut need_delim = false;
        for x in line.iter() {
            if need_delim {
                w.write_all(&[delim])?;
            }
            need_delim = true;
            w.write_all(x)?;
        }
        Ok(())
    }

    fn write_names(&self, w: &mut dyn Write, head: &TextLine, delim: u8) -> Result<()> {
        let mut need_delim = false;
        for x in head.iter() {
            if need_delim {
                w.write_all(&[delim])?;
            }
            need_delim = true;
            w.write_all(x)?;
        }
        Ok(())
    }
}
 */

/// Write the columns with a custom delimiter
pub struct ColumnClump<'a> {
    cols: Box<dyn ColumnFun + 'a>,
    name: Vec<u8>,
    delim: u8,
}
impl fmt::Debug for ColumnClump<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ColumnClump")
    }
}

impl<'a> ColumnClump<'a> {
    /// new ColumnClump
    pub fn new(cols: Box<dyn ColumnFun + 'a>, name: &[u8], delim: u8) -> Self {
        Self {
            cols,
            name: name.to_vec(),
            delim,
        }
    }
}

impl<'a> ColumnFun for ColumnClump<'a> {
    fn write(&self, w: &mut dyn Write, line: &TextLine, _delim: u8) -> Result<()> {
        self.cols.write(w, line, self.delim)?;
        Ok(())
    }

    fn write_names(&self, w: &mut dyn Write, _head: &TextLine, _delim: u8) -> Result<()> {
        w.write_all(&self.name)?;
        Ok(())
    }
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
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
    pub fn new(columns: ColumnSet) -> Self {
        Self { columns }
    }
}

impl ColumnFun for ReaderColumns {
    fn write(&self, w: &mut dyn Write, line: &TextLine, delim: u8) -> Result<()> {
        self.columns.write3(w, line, delim)?;
        Ok(())
    }

    fn write_names(&self, w: &mut dyn Write, head: &TextLine, delim: u8) -> Result<()> {
        self.columns.write3(w, head, delim)?;
        Ok(())
    }
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.columns.lookup(fieldnames)
    }
}

/// A collection of ColumnFun writers
pub struct Writer<'a> {
    v: Vec<Box<dyn ColumnFun + 'a>>,
    delim: u8,
}
impl fmt::Debug for Writer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Writer")
    }
}

impl<'a> Writer<'a> {
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
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        for x in self.v.iter_mut() {
            x.lookup(fieldnames)?
        }
        Ok(())
    }
    /// Add a new writer
    pub fn push(&mut self, x: Box<dyn ColumnFun + 'a>) {
        self.v.push(x);
    }
    /// Write the column names
    pub fn write_names(&mut self, w: &mut dyn Write, head: &TextLine) -> Result<()> {
        w.write_all(b" CDX")?;
        w.write_all(&[self.delim])?;
        let mut need_delim = false;
        for x in self.v.iter_mut() {
            if need_delim {
                w.write_all(&[self.delim])?;
            }
            need_delim = true;
            x.write_names(w, head, self.delim)?;
        }
        w.write_all(&[b'\n'])?;
        Ok(())
    }
    /// Write the column values
    pub fn write(&mut self, w: &mut dyn Write, line: &TextLine) -> Result<()> {
        let mut need_delim = false;
        for x in self.v.iter_mut() {
            if need_delim {
                w.write_all(&[self.delim])?;
            }
            need_delim = true;
            x.write(w, line, self.delim)?;
        }
        w.write_all(&[b'\n'])?;
        Ok(())
    }
}

impl<'a> Default for Writer<'a> {
    fn default() -> Self {
        Self::new(b'\t')
    }
}

/// a column, by name or number
#[derive(Debug, Clone)]
pub struct NamedCol {
    /// the column name. Empty if using column by number
    pub name: Vec<u8>,
    /// the columnn number, either as originally set or after lookup
    pub num: usize,
}

impl NamedCol {
    /// new named column
    pub fn new() -> Self {
        Self {
            name: Vec::new(),
            num: 0,
        }
    }
    /// Resolve the column name
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        if !self.name.is_empty() {
            self.num = ColumnSet::lookup_col2(fieldnames, &self.name)?
        }
        Ok(())
    }
    /// Extract a column name or number, return the unuse part of the slice
    pub fn parse<'a>(&mut self, spec: &'a str) -> Result<&'a str> {
        self.num = 0;
        self.name.clear();
        if spec.is_empty() {
            return Ok(spec);
        }
        let ch = spec.chars().next().unwrap();
        if ch.is_digit(10) {
            let mut pos: usize = 0;
            for x in spec.chars() {
                if x.is_digit(10) {
                    self.num = self.num * 10 + (x as usize) - ('0' as usize);
                    pos += 1;
                } else {
                    break;
                }
            }
            self.num -= 1;
            Ok(&spec[pos..])
        } else if ch.is_alphabetic() {
            let mut pos: usize = 0;
            let mut tmp_name = String::new();
            for x in spec.chars() {
                if x.is_alphanumeric() || x == '_' {
                    tmp_name.push(x);
                    pos += 1;
                } else {
                    break;
                }
            }
            self.name = tmp_name.as_bytes().to_vec();
            Ok(&spec[pos..])
        } else {
            err!("Bad parse of compare spec for {}", spec)
        }
    }
}

impl Default for NamedCol {
    fn default() -> Self {
        Self::new()
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
        let f: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
        assert_eq!(ColumnSet::range(&f, "<p")?, [1, 4]);
        assert_eq!(ColumnSet::range(&f, "2-+2")?, [1, 2, 3]);
        assert_eq!(ColumnSet::range(&f, "-")?, [0, 1, 2, 3, 4]);
        assert_eq!(ColumnSet::range(&f, "2-")?, [1, 2, 3, 4]);
        assert_eq!(ColumnSet::range(&f, "-2")?, [0, 1]);
        assert_err!(ColumnSet::range(&f, "1-2-3"), Err(Error::Error(_)));
        return Ok(());
    }

    #[test]
    fn text_range() -> Result<()> {
        let f: [&[u8]; 5] = [b"zero", b"one", b"two", b"three", b"four"];
        assert_eq!(ColumnSet::text_range(&f, "<p")?, [1, 4]);
        assert_eq!(ColumnSet::text_range(&f, "<p>g")?, [1]);
        assert_eq!(ColumnSet::text_range(&f, ">=g<=p")?, [1]);
        assert_err!(ColumnSet::text_range(&f, "><"), Err(Error::Error(_)));
        return Ok(());
    }
    #[test]
    fn do_compare() -> Result<()> {
        assert_eq!(ColumnSet::do_compare("aaa", "bbb", "<")?, true);
        assert_eq!(ColumnSet::do_compare("aaa", "bbb", "<=")?, true);
        assert_eq!(ColumnSet::do_compare("aaa", "bbb", ">")?, false);
        assert_eq!(ColumnSet::do_compare("aaa", "bbb", ">=")?, false);
        assert_eq!(ColumnSet::do_compare("aaa", "aaa", "<")?, false);
        assert_eq!(ColumnSet::do_compare("aaa", "aaa", "<=")?, true);
        assert_eq!(ColumnSet::do_compare("aaa", "aaa", ">")?, false);
        assert_eq!(ColumnSet::do_compare("aaa", "aaa", ">=")?, true);
        assert_err!(
            ColumnSet::do_compare("aaa", "aaa", "><"),
            Err(Error::Error(_))
        );
        return Ok(());
    }
}
