//! Handles conversion between named column sets and lists of column numbers
//! Also helps with selecting those columns from a line of text

#![warn(missing_copy_implementations,
	absolute_paths_not_starting_with_crate,
	explicit_outlives_requirements,
	keyword_idents,
	noop_method_call,
	rust_2021_incompatible_closure_captures,
	rust_2021_incompatible_or_patterns,
	rust_2021_prefixes_incompatible_syntax,
	rust_2021_prelude_collisions,
        missing_debug_implementations,
//        missing_docs,
	rust_2018_idioms,
        trivial_numeric_casts,
	trivial_casts,
	unreachable_pub,
        unsafe_code,
	unused_lifetimes,
        unused_extern_crates,
        unused_import_braces,
        unused_qualifications)]

use flate2::read::MultiGzDecoder;
use fs_err as fs;
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::error;
use std::io::{self, BufRead, Read, Write};
use std::{fmt, str};
pub mod args;
pub mod cdxmain;
pub mod comp;
pub mod sort;
pub mod cut_main;
pub mod uniq_main;
pub mod sort_main;

#[macro_export]
macro_rules! err {
    ($e:literal) => {Err(Error::Error($e.to_string()))};
    ($e:expr) => {Err(Error::Error($e))};
    ($($e:expr),+) => {Err(Error::Error(format!($($e),+)))}
}

/// Various errors
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    Error(String),
    ParseIntError(std::num::ParseIntError),
    IoError(std::io::Error),
    NeedLookup,
}
pub type Result<T> = core::result::Result<T, Error>;
impl error::Error for Error {}

impl Error {
    pub fn suppress(&self) -> bool {
        match self {
            Error::IoError(err) => err.kind() == io::ErrorKind::BrokenPipe,
            _ => false,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(kind: std::io::Error) -> Error {
        Error::IoError(kind)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Error(s) => write!(f, "Cdx Error : {}", s)?,
            Error::ParseIntError(s) => write!(f, "ParseIntError : {}", s)?,
            Error::IoError(s) => write!(f, "IoError : {}", s)?,
            Error::NeedLookup => write!(
                f,
                "ColumnSet.lookup() must be called before ColumnSet.select()"
            )?,
        }
        Ok(())
    }
}

/// pointers into a vector, simulating a slice without the ownership issues
#[derive(Debug, Clone, Copy)]
pub struct FakeSlice {
    begin: u32,
    end: u32,
}

/// A line of a text file, broken into columns.
/// Access to the `lines` and `parts` is allowed, but should seldom be necessary
/// `line` does not include the trailing newline
/// An empty line contains one empty column
///```
/// use std::io::BufRead;
/// let mut data = b"one\ttwo\tthree\n";
/// let mut dp = &data[..];
/// let mut line = cdx::TextLine::new();
/// let eof = line.read(&mut dp).unwrap();
/// assert_eq!(eof, false);
/// assert_eq!(line.strlen(), 13);
/// line.split(b'\t');
/// assert_eq!(line.len(), 3);
/// assert_eq!(line.get(1), b"two");
///```
#[derive(Debug, Clone)]
pub struct TextLine {
    pub line: Vec<u8>,
    pub parts: Vec<FakeSlice>,
}

impl fmt::Display for TextLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self {
            write!(f, "{} ", str::from_utf8(i).unwrap())?;
        }
        Ok(())
    }
}

impl TextLine {
    /// make a new TextLine
    pub fn new() -> Self {
        Self {
            line: Vec::new(),
            parts: Vec::new(),
        }
    }
    pub fn iter(&self) -> TextLineIter<'_> {
        TextLineIter {
            line: self,
            index: 0,
        }
    }
    fn clear(&mut self) {
        self.parts.clear();
        self.line.clear();
    }
    /// How many column in the line
    pub fn len(&self) -> usize {
        self.parts.len()
    }
    /// How many bytes in the line
    pub fn strlen(&self) -> usize {
        self.line.len()
    }
    /// should always be false, but required by clippy
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }
    /// Get one column. Return an empty column if index is too big.
    pub fn get(&self, index: usize) -> &[u8] {
        if index >= self.parts.len() {
            &self.line[0..0]
        } else {
            &self.line[self.parts[index].begin as usize..self.parts[index].end as usize]
        }
    }
    /// Read a new line from a file, should generally be followed by `split`
    pub fn read<T: BufRead>(&mut self, f: &mut T) -> Result<bool> {
        self.clear();
        let sz = f.read_until(b'\n', &mut self.line)?;
        if sz == 0 {
            Ok(true)
        } else {
            if self.line.last() == Some(&b'\n') {
                self.line.pop();
            }
            Ok(false)
        }
    }
    /// split the line into columns
    /// hypothetically you could split on one delimiter, do some work, then split on a different delimiter.
    pub fn split(&mut self, delim: u8) {
        self.parts.clear();
        let mut begin: u32 = 0;
        let mut end: u32 = 0;
        #[allow(clippy::explicit_counter_loop)] // I need the counter to be u32
        for ch in self.line.iter() {
            if *ch == delim {
                self.parts.push(FakeSlice { begin, end });
                begin = end + 1;
            }
            end += 1;
        }
        self.parts.push(FakeSlice { begin, end });
    }
    // return all parts as a vector
    pub fn vec(&self) -> Vec<&[u8]> {
        self.iter().collect()
    }
}

impl Default for TextLine {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> IntoIterator for &'a TextLine {
    type Item = &'a [u8];
    type IntoIter = TextLineIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TextLineIter {
            line: self,
            index: 0,
        }
    }
}

/// Iterator over the columns in a TextLine
#[derive(Debug, Clone)]
pub struct TextLineIter<'a> {
    line: &'a TextLine,
    index: usize,
}

impl<'a> Iterator for TextLineIter<'a> {
    // we will be counting with usize
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.line.len() {
            None
        } else {
            self.index += 1;
            Some(self.line.get(self.index - 1))
        }
    }
}

/// Input file. Wrapped in a type so I can 'impl Debug'
pub struct Infile {
    pub f: io::BufReader<Box<dyn Read>>,
}

impl Infile {
    /// create a new input file
    pub fn new(f: io::BufReader<Box<dyn Read>>) -> Self {
        Self { f }
    }
}

impl fmt::Debug for Infile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Infile")
    }
}

type Outfile = io::BufWriter<Box<dyn Write>>;

/// Make an Outfile from a file name
pub fn get_writer(name: &str) -> Result<Outfile> {
    let inner: Box<dyn Write> = {
        if name == "-" {
            Box::new(io::stdout())
        } else if name == "--" {
            Box::new(io::stderr())
        } else {
            Box::new(fs::OpenOptions::new().write(true).create(true).open(name)?)
        }
    };
    Ok(io::BufWriter::new(inner))
}

/// Make an Infile from a file name
pub fn get_reader(name: &str) -> Result<Infile> {
    let inner: Box<dyn Read> = {
        if name == "-" {
            Box::new(io::stdin())
            //            } else if name.starts_with("s3://") {
            //                Box::new(open_s3_file(name)?)
        } else {
            Box::new(fs::File::open(name)?)
        }
    };
    let mut outer = io::BufReader::new(inner);
    let start = outer.fill_buf()?;
    if start.starts_with(&[0x1fu8, 0x8bu8, 0x08u8]) {
        outer = io::BufReader::new(Box::new(MultiGzDecoder::new(outer)));
    }
    Ok(Infile::new(outer))
}

#[derive(Debug)]
struct InfileContext {
    header: TextLine,
    delim: u8,
    is_done: bool,
    is_empty: bool,
}

// FIXME -- specify delimiter
// if CDX and specified and different, then strip header
/// Reader header line, if any, and first line of text
impl InfileContext {
    fn new() -> Self {
        Self {
            header: TextLine::new(),
            delim: b'\t',
            is_done: true,
            is_empty: true,
        }
    }
    fn read_header(&mut self, file: &mut Infile, line: &mut TextLine) -> Result<()> {
        if self.header.read(&mut file.f)? {
            self.is_done = true;
            self.is_empty = true;
            return Ok(());
        }
        self.is_done = false;
        self.is_empty = false;
        if self.header.line.starts_with(b" CDX") {
            self.delim = self.header.line[4];
            self.header.split(self.delim);
            self.header.parts.remove(0);
            if line.read(&mut file.f)? {
                self.is_done = true;
                return Ok(());
            }
            line.split(self.delim);
        } else {
            self.delim = b'\t';
            *line = self.header.clone();
            line.split(self.delim);
            let mut head_str = String::new();
            for i in 1..=line.len() {
                head_str += "c";
                head_str += &i.to_string();
                head_str += "\t";
            }
            head_str.pop();
            head_str += "\n";
            let mut fake_head = head_str.as_bytes();
            self.header.read(&mut fake_head)?;
            self.header.split(self.delim);
        }
        Ok(())
    }
}

impl Default for InfileContext {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
/// File reader for text file broken into lines with columns
pub struct Reader {
    file: Infile,
    pub line: TextLine,
    cont: InfileContext,
    pub do_split: bool,
}

impl Reader {
    /// make a new Reader
    pub fn new() -> Self {
        Self {
            file: Infile::new(io::BufReader::new(Box::new(io::empty()))),
            line: TextLine::new(),
            cont: InfileContext::new(),
            do_split: true,
        }
    }
    /// get current line
    pub fn curr(&self) -> &TextLine {
        &self.line
    }
    /// get header
    pub fn header(&self) -> &TextLine {
        &self.cont.header
    }
    /// get column names
    pub fn names(&self) -> Vec<&[u8]> {
        self.cont.header.vec()
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut self.file, &mut self.line)
    }
    /// was the file zero bytes?
    pub fn empty(&self) -> bool {
        self.cont.is_empty
    }
    /// have we read all the lines?
    pub fn done(&self) -> bool {
        self.cont.is_done
    }
    /// get the next line of text. Return true if no more data available.
    pub fn getline(&mut self) -> Result<bool> {
        if !self.cont.is_done {
            if self.line.read(&mut self.file.f)? {
                self.cont.is_done = true;
            } else if self.do_split {
                self.line.split(self.cont.delim);
            }
        }
        Ok(self.cont.is_done)
    }
}

impl Default for Reader {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
/// File reader for text file broken into lines with columns
/// previous N lines are stil available
pub struct LookbackReader {
    file: Infile,
    lines: Vec<TextLine>,
    cont: InfileContext,
    pub do_split: bool,
    curr: usize,
}

impl LookbackReader {
    /// make a new LookbackReader
    pub fn new(lookback: usize) -> Self {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        Self {
            file: Infile::new(io::BufReader::new(Box::new(io::empty()))),
            lines,
            cont: InfileContext::new(),
            do_split: true,
            curr: 0,
        }
    }
    /// get column names
    pub fn names(&self) -> Vec<&[u8]> {
        self.cont.header.vec()
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut self.file, &mut self.lines[0])
    }
    /// The full text of the header, without the trailing newline
    pub fn header(&self) -> &Vec<u8> {
        &self.cont.header.line
    }
    /// was file zero bytes?
    pub fn is_empty(&self) -> bool {
        self.cont.is_empty
    }
    /// have we hit EOF?
    pub fn is_done(&self) -> bool {
        self.cont.is_done
    }
    fn incr(&mut self) {
        self.curr += 1;
        if self.curr >= self.lines.len() {
            self.curr = 0;
        }
    }
    /// get next line of text
    pub fn getline(&mut self) -> Result<bool> {
        self.incr();
        if self.lines[self.curr].read(&mut self.file.f)? {
            self.cont.is_done = true;
        } else if self.do_split {
            self.lines[self.curr].split(self.cont.delim);
        }
        Ok(self.cont.is_done)
    }
    /// get current line of text
    pub fn curr_line(&self) -> &TextLine {
        &self.lines[self.curr]
    }
    /// get a previous line of text
    /// looking back from the start of the file shows empty lines.
    pub fn prev_line(&self, lookback: usize) -> &TextLine {
        if lookback <= self.curr {
            &self.lines[self.curr - lookback]
        } else {
            &self.lines[self.curr + self.lines.len() - lookback]
        }
    }
    pub fn write_curr(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.curr_line().line)?;
        w.write_all(&[b'\n'])?;
        Ok(())
    }
    pub fn write_header(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.cont.header.line)?;
        w.write_all(&[b'\n'])?;
        Ok(())
    }
}

impl Default for LookbackReader {
    fn default() -> Self {
        Self::new(1)
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
