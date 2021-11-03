//! Tools for text file manipulation

#![warn(
    missing_copy_implementations,
    absolute_paths_not_starting_with_crate,
    explicit_outlives_requirements,
    keyword_idents,
    noop_method_call,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    trivial_numeric_casts,
    trivial_casts,
    unreachable_pub,
    unsafe_code,
    unused_lifetimes,
    unused_extern_crates,
    unused_qualifications
)]

use flate2::read::MultiGzDecoder;
use fs_err as fs;
use std::error;
use std::io::{self, BufRead, Read, Write};
use std::ops::{Deref, DerefMut};
use std::{fmt, str};

pub mod check;
pub mod column;
pub mod comp;
pub mod expr;
pub mod sort;
pub mod tooltest;

/// Shorthand for returning an error Result
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
    /// Custom cdxlib error
    Error(String),
    /// pass through ParseIntError
    ParseIntError(std::num::ParseIntError),
    /// pass through io::Error
    IoError(std::io::Error),
    /// pass through regex::Error
    RegexError(regex::Error),
    /// common error with long default string
    NeedLookup,
    /// be an error, but don't report anything
    Silent,
}
/// Result type for cdxlib
pub type Result<T> = core::result::Result<T, Error>;
impl error::Error for Error {}

impl Error {
    /// return true if this error should be treated as not an error
    pub fn suppress(&self) -> bool {
        match self {
            Error::IoError(err) => err.kind() == io::ErrorKind::BrokenPipe,
            _ => false,
        }
    }
    /// return true if this error should be treated as an error, bu silently
    pub fn silent(&self) -> bool {
        matches!(self, Error::Silent)
    }
}

impl From<regex::Error> for Error {
    fn from(kind: regex::Error) -> Error {
        Error::RegexError(kind)
    }
}

impl From<std::io::Error> for Error {
    fn from(kind: std::io::Error) -> Error {
        Error::IoError(kind)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(kind: std::num::ParseIntError) -> Error {
        Error::ParseIntError(kind)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Error(s) => write!(f, "Cdx Error : {}", s)?,
            Error::ParseIntError(s) => write!(f, "ParseIntError : {}", s)?,
            Error::IoError(s) => write!(f, "IoError : {}", s)?,
            Error::RegexError(s) => write!(f, "RegexError : {}", s)?,
            Error::NeedLookup => write!(
                f,
                "ColumnSet.lookup() must be called before ColumnSet.select()"
            )?,
            Error::Silent => write!(f, "Silent")?,
        }
        Ok(())
    }
}

/// pointers into a vector, simulating a slice without the ownership issues
#[derive(Debug, Clone, Copy, Default)]
pub struct FakeSlice {
    begin: u32,
    end: u32,
}

/// A line of a text file, broken into fields.
/// Access to the `lines` and `parts` is allowed, but should seldom be necessary
/// `line` includes the trailing newline, but no field contains the newline
/// An empty line contains one empty field
///```
/// use std::io::BufRead;
/// let mut data = b"one\ttwo\tthree\n";
/// let mut dp = &data[..];
/// let mut line = cdxlib::TextLine::new();
/// let eof = line.read(&mut dp).unwrap();
/// assert_eq!(eof, false);
/// assert_eq!(line.strlen(), 14);
/// line.split(b'\t');
/// assert_eq!(line.len(), 3);
/// assert_eq!(line.get(1), b"two");
///```
#[derive(Debug, Clone, Default)]
pub struct TextLine {
    /// The whole input line, without newline
    pub line: Vec<u8>,
    /// the individual columns
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
    /// Iterator over columns in the line
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
            if self.line.last() != Some(&b'\n') {
                self.line.push(b'\n');
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
        if begin != end {
            self.parts.push(FakeSlice {
                begin,
                end: end - 1,
            });
        }
    }
    /// return all parts as a vector
    pub fn vec(&self) -> Vec<&[u8]> {
        self.iter().collect()
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
    /// The file being read
    pub f: io::BufReader<Box<dyn Read>>,
}

impl Infile {
    /// create a new input file
    pub fn new(f: io::BufReader<Box<dyn Read>>) -> Self {
        Self { f }
    }
}

impl Default for Infile {
    fn default() -> Self {
        Self::new(io::BufReader::new(Box::new(io::empty())))
    }
}

impl fmt::Debug for Infile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Infile")
    }
}

impl Deref for Infile {
    type Target = io::BufReader<Box<dyn Read>>;

    fn deref(&self) -> &Self::Target {
        &self.f
    }
}

impl DerefMut for Infile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.f
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

#[derive(Debug, Default)]
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
            self.header.line.clear();
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
/// File reader for text file broken into lines with columns
pub struct Reader {
    file: Infile,
    /// The current line of text
    pub line: TextLine,
    cont: InfileContext,
    /// automatically split each TextLine
    pub do_split: bool,
}

impl Reader {
    /// make a new Reader
    pub fn new() -> Self {
        Self {
            file: Infile::default(),
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
    pub fn is_empty(&self) -> bool {
        self.cont.is_empty
    }
    /// have we read all the lines?
    pub fn is_done(&self) -> bool {
        self.cont.is_done
    }
    /// write the current text line with newline
    pub fn write(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.line.line)?;
        Ok(())
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

#[derive(Debug, Default)]
/// File reader for text file broken into lines with columns
/// previous N lines are stil available
pub struct LookbackReader {
    file: Infile,
    lines: Vec<TextLine>,
    cont: InfileContext,
    /// Automatically split each line into columns
    pub do_split: bool,
    curr: usize,
}

impl LookbackReader {
    /// make a new LookbackReader
    pub fn new(lookback: usize) -> Self {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        Self {
            file: Infile::default(),
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
    /// write the current text line with newline
    pub fn write_curr(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.curr_line().line)?;
        Ok(())
    }
    /// write header
    pub fn write_header(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.cont.header.line)?;
        Ok(())
    }
}

fn prerr(data: &[&[u8]]) -> Result<()> {
    for x in data {
        std::io::stderr().write_all(x)?;
    }
    std::io::stderr().write_all(b"\n")?;
    Ok(())
}

fn bytes_equal(c1: u8, c2: u8, ic: bool) -> bool {
    if c1 == c2 {
        return true;
    }
    if c1 == b'?' {
        return true;
    }
    if !ic {
        return false;
    }
    c1.to_ascii_lowercase() == c2.to_ascii_lowercase()
}

fn chars_equal(c1: char, c2: char, ic: bool) -> bool {
    if c1 == c2 {
        return true;
    }
    if c1 == '?' {
        return true;
    }
    if !ic {
        return false;
    }
    c1.to_lowercase().eq(c2.to_lowercase())
}

/// match in glob format
pub fn uglob(mut wild: &[u8], mut buff: &[u8], ic: bool) -> bool {
    while !buff.is_empty() && !wild.is_empty() && (wild[0] != b'*') {
        if !bytes_equal(wild[0], buff[0], ic) {
            return false;
        }
        wild = &wild[1..];
        buff = &buff[1..];
    }
    if wild.is_empty() && !buff.is_empty() {
        return false;
    }

    let mut cp: &[u8] = &[];
    let mut mp: &[u8] = &[];

    while !buff.is_empty() {
        if !wild.is_empty() && (wild[0] == b'*') {
            wild = &wild[1..];
            if wild.is_empty() {
                return true;
            }
            mp = wild;
            cp = &buff[1..];
        } else if !wild.is_empty() && bytes_equal(wild[0], buff[0], ic) {
            wild = &wild[1..];
            buff = &buff[1..];
        } else {
            wild = mp;
            cp = &cp[1..];
            buff = cp;
        }
    }

    while !wild.is_empty() && (wild[0] == b'*') {
        wild = &wild[1..];
    }
    wild.is_empty()
}

fn first(s: &str) -> char {
    s.chars().next().unwrap()
}

//fn first_len(s : &str) -> usize {
//    s.chars().next().unwrap().len_utf8()
//}

fn skip_first(s: &str) -> &str {
    &s[s.chars().next().unwrap().len_utf8()..]
}

/// match in glob format
pub fn sglob(mut wild: &str, mut buff: &str, ic: bool) -> bool {
    while !buff.is_empty() && !wild.is_empty() && (first(wild) != '*') {
        if !chars_equal(first(wild), first(buff), ic) {
            return false;
        }
        wild = skip_first(wild);
        buff = skip_first(buff);
    }
    if wild.is_empty() && !buff.is_empty() {
        return false;
    }

    let mut cp: &str = "";
    let mut mp: &str = "";

    while !buff.is_empty() {
        if !wild.is_empty() && (first(wild) == '*') {
            wild = &wild[1..];
            if wild.is_empty() {
                return true;
            }
            mp = wild;
            cp = skip_first(buff);
        } else if !wild.is_empty() && chars_equal(first(wild), first(buff), ic) {
            wild = skip_first(wild);
            buff = skip_first(buff);
        } else {
            wild = mp;
            cp = skip_first(cp);
            buff = cp;
        }
    }

    while !wild.is_empty() && (first(wild) == '*') {
        wild = skip_first(wild);
    }
    wild.is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uwild() {
        assert!(uglob(b"", b"", false));
        assert!(!uglob(b"", b"foo", false));
        assert!(!uglob(b"foo", b"", false));
        assert!(uglob(b"*", b"foo", false));
        assert!(uglob(b"*", b"", false));

        assert!(uglob(b"foo", b"foo", false));
        assert!(!uglob(b"bar", b"foo", false));
        assert!(uglob(b"foo", b"FoO", true));
        assert!(!uglob(b"bar", b"FoO", false));

        assert!(!uglob(b"*.txt", b"foo.txtt", false));
        assert!(uglob(b"*.txt", b"foo.txt", false));
        assert!(uglob(b"foo*.txt", b"foo.txt", false));
        assert!(!uglob(b"foo*.txt", b"foo.txtt", false));
        assert!(uglob(b"foo?txt", b"foo.txt", false));
        assert!(!uglob(b"foo?txt", b"foo..txt", false));

        assert!(uglob(b"*.txt", b"foO.txt", true));
        assert!(!uglob(b"*.txt", b"foO.txtt", true));
        assert!(uglob(b"foo*.txt", b"foO.txt", true));
        assert!(!uglob(b"foo*.txt", b"foO.txtt", true));
        assert!(uglob(b"foo?txt", b"foO.txt", true));
        assert!(!uglob(b"foo?txt", b"foO..txt", true));

        assert!(uglob(b"a?b", b"anb", false));
        assert!(!uglob(b"a?b", "añb".as_bytes(), false));

        assert!(uglob(
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/*/add_hour_*",
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz", false));
        assert!(uglob(
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/???/add_hour_*",
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz", false));
    }
    // ñ
    #[test]
    fn swild() {
        assert!(sglob("", "", false));
        assert!(!sglob("", "foo", false));
        assert!(!sglob("foo", "", false));
        assert!(sglob("*", "foo", false));
        assert!(sglob("*", "", false));

        assert!(sglob("foo", "foo", false));
        assert!(!sglob("bar", "foo", false));
        assert!(sglob("foo", "FoO", true));
        assert!(!sglob("bar", "FoO", false));

        assert!(!sglob("*.txt", "foo.txtt", false));
        assert!(sglob("*.txt", "foo.txt", false));
        assert!(sglob("foo*.txt", "foo.txt", false));
        assert!(!sglob("foo*.txt", "foo.txtt", false));
        assert!(sglob("foo?txt", "foo.txt", false));
        assert!(!sglob("foo?txt", "foo..txt", false));

        assert!(sglob("*.txt", "foO.txt", true));
        assert!(!sglob("*.txt", "foO.txtt", true));
        assert!(sglob("foo*.txt", "foO.txt", true));
        assert!(!sglob("foo*.txt", "foO.txtt", true));
        assert!(sglob("foo?txt", "foO.txt", true));
        assert!(!sglob("foo?txt", "foO..txt", true));

        assert!(sglob("a?b", "anb", false));
        assert!(sglob("a?b", "añb", false));

        assert!(!sglob("añb", "aÑb", false));
        assert!(!sglob("aÑb", "añb", false));
        assert!(sglob("añb", "aÑb", true));
        assert!(sglob("aÑb", "añb", true));

        assert!(sglob(
	    "s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/*/add_hour_*",
	    "s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz", false));
        assert!(sglob(
	    "s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/???/add_hour_*",
	    "s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz", false));
    }
}
