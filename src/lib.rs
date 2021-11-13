//! The command line tool `cdx` is a set of tools for text file manipulation
//! and command line data mining.
//! It is hoped that the associated library will be useful for theid party tools.

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
    unused_lifetimes,
    unused_extern_crates,
    unused_qualifications
)]

use flate2::read::MultiGzDecoder;
use fs_err as fs;
//use std::os::unix::io::FromRawFd;
use std::error;
use std::io::{self, BufRead, Read, Write};
use std::ops::{Deref, DerefMut};
use std::{fmt, str};

pub mod check;
pub mod column;
pub mod comp;
pub mod expr;
pub mod join;
pub mod sort;
pub mod tooltest;

/// Shorthand for returning an error Result
#[macro_export]
macro_rules! err {
    ($e:literal) => {Err(Error::Error($e.to_string()))};
    ($e:expr) => {Err(Error::Error($e))};
    ($($e:expr),+) => {Err(Error::Error(format!($($e),+)))}
}

/// Shorthand for returning an error Result
#[macro_export]
macro_rules! err_type {
    ($x:path, $i:path) => {
        impl From<$x> for Error {
            fn from(kind: $x) -> Error {
                $i(kind)
            }
        }
    };
}

/// Various errors
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Custom cdx error
    Error(String),
    /// pass through ParseIntError
    ParseIntError(std::num::ParseIntError),
    /// pass through ParseIntError
    ParseFloatError(std::num::ParseFloatError),
    /// pass through io::Error
    IoError(std::io::Error),
    /// pass through regex::Error
    RegexError(regex::Error),
    /// pass through FromUtf8Error
    FromUtf8Error(std::string::FromUtf8Error),
    /// pass through Utf8Error
    Utf8Error(std::str::Utf8Error),
    /// common error with long default string
    NeedLookup,
    /// be an error, but don't report anything
    Silent,
}
/// Result type for cdx
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

err_type!(std::str::Utf8Error, Error::Utf8Error);
err_type!(regex::Error, Error::RegexError);
err_type!(std::string::FromUtf8Error, Error::FromUtf8Error);
err_type!(std::io::Error, Error::IoError);
err_type!(std::num::ParseIntError, Error::ParseIntError);
err_type!(std::num::ParseFloatError, Error::ParseFloatError);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Error(s) => write!(f, "Cdx Error : {}", s)?,
            Error::ParseIntError(s) => write!(f, "ParseIntError : {}", s)?,
            Error::ParseFloatError(s) => write!(f, "ParseFloatError : {}", s)?,
            Error::IoError(s) => write!(f, "IoError : {}", s)?,
            Error::RegexError(s) => write!(f, "RegexError : {}", s)?,
            Error::FromUtf8Error(s) => write!(f, "FromUtf8Error : {}", s)?,
            Error::Utf8Error(s) => write!(f, "Utf8Error : {}", s)?,
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
/// let mut line = cdx::TextLine::new();
/// let eof = line.read(&mut dp).unwrap();
/// assert_eq!(eof, false);
/// assert_eq!(line.strlen(), 14);
/// line.split(b'\t');
/// assert_eq!(line.len(), 3);
/// assert_eq!(line.get(1), b"two");
///```
#[derive(Debug, Clone, Default)]
pub struct TextLine {
    /// The whole input line, with newline
    pub line: Vec<u8>,
    /// the individual columns
    pub parts: Vec<FakeSlice>,
}

/// as TextLine, but String rather than Vec<u8>
#[derive(Debug, Clone, Default)]
pub struct StringLine {
    /// The whole input line, with newline
    pub line: String,
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

impl fmt::Display for StringLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self {
            write!(f, "{} ", i)?;
        }
        Ok(())
    }
}

impl std::ops::Index<usize> for TextLine {
    type Output = [u8];
    fn index(&self, pos: usize) -> &Self::Output {
        self.get(pos)
    }
}

impl std::ops::Index<usize> for StringLine {
    type Output = str;
    fn index(&self, pos: usize) -> &Self::Output {
        self.get(pos)
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

impl StringLine {
    /// make a new StringLine
    pub fn new() -> Self {
        Self {
            line: String::new(),
            parts: Vec::new(),
        }
    }
    /// Iterator over columns in the line
    pub fn iter(&self) -> StringLineIter<'_> {
        StringLineIter {
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
    pub fn get(&self, index: usize) -> &str {
        if index >= self.parts.len() {
            &self.line[0..0]
        } else {
            &self.line[self.parts[index].begin as usize..self.parts[index].end as usize]
        }
    }
    /// Read a new line from a file, should generally be followed by `split`
    pub fn read<T: BufRead>(&mut self, f: &mut T) -> Result<bool> {
        self.clear();
        let sz = f.read_line(&mut self.line)?;
        if sz == 0 {
            Ok(true)
        } else {
            if self.line.as_bytes().last() != Some(&b'\n') {
                self.line.push('\n');
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
        for ch in self.line.bytes() {
            if ch == delim {
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
    pub fn vec(&self) -> Vec<&str> {
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

impl<'a> IntoIterator for &'a StringLine {
    type Item = &'a str;
    type IntoIter = StringLineIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StringLineIter {
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

/// Iterator over the columns in a StringLine
#[derive(Debug, Clone)]
pub struct StringLineIter<'a> {
    line: &'a StringLine,
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
            Some(&self.line[self.index - 1])
        }
    }
}

impl<'a> Iterator for StringLineIter<'a> {
    // we will be counting with usize
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.line.len() {
            None
        } else {
            self.index += 1;
            Some(&self.line[self.index - 1])
        }
    }
}

/// Input file. Wrapped in a type so I can 'impl Debug'
pub struct Infile(
    /// The file being read
    io::BufReader<Box<dyn Read>>,
);

impl Infile {
    /// create a new input file
    pub fn new(f: io::BufReader<Box<dyn Read>>) -> Self {
        Self(f)
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
        &self.0
    }
}

impl DerefMut for Infile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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

fn unescape_vec(data: &[u8]) -> Vec<u8> {
    let mut ret: Vec<u8> = Vec::with_capacity(data.len());
    let mut last_was_slash = false;
    for x in data {
        if last_was_slash {
            ret.push(match x {
                b'n' => b'\n',
                b't' => b'\t',
                b's' => b' ',
                ch => *ch,
            });
            last_was_slash = false;
        } else if x == &b'\\' {
            last_was_slash = true;
        } else {
            ret.push(*x);
        }
    }
    if last_was_slash {
        ret.push(b'\\');
    }
    ret
}

/// Make an Infile from a file name
pub fn get_reader(name: &str) -> Result<Infile> {
    let inner: Box<dyn Read> = {
        if name == "-" {
            //	    unsafe { Box::new(std::fs::File::from_raw_fd(1)) }
            Box::new(io::stdin())
            //            } else if name.starts_with("s3://") {
            //                Box::new(open_s3_file(name)?)
        } else if let Some(stripped) = name.strip_prefix("<<") {
            Box::new(std::io::Cursor::new(unescape_vec(stripped.as_bytes())))
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
    header: StringLine,
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
            header: StringLine::new(),
            delim: b'\t',
            is_done: true,
            is_empty: true,
        }
    }
    fn read_header(&mut self, file: &mut impl BufRead, line: &mut TextLine) -> Result<()> {
        if self.header.read(file)? {
            self.is_done = true;
            self.is_empty = true;
            return Ok(());
        }
        self.is_done = false;
        self.is_empty = false;
        if self.header.line.starts_with(" CDX") {
            self.delim = self.header.line.as_bytes()[4];
            self.header.split(self.delim);
            self.header.parts.remove(0);
            if line.read(file)? {
                self.is_done = true;
                return Ok(());
            }
            line.split(self.delim);
        } else {
            self.delim = b'\t';
            line.line = self.header.line.as_bytes().to_vec();
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
    pub fn header(&self) -> &StringLine {
        &self.cont.header
    }
    /// get column names
    pub fn names(&self) -> Vec<&str> {
        self.cont.header.vec()
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut *self.file, &mut self.line)
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
            if self.line.read(&mut *self.file)? {
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
    pub fn names(&self) -> Vec<&str> {
        self.cont.header.vec()
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut *self.file, &mut self.lines[0])
    }
    /// The full text of the header, without the trailing newline
    pub fn header(&self) -> &String {
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
        if self.lines[self.curr].read(&mut *self.file)? {
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
        w.write_all(self.cont.header.line.as_bytes())?;
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
pub fn bglob(mut wild: &[u8], mut buff: &[u8], ic: bool) -> bool {
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
    debug_assert!(!s.is_empty());
    s.chars().next().unwrap()
}

//fn first_len(s : &str) -> usize {
//    s.chars().next().unwrap().len_utf8()
//}

fn skip_first(s: &str) -> &str {
    debug_assert!(!s.is_empty());
    &s[s.chars().next().unwrap().len_utf8()..]
}

fn take_first(s: &mut &str) -> char {
    debug_assert!(!s.is_empty());
    let x = s.chars().next().unwrap();
    *s = &s[x.len_utf8()..];
    x
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

/// How to combine headers from multiple sources
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum HeaderMode {
    /// First can be anything, others must match
    Match,
    /// First must be CDX, others must match
    Require,
    /// Any CDX are removed
    Strip,
    /// No CDX allowed
    None,
    /// First can be anything, others blindly accepted
    Trust,
    /// Ignore CDX is present
    Ignore,
}
/// strings associated with HeaderMode
pub const HEADER_MODE: [&str; 6] = ["Match", "Require", "Strip", "None", "Trust", "Ignore"];

impl str::FromStr for HeaderMode {
    type Err = Error;
    fn from_str(spec: &str) -> Result<Self> {
        if spec.to_ascii_lowercase() == "match" {
            Ok(HeaderMode::Match)
        } else if spec.to_ascii_lowercase() == "require" {
            Ok(HeaderMode::Require)
        } else if spec.to_ascii_lowercase() == "strip" {
            Ok(HeaderMode::Strip)
        } else if spec.to_ascii_lowercase() == "none" {
            Ok(HeaderMode::None)
        } else if spec.to_ascii_lowercase() == "trust" {
            Ok(HeaderMode::Trust)
        } else if spec.to_ascii_lowercase() == "ignore" {
            Ok(HeaderMode::Ignore)
        } else {
            err!(
                "Input Header Mode must be one of Match, Require, Strip, None or Trust : {}",
                spec
            )
        }
    }
}

impl Default for HeaderMode {
    fn default() -> Self {
        HeaderMode::Match
    }
}

/// Object to enforce HeaderMode
#[derive(Debug, Default)]
pub struct HeaderChecker {
    /// the mode to enforce
    pub mode: HeaderMode,
    /// the first header seen
    head: Vec<u8>,
    /// true after first header has been processed
    saw_one: bool,
}

#[allow(dead_code)]
fn is_cdx(data: &[u8]) -> bool {
    data.starts_with(b" CDX")
    // check for more validity?
}

fn is_valid_cdx(data_in: &[u8], mode: HeaderMode, fname: &str) -> Result<bool> {
    if mode == HeaderMode::Ignore {
        return Ok(false);
    }
    if !data_in.starts_with(b" CDX") {
        return Ok(false);
    }
    if mode == HeaderMode::Strip || mode == HeaderMode::None {
        return Ok(true);
    }
    let mut data = data_in;
    if data.last().unwrap() == &b'\n' {
        data = &data[..data.len() - 1];
    }
    if data.len() < 6 {
        return err!("File {} has an oddly truncated header line", fname);
    }
    let delim = data[4];
    if delim == b'\n' || delim.is_ascii_alphanumeric() || delim > 127 {
        return err!("Header for file {} has an invalid column delimiter", fname);
    }
    let data = str::from_utf8(&data[5..])?;
    let delim = char::from_u32(delim as u32).unwrap();
    for x in data.split(|ch| ch == delim) {
        if x.is_empty() {
            return err!("File {} has an empty column name", fname);
        }
        if !first(x).is_alphabetic() {
            return err!("Header for file {} has column name {} which does not start with an alphabetic character.", fname, x);
        }
        for ch in x.chars() {
            if !ch.is_alphanumeric() && ch != '_' {
                return err!("Header for file {} has column name {} which contains something other than alphnumeric and underscore.", fname, x);
            }
        }
    }
    Ok(true)
}

impl HeaderChecker {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// new with mode
    pub fn from_mode(mode: HeaderMode) -> Self {
        Self {
            mode,
            ..Self::default()
        }
    }
    /// call for the first line of every input file
    /// return true if the line should be part of the output
    pub fn check(&mut self, first_line: &[u8], fname: &str) -> Result<bool> {
        // FIXME - if is_cdx, make sure is also a valid header
        let cdx = is_valid_cdx(first_line, self.mode, fname)?;
        if first_line.is_empty() {
            Ok(false)
        } else if first_line.last().unwrap() != &b'\n' && cdx {
            err!("Malformed Header line in file {}", &fname)
        } else if self.saw_one {
            match self.mode {
                HeaderMode::Match => {
                    if cdx {
                        if self.head.is_empty() {
                            return err!("CDX Header found in {}, but first file had none.", fname);
                        }
                        if first_line != self.head {
                            return err!(
                                "Header Mismatch. First was {}, File {} has {}",
                                String::from_utf8_lossy(&self.head),
                                fname,
                                String::from_utf8_lossy(first_line)
                            );
                        }
                        Ok(false)
                    } else {
                        if !self.head.is_empty() {
                            return err!(
                                "No CDX Header found in {}, but first file had one.",
                                fname
                            );
                        }
                        Ok(true)
                    }
                }
                HeaderMode::Require => {
                    if !cdx {
                        return err!("No CDX Header found in {} where one was required.", fname);
                    }
                    Ok(false)
                }
                HeaderMode::Strip => Ok(!cdx),
                HeaderMode::Ignore => Ok(true),
                HeaderMode::None => {
                    if cdx {
                        return err!("CDX Header found in {} where one was forbidden.", fname);
                    }
                    Ok(true)
                }
                HeaderMode::Trust => Ok(!cdx),
            }
        } else {
            self.saw_one = true;
            if cdx && self.mode == HeaderMode::Strip {
                return err!("Found CDX header in {} where one was forbidden", fname);
            } else if !cdx && self.mode == HeaderMode::Require {
                return err!("Found no CDX header in {} where one was required", fname);
            }
            if cdx && (self.mode == HeaderMode::Strip) {
                return Ok(false);
            }
            if cdx {
                self.head = first_line.to_vec();
            }
            Ok(true)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uwild() {
        assert!(bglob(b"", b"", false));
        assert!(!bglob(b"", b"foo", false));
        assert!(!bglob(b"foo", b"", false));
        assert!(bglob(b"*", b"foo", false));
        assert!(bglob(b"*", b"", false));

        assert!(bglob(b"foo", b"foo", false));
        assert!(!bglob(b"bar", b"foo", false));
        assert!(bglob(b"foo", b"FoO", true));
        assert!(!bglob(b"bar", b"FoO", false));

        assert!(!bglob(b"*.txt", b"foo.txtt", false));
        assert!(bglob(b"*.txt", b"foo.txt", false));
        assert!(bglob(b"foo*.txt", b"foo.txt", false));
        assert!(!bglob(b"foo*.txt", b"foo.txtt", false));
        assert!(bglob(b"foo?txt", b"foo.txt", false));
        assert!(!bglob(b"foo?txt", b"foo..txt", false));

        assert!(bglob(b"*.txt", b"foO.txt", true));
        assert!(!bglob(b"*.txt", b"foO.txtt", true));
        assert!(bglob(b"foo*.txt", b"foO.txt", true));
        assert!(!bglob(b"foo*.txt", b"foO.txtt", true));
        assert!(bglob(b"foo?txt", b"foO.txt", true));
        assert!(!bglob(b"foo?txt", b"foO..txt", true));

        assert!(bglob(b"a?b", b"anb", false));
        assert!(!bglob(b"a?b", "añb".as_bytes(), false));

        assert!(bglob(
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/*/add_hour_*",
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz", false));
        assert!(bglob(
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
