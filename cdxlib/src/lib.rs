//! Tools for text file manipulation

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
	missing_docs,
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
use std::error;
use std::io::{self, BufRead, Read, Write};
use std::{fmt, str};
pub mod column;
pub mod comp;
pub mod sort;

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
    /// common error with long default string
    NeedLookup,
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
    /// return all parts as a vector
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
    /// The file being read
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
    /// write the current text line with newline
    pub fn write_curr(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.curr_line().line)?;
        w.write_all(&[b'\n'])?;
        Ok(())
    }
    /// write header
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
