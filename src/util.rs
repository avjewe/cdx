//! Misc utility stuff

use flate2::read::MultiGzDecoder;
use fs_err as fs;
use std::error;
use std::ffi::OsStr;
use std::io::{self, BufRead, Read, Write};
use std::ops::{Deref, DerefMut};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::{fmt, str};
use crate::text::Text;

/// Shorthand for returning an error Result
#[macro_export]
macro_rules! err {
    ($e:literal) => {Err(Error::Error($e.to_string()))};
    ($e:expr) => {Err(Error::Error($e))};
    ($($e:expr),+) => {Err(Error::Error(format!($($e),+)))}
}
pub use err;
// Shorthand for implementing a pass-through error
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
    pub const fn silent(&self) -> bool {
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
            Error::Error(s) => write!(f, "{}", s)?,
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
impl FakeSlice {
    /// get slice from FakeSlice
    pub fn get<'a>(&self, data : &'a [u8]) -> &'a[u8] {
	&data[self.begin as usize..self.end as usize]
    }
    /// len
    pub const fn len(&self) -> usize {
	(self.end-self.begin) as usize
    }
    /// is empty?
    pub const fn is_empty(&self) -> bool {
	self.begin == self.end
    }
}

/// A line of a text file, broken into fields.
/// Access to the `lines` and `parts` is allowed, but should seldom be necessary
/// `line` includes the trailing newline, but no field contains the newline
/// An empty line contains one empty field
///```
/// use std::io::BufRead;
/// let mut data = b"one\ttwo\tthree\n";
/// let mut dp = &data[..];
/// let mut line = cdx::util::TextLine::new();
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

/// write a buffer. If non-empty, ensure trailing newline
pub fn write_all_nl(w: &mut impl Write, buf: &[u8]) -> Result<()> {
    if !buf.is_empty() {
        w.write_all(buf)?;
        if buf[buf.len() - 1] != b'\n' {
            w.write_all(&[b'\n'])?;
        }
    }
    Ok(())
}

impl TextLine {
    /// assign TextLine into existing TextLine, avoiding allocation if possible
    pub fn assign(&mut self, x: &Self) {
        self.line.clear();
        self.line.extend_from_slice(&x.line[..]);
        self.parts.clear();
        self.parts.extend_from_slice(&x.parts[..]);
    }
    /// make a new TextLine
    pub const fn new() -> Self {
        Self {
            line: Vec::new(),
            parts: Vec::new(),
        }
    }
    /// Iterator over columns in the line
    pub const fn iter(&self) -> TextLineIter<'_> {
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
	    let mut f = FakeSlice{begin,end};
	    if self.line[(end-1) as usize] == b'\n' {
		f.end -= 1;
	    }
            self.parts.push(f);
        }
    }
    /// return all parts as a vector
    pub fn vec(&self) -> Vec<&[u8]> {
        self.iter().collect()
    }
}

impl StringLine {
    /// make a new StringLine
    pub const fn new() -> Self {
        Self {
            line: String::new(),
            parts: Vec::new(),
        }
    }
    /// Iterator over columns in the line
    pub const fn iter(&self) -> StringLineIter<'_> {
        StringLineIter {
            line: self,
            index: 0,
        }
    }
    /// create a fake CDX header with columns c1,c2...
    pub fn fake(&mut self, num_cols: usize, delim: u8) {
        self.line = " CDX".to_string();
        for i in 1..=num_cols {
            self.line += std::str::from_utf8(&[delim]).unwrap();
            self.line += "c";
            self.line += &i.to_string();
        }
        self.line += "\n";
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
        if self.index >= self.line.parts.len() {
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
    pub io::BufReader<Box<dyn Read>>,
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

impl AsRef<io::BufReader<Box<dyn Read>>> for Infile {
    fn as_ref(&self) -> &io::BufReader<Box<dyn Read>> {
        &self.0
    }
}

impl AsMut<io::BufReader<Box<dyn Read>>> for Infile {
    fn as_mut(&mut self) -> &mut io::BufReader<Box<dyn Read>> {
        &mut self.0
    }
}

/// output file type
pub type Outfile = io::BufWriter<Box<dyn Write>>;

/// Make an Outfile from a file name
pub fn get_writer2<P: AsRef<Path>>(name: P) -> Result<Outfile> {
    let name = name.as_ref().as_os_str();
    let inner: Box<dyn Write> = {
        if name == OsStr::new("-") {
            Box::new(io::stdout())
        } else if name == OsStr::new("--") {
            Box::new(io::stderr())
        } else {
            Box::new(fs::OpenOptions::new().write(true).create(true).open(name)?)
        }
    };
    Ok(io::BufWriter::new(inner))
}

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

// should return Cow<>
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
pub fn get_reader2<P: AsRef<Path>>(name: P) -> Result<Infile> {
    let name = name.as_ref().as_os_str();
    let inner: Box<dyn Read> = {
        if name == OsStr::new("-") {
            //	    unsafe { Box::new(std::fs::File::from_raw_fd(1)) }
            Box::new(io::stdin())
            //            } else if name.starts_with("s3://") {
            //                Box::new(open_s3_file(name)?)
        } else if name.as_bytes().starts_with(b"<<") {
            Box::new(std::io::Cursor::new(unescape_vec(&name.as_bytes()[2..])))
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
/// shared context for any input file type
pub struct InfileContext {
    /// CDX header, contructed if necessary
    pub header: StringLine,
    /// delimter
    pub delim: u8,
    /// have we read all the btes of the file
    pub is_done: bool,
    /// is the file length zero
    pub is_empty: bool,
    /// was there a CDX header?
    pub has_header: bool,
}

/// create appropriate header from first line of file
pub fn make_header(line: &[u8]) -> StringLine {
    let mut s = StringLine::new();
    if is_cdx(line) {
        s.line = String::from_utf8_lossy(&line[5..]).to_string();
    } else {
        s.line = String::new();
        for x in 1..=line.split(|ch| *ch == b'\t').count() {
            s.line.push_str(&format!("c{}\t", x));
        }
        s.line.pop();
    }
    s.split(b'\t');
    s
}

// FIXME -- specify delimiter
// if CDX and specified and different, then strip header
/// Reader header line, if any, and first line of text
impl InfileContext {
    const fn new() -> Self {
        Self {
            header: StringLine::new(),
            delim: b'\t',
            is_done: true,
            is_empty: true,
            has_header: false,
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
            self.has_header = true;
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
    /// context
    pub cont: InfileContext,
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
    /// make a new Reader
    pub fn new_open(name: &str) -> Result<Self> {
        let mut tmp = Self {
            file: get_reader(name)?,
            line: TextLine::new(),
            cont: InfileContext::new(),
            do_split: true,
        };
        tmp.cont.read_header(&mut *tmp.file, &mut tmp.line)?;
        Ok(tmp)
    }
    /// get current line
    pub const fn curr(&self) -> &TextLine {
        &self.line
    }
    /// get current line
    pub const fn curr_line(&self) -> &TextLine {
        &self.line
    }
    /// get current line contents, without the trailing newline
    pub fn curr_nl(&self) -> &[u8] {
        &self.line.line[0..self.line.line.len() - 1]
    }
    /// get header
    pub const fn header(&self) -> &StringLine {
        &self.cont.header
    }
    /// get header
    pub const fn header_line(&self) -> &String {
        &self.cont.header.line
    }
    /// has header
    pub const fn has_header(&self) -> bool {
        self.cont.has_header
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
    pub const fn is_empty(&self) -> bool {
        self.cont.is_empty
    }
    /// get delimiter
    pub const fn delim(&self) -> u8 {
        self.cont.delim
    }
    /// have we read all the lines?
    pub const fn is_done(&self) -> bool {
        self.cont.is_done
    }
    /// write the current text line with newline
    pub fn write(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.line.line)?;
        Ok(())
    }
    /// write header
    pub fn write_header(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(self.cont.header.line.as_bytes())?;
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
    /// context
    pub cont: InfileContext,
    /// Automatically split each line into columns
    pub do_split: bool,
    curr: usize,
    line_num : usize,
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
	    line_num: 0,
        }
    }
    /// make a new LookbackReader
    pub fn new_open(name : &str, lookback: usize) -> Result<Self> {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        let mut tmp = Self {
            file: get_reader(name)?,
            lines,
            cont: InfileContext::new(),
            do_split: true,
            curr: 0,
	    line_num: 0,
        };
        tmp.cont.read_header(&mut *tmp.file, &mut tmp.lines[0])?;
        Ok(tmp)
    }
    /// get delimiter
    pub const fn delim(&self) -> u8 {
        self.cont.delim
    }
    /// get column names
    pub fn names(&self) -> Vec<&str> {
        self.cont.header.vec()
    }
    /// write the current text line with newline
    pub fn write(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.curr_line().line)?;
        Ok(())
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut *self.file, &mut self.lines[0])
    }
    /// The full text of the header, without the trailing newline
    pub const fn header_line(&self) -> &String {
        &self.cont.header.line
    }
    /// was file zero bytes?
    pub const fn is_empty(&self) -> bool {
        self.cont.is_empty
    }
    /// have we hit EOF?
    pub const fn is_done(&self) -> bool {
        self.cont.is_done
    }
    /// line number of curr_line
    pub const fn line_number(&self) -> usize {
	self.line_num
    }
    fn incr(&mut self) {
	self.line_num += 1;
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
    /// get current line of text
    pub fn curr(&self) -> &TextLine {
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
    /// write previous text line with newline
    pub fn write_prev(&self, w: &mut impl Write, lookback: usize) -> Result<()> {
        w.write_all(&self.prev_line(lookback).line)?;
        Ok(())
    }
    /// write header
    pub fn write_header(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(self.cont.header.line.as_bytes())?;
        Ok(())
    }
    /// write header
    pub const fn header(&self) -> &StringLine {
        &self.cont.header
    }
    /// write header
    pub const fn has_header(&self) -> bool {
        self.cont.has_header
    }
}

/// print a bunch of u8 to stderr
pub fn prerr(data: &[&[u8]]) {
    for x in data {
        std::io::stderr().write_all(x).unwrap();
    }
    std::io::stderr().write_all(b"\n").unwrap();
}
/*
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
*/
/*
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
*/
//fn first_len(s : &str) -> usize {
//    s.chars().next().unwrap().len_utf8()
//}
/*
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
*/
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
            Ok(Self::Match)
        } else if spec.to_ascii_lowercase() == "require" {
            Ok(Self::Require)
        } else if spec.to_ascii_lowercase() == "strip" {
            Ok(Self::Strip)
        } else if spec.to_ascii_lowercase() == "none" {
            Ok(Self::None)
        } else if spec.to_ascii_lowercase() == "trust" {
            Ok(Self::Trust)
        } else if spec.to_ascii_lowercase() == "ignore" {
            Ok(Self::Ignore)
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
        Self::Match
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

/// Is this line a CDX header?
pub fn is_cdx(data: &[u8]) -> bool {
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
        if !x.first().is_alphabetic() {
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
    /// return true if the header should be written
    pub fn check_file(&mut self, file: &InfileContext, fname: &str) -> Result<bool> {
        let first = !self.saw_one;
        if file.has_header {
            self.check(file.header.line.as_bytes(), fname)?;
        } else {
            self.check(b"fake", fname)?;
        }
        Ok(file.has_header && first)
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

/// copy r to w
pub fn copy(mut r: impl Read, mut w: impl Write) -> Result<()> {
    let mut buff = [0u8; 16 * 1024];
    loop {
        let sz = r.read(&mut buff)?;
        if sz == 0 {
            return Ok(());
        }
        w.write_all(&buff[0..sz])?;
    }
}