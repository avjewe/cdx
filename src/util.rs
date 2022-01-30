//! Misc utility stuff

use crate::column::ColumnHeader;
use crate::comp::{CompMaker, Compare, LineCompList};
use crate::text::Text;
use flate2::read::MultiGzDecoder;
use fs_err as fs;
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp::{self, Ordering};
use std::error;
use std::ffi::OsStr;
use std::io::{self, BufRead, Read, Write};
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::str::FromStr;
use std::{fmt, str};
use tokio_stream::StreamExt;

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
#[allow(clippy::large_enum_variant)]
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
    /// pass through DecodeError
    Base64(base64::DecodeError),
    /// pass through SdkError
    MyGetObjectError(aws_sdk_s3::SdkError<aws_sdk_s3::error::GetObjectError>),
    /// bytestream
    ByteStreamError(aws_smithy_http::byte_stream::Error),
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

err_type!(base64::DecodeError, Error::Base64);
err_type!(std::str::Utf8Error, Error::Utf8Error);
err_type!(regex::Error, Error::RegexError);
err_type!(std::string::FromUtf8Error, Error::FromUtf8Error);
err_type!(std::io::Error, Error::IoError);
err_type!(std::num::ParseIntError, Error::ParseIntError);
err_type!(std::num::ParseFloatError, Error::ParseFloatError);
err_type!(aws_smithy_http::byte_stream::Error, Error::ByteStreamError);
err_type!(
    aws_sdk_s3::SdkError<aws_sdk_s3::error::GetObjectError>,
    Error::MyGetObjectError
);

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
            Error::MyGetObjectError(s) => write!(f, "GetObjectError : {}", s)?,
            Error::ByteStreamError(s) => write!(f, "ByteStreamError : {}", s)?,
            Error::Base64(s) => write!(f, "Base64 : {}", s)?,
            Error::NeedLookup => write!(
                f,
                "ColumnSet.lookup() must be called before ColumnSet.select()"
            )?,
            Error::Silent => write!(f, "Silent")?,
        }
        Ok(())
    }
}

/// Yes, No or Maybe
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Tri {
    /// always do the thing
    Yes,
    /// never do the thing
    No,
    /// do the thing under some circumstances
    Maybe,
}
impl Tri {
    /// new from string
    pub fn new(x: &str) -> Result<Self> {
        if x.eq_ignore_ascii_case("yes")
            || x.eq_ignore_ascii_case("true")
            || x.eq_ignore_ascii_case("1")
            || x.eq_ignore_ascii_case("on")
        {
            Ok(Self::Yes)
        } else if x.eq_ignore_ascii_case("no")
            || x.eq_ignore_ascii_case("false")
            || x.eq_ignore_ascii_case("0")
            || x.eq_ignore_ascii_case("off")
        {
            Ok(Self::No)
        } else if x.eq_ignore_ascii_case("maybe") || x.eq_ignore_ascii_case("sometimes") {
            Ok(Self::Maybe)
        } else {
            err!("Tri value must be yes, no or maybe '{}'", x)
        }
    }
}

/// pointers into a vector, simulating a slice without the ownership issues
#[derive(Debug, Clone, Copy, Default)]
pub struct FakeSlice {
    begin: u32,
    end: u32,
}
impl FakeSlice {
    /// new from text, like '7' or '3-6'
    pub fn new(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once('-') {
            let begin = a.to_usize_whole(spec.as_bytes(), "range")? as u32;
            let end = b.to_usize_whole(spec.as_bytes(), "range")? as u32;
            if begin == 0 || end == 0 {
                err!("Invalid range, both number must be greater than zero")
            } else if begin > end {
                err!("Invalid range, begin is greater than end")
            } else {
                Ok(Self {
                    begin: begin - 1,
                    end,
                })
            }
        } else {
            let num = spec.to_usize_whole(spec.as_bytes(), "position")? as u32;
            if num == 0 {
                err!("Invalid offset, must be greater than zero")
            } else {
                Ok(Self {
                    begin: num - 1,
                    end: num,
                })
            }
        }
    }
    /// get slice from FakeSlice
    pub fn get<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[self.begin as usize..self.end as usize]
    }
    /// get slice from FakeSlice, but don't fall off the end.
    pub fn get_safe<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[cmp::min(self.begin as usize, data.len())..cmp::min(self.end as usize, data.len())]
    }
    /// len
    pub const fn len(&self) -> usize {
        (self.end - self.begin) as usize
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
            let mut f = FakeSlice { begin, end };
            if self.line[(end - 1) as usize] == b'\n' {
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

struct S3Reader {
    //    name : String,
    rt: tokio::runtime::Runtime,
    //    client : aws_sdk_s3::Client,
    f: aws_sdk_s3::output::GetObjectOutput,
    left: Option<bytes::Bytes>,
}
impl S3Reader {
    fn new(bucket: &str, key: &str) -> Result<Self> {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
        let shared_config = rt.block_on(aws_config::load_from_env());
        let client = aws_sdk_s3::Client::new(&shared_config);
        let obj = rt.block_on(client.get_object().bucket(bucket).key(key).send())?;
        Ok(Self {
            //	    name : key.to_string(),
            rt,
            //	    client,
            f: obj,
            left: None,
        })
    }
    fn new_path(spec: &str) -> Result<Self> {
        if let Some(name) = spec.strip_prefix("s3://") {
            if let Some((a, b)) = name.split_once('/') {
                Self::new(a, b)
            } else {
                err!("Not an S3 file spec '{}'", spec)
            }
        } else {
            err!("Not an S3 file '{}'", spec)
        }
    }
}
impl Read for S3Reader {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, std::io::Error> {
        if let Some(bytes) = &self.left {
            if bytes.len() > buf.len() {
                buf.clone_from_slice(&bytes[..buf.len()]);
                self.left = Some(bytes.slice(buf.len()..));
                return Ok(buf.len());
            } else {
                let len = bytes.len();
                buf[0..len].clone_from_slice(bytes);
                self.left = None;
                return Ok(len);
            }
        }
        let bytes_res = self.rt.block_on(self.f.body.try_next());
        if bytes_res.is_err() {
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no"));
        }
        self.left = bytes_res.unwrap();
        if let Some(bytes) = &self.left {
            if bytes.len() > buf.len() {
                buf.clone_from_slice(&bytes[..buf.len()]);
                self.left = Some(bytes.slice(buf.len()..));
                Ok(buf.len())
            } else {
                let len = bytes.len();
                buf[0..bytes.len()].clone_from_slice(bytes);
                self.left = None;
                Ok(len)
            }
        } else {
            Ok(0)
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
pub fn get_reader(name: &str) -> Result<Infile> {
    let inner: Box<dyn Read> = {
        if name == "-" {
            //	    unsafe { Box::new(std::fs::File::from_raw_fd(1)) }
            Box::new(io::stdin())
        } else if name.starts_with("s3://") {
            Box::new(S3Reader::new_path(name)?)
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
struct InfileContext {
    // CDX header, contructed if necessary
    header: StringLine,
    // delimter
    delim: u8,
    // have we read all the btes of the file
    is_done: bool,
    // is the file length zero
    is_empty: bool,
    // was there a CDX header?
    has_header: bool,
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

/// where are we, in which file?
#[derive(Debug, Default, Clone)]
pub struct FileLocData {
    /// full file name
    name: String,
    /// byte offset, uncompressed
    bytes: usize,
    /// line number
    line: usize,
}

/// types of file location data
#[derive(Debug, Copy, Clone)]
enum FileLocItem {
    /// byte offset of start of line
    Bytes,
    /// 1-based line number
    Line,
    /// file name, with given number of parts
    Name(usize),
}
impl FileLocItem {
    fn new(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("bytes") {
            Ok(Self::Bytes)
        } else if spec.eq_ignore_ascii_case("line") {
            Ok(Self::Line)
        } else if spec.eq_ignore_ascii_case("name") {
            Ok(Self::Name(0))
        } else if let Some((a, b)) = spec.split_once('.') {
            if a.eq_ignore_ascii_case("name") {
                Ok(Self::Name(
                    b.to_usize_whole(spec.as_bytes(), "File location")?,
                ))
            } else {
                err!("File Loc must be once of Bytes, Line, Name : '{}'", spec)
            }
        } else {
            err!("File Loc must be once of Bytes, Line, Name : '{}'", spec)
        }
    }
    const fn dflt_name(&self) -> &'static str {
        match self {
            Self::Bytes => "bytes",
            Self::Line => "line",
            Self::Name(_) => "filename",
        }
    }
    fn write_data(&mut self, data: &mut impl Write, loc: &FileLocData) -> Result<()> {
        match self {
            Self::Bytes => write!(data, "{}", loc.bytes).unwrap(),
            Self::Line => write!(data, "{}", loc.line).unwrap(),
            Self::Name(n) => {
                if *n == 0 {
                    data.write_all(loc.name.as_bytes())?;
                } else {
                    // FIXME - this should really be cached somehow
                    data.write_all(loc.name.tail_path_u8(*n, b'/').as_bytes())?;
                }
            }
        }
        Ok(())
    }
}

/// FileLocItem with column name
#[derive(Debug, Clone)]
struct FileLoc {
    col_name: String,
    item: FileLocItem,
}
impl FileLoc {
    fn new(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(':') {
            Ok(Self {
                col_name: a.to_string(),
                item: FileLocItem::new(b)?,
            })
        } else {
            let item = FileLocItem::new(spec)?;
            Ok(Self {
                col_name: item.dflt_name().to_string(),
                item,
            })
        }
    }
    fn write_data(&mut self, data: &mut impl Write, loc: &FileLocData) -> Result<()> {
        self.item.write_data(data, loc)
    }
}

/// List of FileLoc
#[derive(Default, Debug, Clone)]
pub struct FileLocList {
    v: Vec<FileLoc>,
}
impl FileLocList {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// new
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    /// add Name:Spec
    pub fn push(&mut self, spec: &str) -> Result<()> {
        for x in spec.split(',') {
            self.v.push(FileLoc::new(x)?);
        }
        Ok(())
    }
    /// fill data with file loc data
    pub fn write_data(
        &mut self,
        data: &mut impl Write,
        delim: u8,
        loc: &FileLocData,
    ) -> Result<()> {
        for x in &mut self.v {
            x.write_data(data, loc)?;
            data.write_all(&[delim])?;
        }
        Ok(())
    }
    /// fill data with column names
    pub fn write_names(&mut self, data: &mut String, delim: u8) {
        for x in &mut self.v {
            data.push_str(&x.col_name);
            data.push(delim as char);
        }
    }
    /// add new columns to header
    pub fn add(&self, header: &mut ColumnHeader) -> Result<()> {
        for x in &self.v {
            header.push(&x.col_name)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
/// File reader for text file broken into lines with columns
/// previous N lines are still available
pub struct Reader {
    file: Infile,
    lines: Vec<TextLine>,
    cont: InfileContext,
    do_split: bool,
    curr: usize,
    loc: FileLocData,
}

impl Reader {
    /// loc
    pub const fn loc(&self) -> &FileLocData {
        &self.loc
    }
    /// make a new Reader
    pub fn new() -> Self {
        Self::new_with(1)
    }
    /// set to false to skip breaking into columns
    pub fn do_split(&mut self, val: bool) {
        self.do_split = val;
    }
    /// make a new Reader, with explicit lookback
    pub fn new_with(lookback: usize) -> Self {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        Self {
            file: Infile::default(),
            lines,
            cont: InfileContext::new(),
            do_split: true,
            curr: 0,
            loc: FileLocData::default(),
        }
    }
    /// make a new Reader
    pub fn new_open(name: &str) -> Result<Self> {
        Self::new_open_with(name, 1)
    }
    /// make a new Reader
    pub fn new_open_with(name: &str, lookback: usize) -> Result<Self> {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        let mut tmp = Self {
            file: get_reader(name)?,
            lines,
            cont: InfileContext::new(),
            do_split: true,
            curr: 0,
            loc: FileLocData::default(),
        };
        tmp.cont.read_header(&mut *tmp.file, &mut tmp.lines[0])?;
        tmp.loc.name = name.to_string();
        tmp.loc.line = 1;
        tmp.loc.bytes = if tmp.has_header() {
            tmp.header().line.len()
        } else {
            0
        };
        Ok(tmp)
    }
    /// get current line contents, without the trailing newline
    pub fn curr_nl(&self) -> &[u8] {
        let line = self.curr_line();
        &line.line[0..line.line.len() - 1]
    }
    /// get previous line contents, without the trailing newline
    pub fn prev_nl(&self, n: usize) -> &[u8] {
        let line = self.prev_line(n);
        &line.line[0..line.line.len() - 1]
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
        self.loc.line
    }
    fn incr(&mut self) {
        self.loc.line += 1;
        self.curr += 1;
        if self.curr >= self.lines.len() {
            self.curr = 0;
        }
    }
    /// get next line of text
    pub fn getline(&mut self) -> Result<bool> {
        self.loc.bytes += self.curr().line.len();
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
    pub fn curr_mut(&mut self) -> &mut TextLine {
        &mut self.lines[self.curr]
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

/// print a bunch of u8 to stderr, adding a newline
pub fn prerr(data: &[&[u8]]) {
    for x in data {
        std::io::stderr().write_all(x).unwrap();
    }
    std::io::stderr().write_all(b"\n").unwrap();
}
/// print a bunch of u8 to stderr
pub fn prerr_n(data: &[&[u8]]) {
    for x in data {
        std::io::stderr().write_all(x).unwrap();
    }
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

impl FromStr for HeaderMode {
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
    pub fn check_file(&mut self, file: &Reader, fname: &str) -> Result<bool> {
        let first = !self.saw_one;
        if file.has_header() {
            self.check(file.header().line.as_bytes(), fname)?;
        } else {
            self.check(b"fake", fname)?;
        }
        Ok(file.has_header() && first)
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

/// comparison
#[derive(Debug, Copy, Clone)]
pub enum CompareOp {
    /// les than
    LT,
    /// greater than
    GT,
    /// less than or equal
    LE,
    /// greater than or equal
    GE,
    /// equal
    EQ,
    /// not equal
    NE,
}
impl Default for CompareOp {
    fn default() -> Self {
        Self::LT
    }
}

impl FromStr for CompareOp {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        if s.eq_ignore_ascii_case("lt") {
            Ok(Self::LT)
        } else if s.eq_ignore_ascii_case("gt") {
            Ok(Self::GT)
        } else if s.eq_ignore_ascii_case("le") {
            Ok(Self::LE)
        } else if s.eq_ignore_ascii_case("ge") {
            Ok(Self::GE)
        } else if s.eq_ignore_ascii_case("eq") {
            Ok(Self::EQ)
        } else if s.eq_ignore_ascii_case("ne") {
            Ok(Self::NE)
        } else if s.eq_ignore_ascii_case("<") {
            Ok(Self::LT)
        } else if s.eq_ignore_ascii_case(">") {
            Ok(Self::GT)
        } else if s.eq_ignore_ascii_case("<=") {
            Ok(Self::LE)
        } else if s.eq_ignore_ascii_case(">=") {
            Ok(Self::GE)
        } else if s.eq_ignore_ascii_case("==") {
            Ok(Self::EQ)
        } else if s.eq_ignore_ascii_case("!=") {
            Ok(Self::NE)
        } else {
            err!(
                "Invalid CompareOp, should be one of LT,<,GT,>,LE,<=,GE,>=,EQ,==,NE,!= : '{}'",
                s
            )
        }
    }
}

impl CompareOp {
    /// invert left vs right. That is, swap less vs greater, but not equal vs not equal
    pub const fn invert(&self) -> Self {
        use CompareOp::*;
        match self {
            LT => GT,
            GT => LT,
            LE => GE,
            GE => LE,
            EQ => EQ,
            NE => NE,
        }
    }
    /// return (line OP value), writing to stderr if false
    pub fn line_ok_verbose(
        &self,
        line: &TextLine,
        comp: &mut LineCompList,
        line_num: usize,
    ) -> bool {
        if !self.invert().line_ok(line, comp) {
            eprint!("Line {} : ", line_num);
            prerr_n(&[&line.line]);
            eprint!("should have been {:?} ", self);
            prerr_n(&[comp.get_value()]);
            eprintln!(" but wasn't");
            false
        } else {
            true
        }
    }
    /// return (line OP value)
    pub fn line_ok(&self, line: &TextLine, comp: &mut LineCompList) -> bool {
        let o = comp.comp_self_cols(line);
        self.ord_ok(o)
    }
    /// does Ordering satisfy CompareOp
    pub fn ord_ok(&self, o: Ordering) -> bool {
        use CompareOp::*;
        use Ordering::*;
        match self {
            LT => o == Less,
            GT => o == Greater,
            LE => o != Greater,
            GE => o != Less,
            EQ => o == Equal,
            NE => o != Equal,
        }
    }
}

struct RangeSpec<'a> {
    op1: &'a str,
    val1: &'a str,
    op2: Option<&'a str>,
    val2: Option<&'a str>,
}

impl<'a> RangeSpec<'a> {
    fn new(spec: &'a str) -> Result<Self> {
        lazy_static! {
            static ref RE1: Regex =
                Regex::new("^(<|>|<=|>=|==|!=)([^<>!=]+)(<|>|<=|>=|==|!=)(.+)$").unwrap();
            static ref RE2: Regex = Regex::new("^(<|>|<=|>=|==|!=)(.+)$").unwrap();
            static ref RE3: Regex =
                Regex::new("^(LT|GT|LE|GE|EQ|NE),([^,]+),(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap();
            static ref RE4: Regex = Regex::new("^(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap();
        }
        if let Some(caps) = RE1.captures(spec) {
            Ok(Self {
                op1: caps.get(1).unwrap().as_str(),
                val1: caps.get(2).unwrap().as_str(),
                op2: Some(caps.get(3).unwrap().as_str()),
                val2: Some(caps.get(4).unwrap().as_str()),
            })
        } else if let Some(caps) = RE2.captures(spec) {
            Ok(Self {
                op1: caps.get(1).unwrap().as_str(),
                val1: caps.get(2).unwrap().as_str(),
                op2: None,
                val2: None,
            })
        } else if let Some(caps) = RE3.captures(spec) {
            Ok(Self {
                op1: caps.get(1).unwrap().as_str(),
                val1: caps.get(2).unwrap().as_str(),
                op2: Some(caps.get(3).unwrap().as_str()),
                val2: Some(caps.get(4).unwrap().as_str()),
            })
        } else if let Some(caps) = RE4.captures(spec) {
            Ok(Self {
                op1: caps.get(1).unwrap().as_str(),
                val1: caps.get(2).unwrap().as_str(),
                op2: None,
                val2: None,
            })
        } else {
            err!("Not valid range spec '{}'", spec)
        }
    }
    fn new_trail(spec: &'a str) -> Result<(Self, usize)> {
        lazy_static! {
            static ref RE1: Regex =
                Regex::new("(^|,)(<|>|<=|>=|==|!=)([^<>!=]+)(<|>|<=|>=|==|!=)([^=].*)$").unwrap();
            static ref RE2: Regex = Regex::new("(^|,)(<|>|<=|>=|==|!=)([^=].*)$").unwrap();
            static ref RE3: Regex =
                Regex::new("(^|,)(LT|GT|LE|GE|EQ|NE),([^,]+),(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap();
            static ref RE4: Regex = Regex::new("(^|,)(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap();
        }
        if let Some(caps) = RE1.captures(spec) {
            Ok((
                Self {
                    op1: caps.get(2).unwrap().as_str(),
                    val1: caps.get(3).unwrap().as_str(),
                    op2: Some(caps.get(4).unwrap().as_str()),
                    val2: Some(caps.get(5).unwrap().as_str()),
                },
                caps.get(0).unwrap().start(),
            ))
        } else if let Some(caps) = RE2.captures(spec) {
            Ok((
                Self {
                    op1: caps.get(2).unwrap().as_str(),
                    val1: caps.get(3).unwrap().as_str(),
                    op2: None,
                    val2: None,
                },
                caps.get(0).unwrap().start(),
            ))
        } else if let Some(caps) = RE3.captures(spec) {
            Ok((
                Self {
                    op1: caps.get(2).unwrap().as_str(),
                    val1: caps.get(3).unwrap().as_str(),
                    op2: Some(caps.get(4).unwrap().as_str()),
                    val2: Some(caps.get(5).unwrap().as_str()),
                },
                caps.get(0).unwrap().start(),
            ))
        } else if let Some(caps) = RE4.captures(spec) {
            Ok((
                Self {
                    op1: caps.get(2).unwrap().as_str(),
                    val1: caps.get(3).unwrap().as_str(),
                    op2: None,
                    val2: None,
                },
                caps.get(0).unwrap().start(),
            ))
        } else {
            err!("Not valid trailing range spec '{}'", spec)
        }
    }
}

#[derive(Debug, Default)]
/// A compare op with some text to compare against
pub struct CheckLine {
    op: CompareOp,
    val: String,
    op2: Option<CompareOp>,
    val2: Option<String>,
}

impl CheckLine {
    /// new from spec : OP,Text
    pub fn new(spec: &str) -> Result<Self> {
        let mut s = Self::default();
        s.set(spec)?;
        Ok(s)
    }
    /// new from parts
    fn set_with(&mut self, r: &RangeSpec<'_>) -> Result<()> {
        self.op = r.op1.parse::<CompareOp>()?;
        self.val = r.val1.to_owned();
        self.op2 = if r.op2.is_none() {
            None
        } else {
            Some(r.op2.unwrap().parse::<CompareOp>()?)
        };
        self.val2 = if r.val2.is_none() {
            None
        } else {
            Some(r.op2.unwrap().to_owned())
        };
        Ok(())
    }
    /// set from spec OP,Text
    pub fn set(&mut self, spec: &str) -> Result<()> {
        self.set_with(&RangeSpec::new(spec)?)
    }
    /// compare line OP text, return true if match, print to stderr if non-mtch
    pub fn line_ok_verbose(
        &self,
        line: &TextLine,
        comp: &mut LineCompList,
        line_num: usize,
    ) -> Result<bool> {
        comp.set(self.val.as_bytes(), b',')?;
        Ok(self.op.line_ok_verbose(line, comp, line_num))
    }
    /// compare line OP text, return true if match
    pub fn line_ok(&self, line: &TextLine, comp: &mut LineCompList) -> Result<bool> {
        comp.set(self.val.as_bytes(), b',')?;
        let ret = self.op.line_ok(line, comp);
        if !ret {
            return Ok(false);
        }
        match self.op2 {
            None => Ok(true),
            Some(o) => {
                comp.set(self.val2.as_ref().unwrap().as_bytes(), b',')?;
                Ok(o.line_ok(line, comp))
            }
        }
    }
}

// range,Comparator,RangeThing

/// A compare op with some text to compare against
pub struct CheckBuff {
    op: CompareOp,
    val: Box<dyn Compare>,
    op2: Option<CompareOp>,
    val2: Option<Box<dyn Compare>>,
}
impl fmt::Debug for CheckBuff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CheckBuff")
    }
}

impl CheckBuff {
    /// new from spec : OP,Text
    pub fn new(spec: &str) -> Result<Self> {
        let (range, bytes) = RangeSpec::new_trail(spec)?;
        Self::new_with(&spec[..bytes], &range)
    }
    /// new from parts
    fn new_with(comp_spec: &str, r: &RangeSpec<'_>) -> Result<Self> {
        Ok(Self {
            op: r.op1.parse::<CompareOp>()?,
            val: {
                let mut c = CompMaker::make_comp_box(comp_spec)?;
                c.set(r.val1.as_bytes());
                c
            },
            op2: if r.op2.is_none() {
                None
            } else {
                Some(r.op2.unwrap().parse::<CompareOp>()?)
            },
            val2: if r.val2.is_none() {
                None
            } else {
                let mut c = CompMaker::make_comp_box(comp_spec)?;
                c.set(r.val2.unwrap().as_bytes());
                Some(c)
            },
        })
    }
    /// does right match?
    pub fn buff_ok(&self, right: &[u8]) -> bool {
        let c = self.val.comp_self(right);
        if !self.op.invert().ord_ok(c) {
            return false;
        }
        match self.op2 {
            None => true,
            Some(o) => {
                let c = self.val2.as_ref().unwrap().comp_self(right);
                o.invert().ord_ok(c)
            }
        }
    }
}

/// find the closing marker, e.g return ')' for '('
pub fn closer(ch: u8) -> Result<u8> {
    match ch {
        b'(' => Ok(b')'),
        b'{' => Ok(b'}'),
        b'[' => Ok(b']'),
        b'<' => Ok(b'>'),
        _ => err!("I don't know how to cloase a '{}'", ch as char),
    }
}

/// return the length of the enclosed string, allowing for nested markers
pub fn find_close(spec: &str) -> Result<usize> {
    if spec.is_empty() {
        return err!("Can't find_close an empty string");
    }
    let bspec = spec.as_bytes();
    let open = bspec[0];
    let close = closer(open)?;
    let mut depth = 0;
    for (i, x) in bspec.iter().enumerate() {
        if *x == open {
            depth += 1;
        } else if *x == close {
            depth -= 1;
        }
        if depth == 0 {
            return Ok(i + 1);
        }
    }
    err!("Had no closing delimiter '{}'", spec)
}

/// remove trailing end of line characters
pub fn chomp(mut x: &[u8]) -> &[u8] {
    while !x.is_empty() {
        let len = x.len() - 1;
        if x[len] != b'\n' && x[len] != b'\r' {
            break;
        } else {
            x = &x[..len];
        }
    }
    x
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn markers() {
        let c = closer(b'(');
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), b')');
        let c = find_close("(abc)");
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), 5);
        let c = find_close("[abc][def]");
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), 5);
        let c = find_close("{abc{def}}");
        assert!(c.is_ok());
        assert_eq!(c.unwrap(), 10);
        let c = find_close("{abc{def}ghi");
        assert!(c.is_err());
    }
}
