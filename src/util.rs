//! Misc utility stuff
use crate::comp::CompMaker;
use crate::prelude::*;
use crate::*;
use anyhow;
use flate2::read::MultiGzDecoder;
use fs_err as fs;
use regex::Regex;
use std::cmp;
use std::error;
use std::io;
use std::ops::{Deref, DerefMut};
use std::str;
use std::sync::LazyLock;

fn do_init() -> Result<()> {
    agg::AggMaker::init()?;
    CompMaker::init()?;
    matcher::MatchMaker::init()?;
    textgen::GenMaker::init()?;
    trans::TransMaker::init()?;
    Ok(())
}

static INIT: LazyLock<core::result::Result<(), String>> = LazyLock::new(|| {
    do_init().map_err(|e| format!("{e:?}"))?;
    Ok(())
});

/// Call before anything else. Automatically done in `main`
pub fn init() -> Result<()> {
    match INIT.as_ref() {
        Ok(()) => Ok(()),
        Err(e) => err!(e),
    }
}

/// Shorthand for returning an error Result
#[macro_export]
macro_rules! err {
    ($($x:tt)*) => {Err(anyhow!($($x)*))}
}
pub use err;

/// Common CDX error type
#[derive(Debug)]
#[non_exhaustive]
pub enum CdxError {
    /// Custom cdx error
    Error(String),
    /// common error with long default string
    NeedLookup,
    /// be an error, but don't report anything
    Silent,
    /// Exit with status code zero
    NoError,
}

/// Common CDX error type
pub type Error = anyhow::Error;
/// Common CDX result type
pub type Result<T> = core::result::Result<T, Error>;
impl error::Error for CdxError {}

/// convert `CdxError` to Result
pub fn cdx_err<T>(err: CdxError) -> Result<T> {
    Err(Error::new(err))
}

/// if suppress(e) is true, treat as success
#[must_use]
pub fn suppress(err: &Error) -> bool {
    if let Some(ioerr) = err.downcast_ref::<io::Error>() {
        ioerr.kind() == io::ErrorKind::BrokenPipe
    } else {
        matches!(err.downcast_ref::<CdxError>(), Some(CdxError::NoError))
    }
}

/// if silent(e) is true, treat as failure, but print no message
#[must_use]
pub fn silent(err: &Error) -> bool {
    matches!(err.downcast_ref::<CdxError>(), Some(CdxError::Silent))
}

impl fmt::Display for CdxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(s) => write!(f, "{s}")?,
            Self::NeedLookup => {
                write!(f, "ColumnSet.lookup() must be called before ColumnSet.select()")?;
            }
            Self::Silent => write!(f, "Silent")?,
            Self::NoError => write!(f, "Not an error.")?,
        }
        Ok(())
    }
}

/// Yes, No or Maybe
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    /// return Yes is x is true, else Maybe
    #[must_use]
    pub const fn yes_if(x: bool) -> Self {
        if x { Self::Yes } else { Self::Maybe }
    }
    /// return No if x is true, else Maybe
    #[must_use]
    pub const fn no_if(x: bool) -> Self {
        if x { Self::No } else { Self::Maybe }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
/// DO we have a header line?
pub enum HeadMode {
    /// has header
    Yes,
    /// does not have header
    No,
    /// check for cdx header
    Maybe,
    /// discard cdx header, if present
    Skip,
    /// has a CDX header
    Cdx,
}
impl HeadMode {
    /// does this mode imply a CDX header
    #[must_use]
    pub fn has_cdx(self) -> bool {
        self == Self::Cdx || self == Self::Maybe
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
/// how are column values escaped?
pub enum QuoteMode {
    /// no escaping
    Plain,
    /// double quotes
    Quote,
    /// backslash escaped
    Backslash,
}

// whitespace delimited
// double quotes
// single quotes
// square brackets
// curly bracket
// angle brackets
// explicit quotes, e.g. A...B or A...A
// parens
// nested ?? e.g. ab"cd(ef" means what?
// brackets only recognized at ends of values
// --dx (whole line one column)

/// convert char to u8, but 't' means tab and such
#[must_use]
pub const fn auto_escape(ch: char) -> u8 {
    if ch == 't' {
        b'\t'
    } else if ch == 's' {
        b' '
    } else if ch == 'r' {
        b'\r'
    } else if ch == 'n' {
        b'\n'
    }
    // FIXME - more auto-escape options
    else {
        ch as u8
    }
}

/// usually return input char, but 't' means tab and such
#[must_use]
pub const fn auto_escape_char(ch: char) -> char {
    if ch == 't' {
        '\t'
    } else if ch == 's' {
        ' '
    } else if ch == 'r' {
        '\r'
    } else if ch == 'n' {
        '\n'
    }
    // FIXME - more auto-escape options
    else {
        ch
    }
}

const TEXT_HELP_STRING: &str = r#"

# Input Text Files #

All tools have a --text-in option, that takes a string of up to five characters
which defines how text files are parsed.

The first character is the header mode, which can be
    m - maybe (default) : Use the CDX header if it exists, otherwise don't.
    y - yes : A CDX header is required
    n - no : A CDX header is forbidden
    s - skip : A CDX header, if present, is ignored.

The second character is the column delimiter, auto-escaped
    default is 't' for 'tab'

The third character is the quoting mode
    p - plain (default) : no quoting is done. It is impossible to have a data field
                          containing the column delimiter.
    q - quote : The column delimiter is ignored when between double quotes : '"'
                Within a quoted sting, '""' is used to mean '"'
    b - backslash : All backslashes are combined with the following character in the usual way.

The fourth character is the replacement character, used when the quote mode is 'plain'.
Any occurrences of the column delimiter are replaced with this character.
    default is space.

The fifth character is the line delimiter, default is newline (\n)

Omitted trailing characters accept the default, so 'y' is the same as 'ytpsn'

# Output Text Files #

All tools have a --text-out option that controls how text files are written.
Its parameter is treated exactly like --text-in, except that the default values
are those passed to --text-in.
If --text-in specified "maybe" as the header mode, then the output default is
'yes' if an input header was present and 'no' otherwise.

# Auto Escaped #
    Auto escaping make it easier to specify special characters,
    mostly by implying a leading backslash.
    Auto escaping maps letters to other characters :
    t is tab
    s is space
    r is return (/r)
    n is newline (/n)
    other lowercase ascii is reserved
    otherwise the character is unchanged.
"#;

#[derive(Debug, Copy, Clone)]
/// how to parse a text file into lines and columns
pub struct TextFileMode {
    /// yes, no, maybe
    pub head_mode: HeadMode,
    /// plain, quote, backslash
    pub col_mode: QuoteMode,
    /// column delimiter
    pub delim: u8,
    /// line delimiter
    pub line_break: u8,
    /// replacement character for plain encoding
    // maybe should be Option<u8> where None means to drop, rather than replace
    pub repl: u8,
}
impl Default for TextFileMode {
    fn default() -> Self {
        Self {
            head_mode: HeadMode::Maybe,
            col_mode: QuoteMode::Plain,
            delim: b'\t',
            line_break: b'\n',
            repl: b' ',
        }
    }
}
impl TextFileMode {
    /// new from spec
    pub fn new(spec: &str) -> Result<Self> {
        Self::new_with(spec, &Self::default())
    }
    /// new from spec, with defaults
    pub fn new_with(spec: &str, dflt: &Self) -> Result<Self> {
        let mut spec = spec;
        let mut head_mode = dflt.head_mode;
        let mut col_mode = dflt.col_mode;
        let mut delim = dflt.delim;
        let mut line_break = dflt.line_break;
        let mut repl = dflt.repl;

        // 1st char : header mode. y - yes, n - no, s - skip, m - maybe, x - cdx
        if !spec.is_empty() {
            let ch = spec.take_first();
            if ch == 'y' {
                head_mode = HeadMode::Yes;
            } else if ch == 'n' {
                head_mode = HeadMode::No;
            } else if ch == 's' {
                head_mode = HeadMode::Skip;
            } else if ch == 'm' {
                head_mode = HeadMode::Maybe;
            } else if ch == 'x' {
                head_mode = HeadMode::Cdx;
            } else {
                return err!(
                    "First char of text-fmt spec must be y (yes), n (no), m (maybe) s (skip) or x (cdx) not '{ch}'"
                );
            }
        }
        // 2nd char : delimiter.
        if !spec.is_empty() {
            let ch = spec.take_first();
            delim = auto_escape(ch);
        }
        // 3rd char : quoting. p - plain, q - quote, b - backslash
        if !spec.is_empty() {
            let ch = spec.take_first();
            if ch == 'p' {
                col_mode = QuoteMode::Plain;
            } else if ch == 'q' {
                col_mode = QuoteMode::Quote;
            } else if ch == 'b' {
                col_mode = QuoteMode::Backslash;
            } else {
                return err!(
                    "Fourth char of text-fmt spec must be p (plain), q (quote) or b (backslash) not '{ch}'"
                );
            }
        }
        // 4th character : repl for plain encoding
        if !spec.is_empty() {
            let ch = spec.take_first();
            repl = auto_escape(ch);
        }
        // 5th char : end of line
        if !spec.is_empty() {
            let ch = spec.take_first();
            line_break = auto_escape(ch);
        }
        Ok(Self { head_mode, col_mode, delim, line_break, repl })
    }

    /// print the help for `TextFileMode`
    pub fn text_help() {
        println!("{TEXT_HELP_STRING}");
    }

    /// write a column value, properly escaped
    /// print help for text modes
    pub fn write(&self, w: &mut dyn Write, buf: &[u8]) -> Result<()> {
        match self.col_mode {
            QuoteMode::Plain => write_plain(w, buf, self.delim, self.line_break, self.repl),
            QuoteMode::Backslash => write_backslash(w, buf, self.delim, self.line_break),
            QuoteMode::Quote => write_quotes(w, buf, self.delim, self.line_break),
        }
    }

    /// append a newline, if line does not already end with one. Return false.
    pub fn ensure_eof(&self, line: &mut Vec<u8>) -> Result<bool> {
        if line[line.len() - 1] != self.line_break {
            line.push(self.line_break);
        }
        Ok(false)
    }
}

/*
CDX mode yes/no (cx)
if cdx mode, yes,no,maybe,skip (ynms)
else yes or no (yn)
delim : auto escape stnl
quoting : omit, quote, backslash (oqb)
line delimiter auto escape stnr, C for crlf

default is cmton
regular csv is xy,qC
regular tsv is xytbn
plain tsv xy
cdx.maybe.Ct.literal.Ln
--text-in
 */

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
                Ok(Self { begin: begin - 1, end })
            }
        } else {
            let num = spec.to_usize_whole(spec.as_bytes(), "position")? as u32;
            if num == 0 {
                err!("Invalid offset, must be greater than zero")
            } else {
                Ok(Self { begin: num - 1, end: num })
            }
        }
    }
    /// get slice from `FakeSlice`
    #[must_use]
    pub fn get<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[self.begin as usize..self.end as usize]
    }
    /// get slice from `FakeSlice`, but don't fall off the end.
    #[must_use]
    pub fn get_safe<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[cmp::min(self.begin as usize, data.len())..cmp::min(self.end as usize, data.len())]
    }
    /// len
    #[must_use]
    pub const fn len(&self) -> usize {
        (self.end - self.begin) as usize
    }
    /// is empty?
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.begin == self.end
    }
}

fn write_plain(w: &mut dyn Write, buf: &[u8], tab: u8, eol: u8, repl: u8) -> Result<()> {
    for ch in buf {
        if *ch == tab || *ch == eol {
            w.write_all(&[repl])?;
        } else {
            w.write_all(&[*ch])?;
        }
    }
    Ok(())
}

fn write_backslash(w: &mut dyn Write, buf: &[u8], tab: u8, eol: u8) -> Result<()> {
    for ch in buf {
        if *ch == tab || *ch == eol || *ch == b'\\' {
            w.write_all(b"\\")?;
            w.write_all(&[enslash(*ch)])?;
        } else {
            w.write_all(&[*ch])?;
        }
    }
    Ok(())
}

fn write_quotes(w: &mut dyn Write, buf: &[u8], tab: u8, eol: u8) -> Result<()> {
    let mut made_quote = false;
    for ch in buf {
        if *ch == b'"' {
            w.write_all(b"\"\"")?;
            continue;
        }
        if !made_quote && (*ch == tab || *ch == eol) {
            made_quote = true;
            w.write_all(b"\"")?;
        }
        w.write_all(&[*ch])?;
    }
    Ok(())
}

/// undo slash encoding, e.g. 'n' becomes '\n'
#[must_use]
pub const fn unslash(ch: u8) -> u8 {
    match ch {
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        _ => ch,
    }
}

/// do slash encoding, e.g. 'n' becomes '\n'
#[must_use]
pub const fn enslash(ch: u8) -> u8 {
    match ch {
        b'\n' => b'n',
        b'\r' => b'r',
        b'\t' => b't',
        _ => ch,
    }
}

/// write a buffer. If non-empty, ensure trailing newline
pub fn write_all_nl(w: &mut impl Write, buf: &[u8]) -> Result<()> {
    if !buf.is_empty() {
        w.write_all(buf)?;
        if buf[buf.len() - 1] != b'\n' {
            w.write_all(b"\n")?;
        }
    }
    Ok(())
}

#[cfg(feature = "s3")]
struct S3Reader {
    //    name : String,
    rt: tokio::runtime::Runtime,
    //    client : aws_sdk_s3::Client,
    f: aws_sdk_s3::operation::get_object::GetObjectOutput,
    left: Option<bytes::Bytes>,
}
#[cfg(feature = "s3")]
impl S3Reader {
    fn new(bucket: &str, key: &str) -> Result<Self> {
        let rt = tokio::runtime::Builder::new_current_thread().enable_all().build()?;
        let version = aws_config::BehaviorVersion::latest();
        let shared_config = rt.block_on(aws_config::load_defaults(version));
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

#[cfg(feature = "s3")]
impl Read for S3Reader {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, io::Error> {
        if let Some(bytes) = &self.left {
            return if bytes.len() > buf.len() {
                buf.clone_from_slice(&bytes[..buf.len()]);
                self.left = Some(bytes.slice(buf.len()..));
                Ok(buf.len())
            } else {
                let len = bytes.len();
                buf[0..len].clone_from_slice(bytes);
                self.left = None;
                Ok(len)
            };
        }
        let bytes_res = self.rt.block_on(self.f.body.try_next());
        if bytes_res.is_err() {
            return Err(io::Error::other("oh no"));
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

/// Necessary functionality for writers
pub trait MyRead: Read + Send + Sync + fmt::Debug {}
impl<T> MyRead for T where T: Read + Send + Sync + fmt::Debug {}

/// Input file. Wrapped in a type, so I can 'impl Debug'
#[derive(Debug)]
pub struct Infile(
    /// The file being read
    pub io::BufReader<Box<dyn MyRead>>,
    pub String,
);

impl Infile {
    /// create a new input file
    #[must_use]
    pub fn new(f: io::BufReader<Box<dyn MyRead>>, n: &str) -> Self {
        Self(f, n.to_string())
    }
    /// Build an `InFile` from any readable source using default buffering.
    #[must_use]
    pub fn from_reader<R>(reader: R, source_name: impl Into<String>) -> Self
    where
        R: MyRead + 'static,
    {
        Self(io::BufReader::new(Box::new(reader)), source_name.into())
    }
    /// Build an `InFile` from any readable source with an explicit buffer capacity.
    #[must_use]
    pub fn with_capacity<R>(capacity: usize, reader: R, source_name: impl Into<String>) -> Self
    where
        R: MyRead + 'static,
    {
        Self(io::BufReader::with_capacity(capacity, Box::new(reader)), source_name.into())
    }
}

impl Default for Infile {
    fn default() -> Self {
        Self::new(io::BufReader::new(Box::new(io::empty())), "")
    }
}

impl Deref for Infile {
    type Target = io::BufReader<Box<dyn MyRead>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Infile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Necessary functionality for writers
pub trait MyWrite: Write + Send + Sync + fmt::Debug {}
impl<T> MyWrite for T where T: Write + Send + Sync + fmt::Debug {}

/// output file type
#[derive(Debug)]
pub struct Outfile(pub io::BufWriter<Box<dyn MyWrite>>, pub String);

impl Outfile {
    /// create a new input file
    #[must_use]
    pub fn new(f: io::BufWriter<Box<dyn MyWrite>>, n: &str) -> Self {
        Self(f, n.to_string())
    }
}

impl Default for Outfile {
    fn default() -> Self {
        Self::new(io::BufWriter::new(Box::new(io::sink())), "")
    }
}

impl Deref for Outfile {
    type Target = io::BufWriter<Box<dyn MyWrite>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Outfile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<io::BufWriter<Box<dyn MyWrite>>> for Outfile {
    fn as_ref(&self) -> &io::BufWriter<Box<dyn MyWrite>> {
        &self.0
    }
}

impl AsMut<io::BufWriter<Box<dyn MyWrite>>> for Outfile {
    fn as_mut(&mut self) -> &mut io::BufWriter<Box<dyn MyWrite>> {
        &mut self.0
    }
}

/// Make an Outfile from a file name
pub fn get_writer(name: &str) -> Result<Outfile> {
    let inner: Box<dyn MyWrite> = {
        if name == "-" {
            Box::new(io::stdout())
        } else if name == "--" {
            Box::new(io::stderr())
        } else {
            Box::new(fs::OpenOptions::new().write(true).create(true).open(name)?)
        }
    };
    Ok(Outfile::new(io::BufWriter::new(inner), name))
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

#[cfg(feature = "s3")]
fn new_s3_reader(name: &str) -> Result<Box<dyn MyRead>> {
    Ok(Box::new(S3Reader::new_path(name)?))
}
#[cfg(not(feature = "s3"))]
fn new_s3_reader(name: &str) -> Result<Box<dyn MyRead>> {
    err!("S3 feature not enabled, cannot read '{}'", name)
}

/// Make an Infile from a file name
pub fn get_reader(name: &str) -> Result<Infile> {
    let inner: Box<dyn MyRead> = {
        if name == "-" {
            Box::new(io::stdin())
        } else if name.starts_with("s3://") {
            new_s3_reader(name)?
        } else if let Some(stripped) = name.strip_prefix("<<") {
            Box::new(io::Cursor::new(unescape_vec(stripped.as_bytes())))
        } else {
            Box::new(fs::File::open(name)?)
        }
    };
    let mut outer = io::BufReader::new(inner);
    let start = outer.fill_buf()?;
    if start.starts_with(&[0x1f, 0x8b, 0x08]) {
        outer = io::BufReader::new(Box::new(MultiGzDecoder::new(outer)));
    }
    let start = outer.fill_buf()?;
    if start.starts_with(&[0xef, 0xbb, 0xbf]) {
        outer.consume(3);
    }
    Ok(Infile::new(outer, name))
}

/// where are we, in which file?
#[derive(Debug, Default, Clone)]
pub struct FileLocData {
    /// full file name
    pub name: String,
    /// byte offset, uncompressed
    pub bytes: usize,
    /// byte offset or beginning of line, uncompressed
    pub prev_bytes: usize,
    /// line number
    pub line: usize,
}

impl FileLocData {
    /// new
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), bytes: 0, line: 0, prev_bytes: 0 }
    }
    /// update location data for a new line
    pub fn new_line(&mut self, name: &str, bytes: usize) {
        self.name = name.to_string();
        self.bytes = bytes;
        self.line += 1;
    }
    /// reset counts to zero
    pub const fn reset(&mut self) {
        self.bytes = 0;
        self.line = 0;
    }
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
                Ok(Self::Name(b.to_usize_whole(spec.as_bytes(), "File location")?))
            } else {
                err!("File Loc must be one of Bytes, Line, Name : '{}'", spec)
            }
        } else {
            err!("File Loc must be one of Bytes, Line, Name : '{}'", spec)
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
            Self::Bytes => write!(data, "{}", loc.prev_bytes + 1).unwrap(),
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

/// `FileLocItem` with column name
#[derive(Debug, Clone)]
struct FileLoc {
    col_name: String,
    item: FileLocItem,
}
impl FileLoc {
    fn new(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(':') {
            Ok(Self { col_name: a.to_string(), item: FileLocItem::new(b)? })
        } else {
            let item = FileLocItem::new(spec)?;
            Ok(Self { col_name: item.dflt_name().to_string(), item })
        }
    }
    fn write_data(&mut self, data: &mut impl Write, loc: &FileLocData) -> Result<()> {
        self.item.write_data(data, loc)
    }
}

/// List of `FileLoc`
#[derive(Default, Debug, Clone)]
pub struct FileLocList {
    v: Vec<FileLoc>,
}
impl FileLocList {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// new
    #[must_use]
    pub const fn is_empty(&self) -> bool {
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

/// print a bunch of u8 to stderr, adding a newline
pub fn prerr(data: &[&[u8]]) {
    for x in data {
        io::stderr().write_all(x).unwrap();
    }
    io::stderr().write_all(b"\n").unwrap();
}
/// print a bunch of u8 to stderr
pub fn prerr_n(data: &[&[u8]]) {
    for x in data {
        io::stderr().write_all(x).unwrap();
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum HeaderMode {
    /// First can be anything, others must match
    #[default]
    Match,
    /// First must be CDX, others must match
    Require,
    /// Any CDX are removed
    Strip,
    /// No CDX allowed
    None,
    /// First can be anything, others blindly accepted
    Trust,
    /// Ignore CDX if present
    Ignore,
}

impl FromStr for HeaderMode {
    type Err = Error;
    fn from_str(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("match") {
            Ok(Self::Match)
        } else if spec.eq_ignore_ascii_case("require") {
            Ok(Self::Require)
        } else if spec.eq_ignore_ascii_case("strip") {
            Ok(Self::Strip)
        } else if spec.eq_ignore_ascii_case("none") {
            Ok(Self::None)
        } else if spec.eq_ignore_ascii_case("trust") {
            Ok(Self::Trust)
        } else if spec.eq_ignore_ascii_case("ignore") {
            Ok(Self::Ignore)
        } else {
            err!("Input Header Mode must be one of Match, Require, Strip, None or Trust : {}", spec)
        }
    }
}

/// Object to enforce `HeaderMode`
#[derive(Debug, Default, Clone)]
pub struct HeaderChecker {
    /// the mode to enforce
    pub mode: HeaderMode,
    /// the first header seen
    pub head: Vec<u8>,
    /// true after first header has been processed
    pub saw_one: bool,
}

/// Is this line a CDX header?
#[must_use]
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
    let delim = char::from_u32(u32::from(delim)).unwrap();
    for x in data.split(delim) {
        if x.is_empty() {
            return err!("File {} has an empty column name", fname);
        }
        if !x.first().is_alphabetic() {
            return err!(
                "Header for file {} has column name {} which does not start with an alphabetic character.",
                fname,
                x
            );
        }
        for ch in x.chars() {
            if !ch.is_alphanumeric() && ch != '_' {
                return err!(
                    "Header for file {} has column name {} which contains something other than alphanumeric and underscore.",
                    fname,
                    x
                );
            }
        }
    }
    Ok(true)
}

impl HeaderChecker {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// new with mode
    #[must_use]
    pub fn from_mode(mode: HeaderMode) -> Self {
        Self { mode, ..Self::default() }
    }
    /// call for the first line of every input file
    /// return true if the header should be written
    pub fn check_file(&mut self, file: &TextFile, fname: &str) -> Result<bool> {
        let first = !self.saw_one;
        if file.has_header() {
            self.check(file.line(), fname)?;
        } else {
            self.check(b"fake", fname)?;
        }
        Ok(file.has_header() && first)
    }

    /// call for the first line of every input file
    /// return true if the line should be part of the output
    // FIXME - this needs to take input::Config and needs to return "is this a header".
    pub fn check(&mut self, first_line: &[u8], fname: &str) -> Result<bool> {
        // FIXME - if is_cdx, make sure is also a valid header
        let cdx = is_valid_cdx(first_line, self.mode, fname)?;
        if first_line.is_empty() {
            Ok(false)
        // } else if first_line.last().unwrap() != &b'\n' && cdx {
        //     err!("Malformed Header line in file {}", &fname)
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
#[derive(Debug, Copy, Clone, Default)]
pub enum CompareOp {
    /// less than
    #[default]
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
            err!("Invalid CompareOp, should be one of LT,<,GT,>,LE,<=,GE,>=,EQ,==,NE,!= : '{}'", s)
        }
    }
}

impl CompareOp {
    /// invert left vs right. That is, swap less vs greater, but not equal vs not equal
    #[must_use]
    pub const fn invert(&self) -> Self {
        use CompareOp::{EQ, GE, GT, LE, LT, NE};
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
        if self.invert().line_ok(line, comp) {
            true
        } else {
            eprint!("Line {line_num} : ");
            prerr_n(&[&line.line]);
            eprint!("should have been {self:?} ");
            prerr_n(&[comp.get_value()]);
            eprintln!(" but wasn't");
            false
        }
    }
    /// return (line OP value)
    pub fn line_ok(&self, line: &TextLine, comp: &mut LineCompList) -> bool {
        let o = comp.comp_self_cols(line);
        self.ord_ok(o)
    }
    /// does Ordering satisfy `CompareOp`
    #[must_use]
    pub fn ord_ok(&self, o: Ordering) -> bool {
        use CompareOp::{EQ, GE, GT, LE, LT, NE};
        use Ordering::{Equal, Greater, Less};
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
        static RE1: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("^(<|>|<=|>=|==|!=)([^<>!=]+)(<|>|<=|>=|==|!=)(.+)$").unwrap()
        });
        static RE2: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("^(<|>|<=|>=|==|!=)(.+)$").unwrap());
        static RE3: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("^(LT|GT|LE|GE|EQ|NE),([^,]+),(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap()
        });
        static RE4: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("^(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap());
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
        static RE1: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("(^|,)(<|>|<=|>=|==|!=)([^<>!=]+)(<|>|<=|>=|==|!=)([^=].*)$").unwrap()
        });
        static RE2: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("(^|,)(<|>|<=|>=|==|!=)([^=].*)$").unwrap());
        static RE3: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new("(^|,)(LT|GT|LE|GE|EQ|NE),([^,]+),(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap()
        });
        static RE4: LazyLock<Regex> =
            LazyLock::new(|| Regex::new("(^|,)(LT|GT|LE|GE|EQ|NE),(.+)$").unwrap());
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
        r.val1.clone_into(&mut self.val);
        self.op2 = if let Some(item) = r.op2 { Some(item.parse::<CompareOp>()?) } else { None };
        self.val2 = if r.val2.is_none() { None } else { Some(r.op2.unwrap().to_owned()) };
        Ok(())
    }
    /// set from spec OP,Text
    pub fn set(&mut self, spec: &str) -> Result<()> {
        self.set_with(&RangeSpec::new(spec)?)
    }
    /// compare line OP text, return true if matched, print to stderr if non-match
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
#[derive(Debug)]
pub struct CheckBuff {
    op: CompareOp,
    val: Box<dyn comp::UsefulCompare>,
    op2: Option<CompareOp>,
    val2: Option<Box<dyn comp::UsefulCompare>>,
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
            op2: if let Some(item) = r.op2 { Some(item.parse::<CompareOp>()?) } else { None },
            val2: if let Some(item) = r.val2 {
                let mut c = CompMaker::make_comp_box(comp_spec)?;
                c.set(item.as_bytes());
                Some(c)
            } else {
                None
            },
        })
    }
    /// does right match?
    #[must_use]
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
        _ => err!("I don't know how to close a '{}'", ch as char),
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
#[must_use]
pub fn chomp(mut x: &[u8]) -> &[u8] {
    while !x.is_empty() {
        let len = x.len() - 1;
        if x[len] != b'\n' && x[len] != b'\r' {
            break;
        }
        x = &x[..len];
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
