//! Misc utility stuff
//#![allow(dead_code)]
use crate::column::ColumnHeader;
use crate::comp::{CompMaker, Compare};
use crate::prelude::*;
use anyhow;
use flate2::read::MultiGzDecoder;
use fs_err as fs;
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp;
use std::error;
use std::fmt::Write as _;
use std::io;
use std::ops::{Deref, DerefMut};
use std::str;
// use tokio_stream::StreamExt;

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
    Err(anyhow::Error::new(err))
}

/// if suppress(e) is true, treat as success
#[must_use] pub fn suppress(err: &Error) -> bool {
    if let Some(ioerr) = err.downcast_ref::<io::Error>() {
        ioerr.kind() == io::ErrorKind::BrokenPipe
    } else {
        matches!(err.downcast_ref::<CdxError>(), Some(CdxError::NoError))
    }
}

/// if silent(e) is true, treat as failure, but print no message
#[must_use] pub fn silent(err: &Error) -> bool {
    matches!(err.downcast_ref::<CdxError>(), Some(CdxError::Silent))
}

impl fmt::Display for CdxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(s) => write!(f, "{s}")?,
            Self::NeedLookup => write!(
                f,
                "ColumnSet.lookup() must be called before ColumnSet.select()"
            )?,
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
    #[must_use] pub const fn yes_if(x: bool) -> Self {
        if x {
            Self::Yes
        } else {
            Self::Maybe
        }
    }
    /// return No is x is true, else Maybe
    #[must_use] pub const fn no_if(x: bool) -> Self {
        if x {
            Self::No
        } else {
            Self::Maybe
        }
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
    /// discrd cdx header, if present
    Skip,
    /// has a CDX header
    Cdx,
}
impl HeadMode {
    /// does this mode imply a CDX header
    #[must_use] pub fn has_cdx(self) -> bool {
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
// angle brckets
// explicit quotes, e.g. A...B or A...A
// parens
// nested ?? e.g. ab"cd(ef" means what?
// brackets only recognized at ends of values
// --dx (whole line one column)

/// convert char to ue, but 't' means tab and such
#[must_use] pub const fn auto_escape(ch: char) -> u8 {
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

const TEXT_HELP_STRING: &str = r#"

# Input Text Files #

All tools have a --text-in option, that takes a string of up to five characters
which defines how text files are parsed.

The first character is the header mode, which can be
    m - maybe (default) : Use the CDX header if it exists, otherwise don't.
    y - yes : A CDX header is required
    n - no : A CDX header is forbidden
    s - skip : A CDX header, if present, is ignored.

The second chracter is the column delimiter, auto-escaped
    default is 't' for 'tab'

The third charater is the quoting mode
    p - plain (default) : no quoting is done. It is impossible to have a data field
                          containing the column deliiter.
    q - quote : The column delimiter is ignored when between double quotes : '"'
                Within a quoted sting, '""' is used to mean '"'
    b - backslash : All backslahes are combined with the following character in the usual way.

The fouth character is the replacement character, used when the quote mode is 'plain'.
Any occurrences of the column delimiter are replaced with this charater.
    default is space.

The fifth character is the line delimiter, default is newline (\n)

Omitted trailing characters accept the default, so 'y' is the same as 'ytpsn'

# Output Text Files #

All tools have a --text-out option that controls how text files are written.
Its parameter is treated exactly like --text-in, except that the default values
are thos passed to --text-in.
If --text-in sepcified "maybe" as the header mode, then the output default is
'yes' if an input header was present and 'no' otherwise.

# Auto Escaped #
    Auto escaping make it easier to specify special characters,
    mostly by implying a leading backslash.
    Auto escaping maps letters to other characters :
    t is tab
    s is space
    r is return (/r)
    n is newline (/n)
    other lowecase ascii is reserved
    otherwise the character is unchanged.
"#;

#[derive(Debug, Copy, Clone)]
/// how to pase a text file into lines and columns
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
                return err!("First char of text-fmt spec must be y (yes), n (no), m (maybe) s (skip) or x (cdx) not '{ch}'");
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
                return err!("Fourth char of text-fmt spec must be p (plain), q (quote) or b (backslash) not '{ch}'");
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
        Ok(Self {
            head_mode,
            col_mode,
            delim,
            line_break,
            repl,
        })
    }

    /// print the help for TextFileMode
    pub fn text_help() {
        println!("{}", TEXT_HELP_STRING);
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
    /// split data line into columns
    pub fn split(&self, line: &mut TextLine) {
        match self.col_mode {
            QuoteMode::Plain => split_plain(&mut line.parts, &line.line, self.delim),
            QuoteMode::Backslash => {
                line.orig.clear();
                line.orig.extend_from_slice(&line.line);
                split_backslash(&mut line.parts, &line.orig, &mut line.line, self.delim);
            }
            QuoteMode::Quote => {
                line.orig.clear();
                line.orig.extend_from_slice(&line.line);
                split_quotes(&mut line.parts, &line.orig, &mut line.line, self.delim);
            }
        }
    }
    /// split data line into columns
    pub fn split_head(&self, line: &mut StringLine) {
        line.split(self.delim);
        if line.line.starts_with(" CDX") {
            line.parts.remove(0);
        }
    }
    /// append a newline, if line does not already end with one. Return false.
    pub fn ensure_eof(&self, line: &mut Vec<u8>) -> Result<bool> {
        if line[line.len() - 1] != self.line_break {
            line.push(self.line_break);
        }
        Ok(false)
    }
    /// read a line, dealing with quotes if necessary, return as String    
    pub fn read_string<T: BufRead>(&self, f: &mut T, line: &mut String) -> Result<bool> {
        let mut x = Vec::new();
        if self.read_line(f, &mut x)? {
            return Ok(true);
        }
        *line = String::from_utf8(x)?;
        Ok(false)
    }
    /// read a line, dealing with quotes if necessary. Return true if file empty
    pub fn read_header<T: BufRead>(&self, f: &mut T, line: &mut String) -> Result<bool> {
        line.clear();
        let start = f.fill_buf()?;
        if start.is_empty() {
            return Ok(true);
        }
        match self.head_mode {
            HeadMode::Yes => self.read_string(f, line),
            HeadMode::Cdx => self.read_string(f, line),
            HeadMode::No => Ok(false),
            HeadMode::Maybe => {
                if start.starts_with(b" CDX") {
                    self.read_string(f, line)
                } else {
                    Ok(false)
                }
            }
            HeadMode::Skip => {
                let start = f.fill_buf()?;
                if start.starts_with(b" CDX") {
                    self.read_string(f, line)?;
                    line.clear();
                }
                let start = f.fill_buf()?;
                Ok(start.is_empty())
            }
        }
    }
    /// read a line, dealing with quotes if necessary. return true if no bytes read.
    pub fn read_line<T: BufRead>(&self, f: &mut T, line: &mut Vec<u8>) -> Result<bool> {
        line.clear();
        let sz = f.read_until(self.line_break, line)?;
        if sz == 0 {
            return Ok(true);
        }
        if self.col_mode != QuoteMode::Quote {
            return self.ensure_eof(line);
        }
        let mut within = false;
        let mut skip = 0;
        loop {
            for ch in line.iter().skip(skip) {
                if *ch == b'"' {
                    within = !within;
                }
            }
            skip = line.len();
            if !within {
                return self.ensure_eof(line);
            }
            let sz = f.read_until(self.line_break, line)?;
            if sz == 0 {
                return self.ensure_eof(line);
            }
        }
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
    /// get slice from `FakeSlice`
    #[must_use] pub fn get<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[self.begin as usize..self.end as usize]
    }
    /// get slice from `FakeSlice`, but don't fall off the end.
    #[must_use] pub fn get_safe<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[cmp::min(self.begin as usize, data.len())..cmp::min(self.end as usize, data.len())]
    }
    /// len
    #[must_use] pub const fn len(&self) -> usize {
        (self.end - self.begin) as usize
    }
    /// is empty?
    #[must_use] pub const fn is_empty(&self) -> bool {
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
            w.write_all(&[b'\\'])?;
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
            w.write_all(&[b'"'])?;
        }
        w.write_all(&[*ch])?;
    }
    Ok(())
}

/// split a line by a delimiter. Remove trailing newline, if any.
pub fn split_plain(parts: &mut Vec<FakeSlice>, line: &[u8], delim: u8) {
    parts.clear();
    let mut begin: u32 = 0;
    let mut end: u32 = 0;
    #[allow(clippy::explicit_counter_loop)] // I need the counter to be u32
    for ch in line {
        if *ch == delim {
            parts.push(FakeSlice { begin, end });
            begin = end + 1;
        }
        end += 1;
    }
    if begin != end {
        let mut f = FakeSlice { begin, end };
        if line[(end - 1) as usize] == b'\n' {
            f.end -= 1;
        }
        parts.push(f);
    }
}

/// undo slash encoding, e.g. 'n' becomes '\n'
#[must_use] pub const fn unslash(ch: u8) -> u8 {
    match ch {
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        _ => ch,
    }
}

/// do slash encoding, e.g. 'n' becomes '\n'
#[must_use] pub const fn enslash(ch: u8) -> u8 {
    match ch {
        b'\n' => b'n',
        b'\r' => b'r',
        b'\t' => b't',
        _ => ch,
    }
}

/// split a line by a delimiter. Remove trailing newline, if any.
pub fn split_backslash(parts: &mut Vec<FakeSlice>, line: &[u8], tmp: &mut Vec<u8>, delim: u8) {
    parts.clear();
    tmp.clear();
    let mut line = line;
    while !line.is_empty() && line[line.len() - 1] == b'\n' {
        line = &line[..line.len() - 1];
    }
    let mut begin: u32 = 0;
    let mut end: u32 = 0;
    let mut last_was_slash = false;
    for xch in line {
        let ch = *xch;
        if last_was_slash {
            tmp.push(unslash(ch));
            end += 1;
            last_was_slash = false;
            continue;
        } else if ch == b'\\' {
            last_was_slash = true;
            continue;
        }

        tmp.push(ch);
        if ch == delim && !last_was_slash {
            parts.push(FakeSlice { begin, end });
            begin = end + 1;
        }
        end += 1;
        last_was_slash = false;
    }
    if begin != end {
        parts.push(FakeSlice { begin, end });
    }
}

/// split a line by a delimiter. Remove trailing newline, if any.
pub fn split_quotes(parts: &mut Vec<FakeSlice>, line: &[u8], tmp: &mut Vec<u8>, delim: u8) {
    parts.clear();
    tmp.clear();
    let mut line = line;
    while !line.is_empty() && line[line.len() - 1] == b'\n' {
        line = &line[..line.len() - 1];
    }
    let mut begin: u32 = 0;
    let mut end: u32 = 0;
    let mut last_was_quote = false;
    let mut in_quote = false;
    for ch in line {
        if in_quote {
            if last_was_quote {
                if *ch == b'"' {
                    tmp.push(*ch);
                    end += 1;
                    continue;
                } else {
                    in_quote = false;
                    // not continue
                }
            } else if *ch == b'"' {
                last_was_quote = true;
                continue;
            }
        } else if *ch == b'"' {
            in_quote = true;
            last_was_quote = true;
            continue;
        }

        tmp.push(*ch);
        if *ch == delim {
            parts.push(FakeSlice { begin, end });
            begin = end + 1;
        }
        end += 1;
    }
    if begin != end {
        parts.push(FakeSlice { begin, end });
    }
}

/// A line of a text file, broken into columns.
/// A line ends with a newline character, but column values do not.
/// An empty line contains one empty column
///```
/// use std::io::BufRead;
/// use cdx::util::TextFileMode;
/// let mut data = b"one\ttwo\tthree\n";
/// let mut dp = &data[..];
/// let mut line = cdx::util::TextLine::new();
/// let eof = line.read(&mut dp).unwrap();
/// assert_eq!(eof, false);
/// assert_eq!(line.strlen(), 14);
/// let mode = TextFileMode::default();
/// line.split(&mode);
/// assert_eq!(line.len(), 3);
/// assert_eq!(line.get(1), b"two");
///```
#[derive(Debug, Clone, Default)]
// pub(crate) because the borrow checker can be a nuisance.
pub struct TextLine {
    // The whole input line, with newline, decoded as necessary
    pub(crate) line: Vec<u8>,
    // the individual columns, without newline
    pub(crate) parts: Vec<FakeSlice>,
    // the original input line, if any decoding was necessary
    pub(crate) orig: Vec<u8>,
}

/// as `TextLine`, but [String] rather than `Vec<u8>`
#[derive(Debug, Clone, Default)]
pub struct StringLine {
    /// The whole input line, with newline
    pub line: String,
    /// the individual columns
    pub parts: Vec<FakeSlice>,
    // the original input line, if any decoding was necessary
    pub(crate) orig: String,
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
            write!(f, "{i} ")?;
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
    /// whole line, with newline
    pub fn parts(&mut self) -> &mut Vec<FakeSlice> {
        &mut self.parts
    }
    /// whole line, with newline
    #[must_use] pub fn line(&self) -> &[u8] {
        if self.orig.is_empty() {
            &self.line
        } else {
            &self.orig
        }
    }
    /// whole line, without newline
    #[must_use] pub fn line_nl(&self) -> &[u8] {
        &self.line[..self.line.len() - 1]
    }
    /// whole line, with newline, as Vec
    pub fn raw(&mut self) -> &mut Vec<u8> {
        &mut self.line
    }
    /// assign `TextLine` into existing `TextLine`, avoiding allocation if possible
    pub fn assign(&mut self, x: &Self) {
        self.line.clear();
        self.line.extend_from_slice(&x.line[..]);
        self.parts.clear();
        self.parts.extend_from_slice(&x.parts[..]);
    }
    /// make a new `TextLine`
    #[must_use] pub const fn new() -> Self {
        Self {
            line: Vec::new(),
            parts: Vec::new(),
            orig: Vec::new(),
        }
    }
    /// Iterator over columns in the line
    #[must_use] pub const fn iter(&self) -> TextLineIter<'_> {
        TextLineIter {
            line: self,
            index: 0,
        }
    }
    /// empty the line
    pub fn clear(&mut self) {
        self.parts.clear();
        self.line.clear();
    }
    /// How many column in the line
    #[must_use] pub fn len(&self) -> usize {
        self.parts.len()
    }
    /// How many bytes in the line
    #[must_use] pub fn strlen(&self) -> usize {
        self.line.len()
    }
    /// should always be false, but required by clippy
    #[must_use] pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }
    /// Get one column. Return an empty column if index is too big.
    #[must_use] pub fn get(&self, index: usize) -> &[u8] {
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
    pub fn split(&mut self, text: &TextFileMode) {
        split_plain(&mut self.parts, &self.line, text.delim);
    }
    /// return all parts as a vector
    #[must_use] pub fn vec(&self) -> Vec<&[u8]> {
        self.iter().collect()
    }
}

impl StringLine {
    /// make a new `StringLine`
    #[must_use] pub const fn new() -> Self {
        Self {
            line: String::new(),
            parts: Vec::new(),
            orig: String::new(),
        }
    }
    /// Iterator over columns in the line
    #[must_use] pub const fn iter(&self) -> StringLineIter<'_> {
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
    #[must_use] pub fn len(&self) -> usize {
        self.parts.len()
    }
    /// How many bytes in the line
    #[must_use] pub fn strlen(&self) -> usize {
        self.line.len()
    }
    /// should always be false, but required by clippy
    #[must_use] pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }
    /// Get one column. Return an empty column if index is too big.
    #[must_use] pub fn get(&self, index: usize) -> &str {
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
        split_plain(&mut self.parts, self.line.as_bytes(), delim);
    }
    /// return all parts as a vector
    #[must_use] pub fn vec(&self) -> Vec<&str> {
        self.iter().collect()
    }
    /// whole line, with newline
    #[must_use] pub fn line(&self) -> &str {
        if self.orig.is_empty() {
            &self.line
        } else {
            &self.orig
        }
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

/// Iterator over the columns in a `TextLine`
#[derive(Debug, Clone)]
pub struct TextLineIter<'a> {
    line: &'a TextLine,
    index: usize,
}

/// Iterator over the columns in a `StringLine`
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
    f: aws_sdk_s3::operation::get_object::GetObjectOutput,
    left: Option<bytes::Bytes>,
}
impl S3Reader {
    fn new(bucket: &str, key: &str) -> Result<Self> {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
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
    pub String,
);

impl Infile {
    /// create a new input file
    #[must_use] pub fn new(f: io::BufReader<Box<dyn Read>>, n: &str) -> Self {
        Self(f, n.to_string())
    }
}

impl Default for Infile {
    fn default() -> Self {
        Self::new(io::BufReader::new(Box::new(io::empty())), "")
    }
}

impl fmt::Debug for Infile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Infile : {}", self.1)
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
pub struct Outfile(pub io::BufWriter<Box<dyn Write>>, pub String);

impl Outfile {
    /// create a new input file
    #[must_use] pub fn new(f: io::BufWriter<Box<dyn Write>>, n: &str) -> Self {
        Self(f, n.to_string())
    }
}

impl Default for Outfile {
    fn default() -> Self {
        Self::new(io::BufWriter::new(Box::new(io::sink())), "")
    }
}

impl fmt::Debug for Outfile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Outfile : {}", self.1)
    }
}

impl Deref for Outfile {
    type Target = io::BufWriter<Box<dyn Write>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Outfile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<io::BufWriter<Box<dyn Write>>> for Outfile {
    fn as_ref(&self) -> &io::BufWriter<Box<dyn Write>> {
        &self.0
    }
}

impl AsMut<io::BufWriter<Box<dyn Write>>> for Outfile {
    fn as_mut(&mut self) -> &mut io::BufWriter<Box<dyn Write>> {
        &mut self.0
    }
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
    Ok(Infile::new(outer, name))
}

#[derive(Debug, Default)]
/// shared context for any input file type
pub struct InfileContext {
    // CDX header, contructed if necessary
    header: StringLine,
    // have we read all the btes of the file
    is_done: bool,
    // is the file length zero
    is_empty: bool,
    // was there a CDX header?
    has_header: bool,
    text: TextFileMode,
}

/// create appropriate header from first line of file
#[must_use] pub fn make_header(line: &[u8]) -> StringLine {
    let mut s = StringLine::new();
    if is_cdx(line) {
        s.line = String::from_utf8_lossy(&line[5..]).to_string();
    } else {
        s.line = String::new();
        for x in 1..=line.split(|ch| *ch == b'\t').count() {
            write!(s.line, "c{x}\t").unwrap();
        }
        s.line.pop();
    }
    s.split(b'\t');
    s
}

// if CDX and specified and different, then strip header
/// Reader header line, if any, and first line of text
impl InfileContext {
    const fn new(text_in: &TextFileMode) -> Self {
        Self {
            header: StringLine::new(),
            is_done: true,
            is_empty: true,
            has_header: false,
            text: *text_in,
        }
    }
    fn read_header(&mut self, file: &mut impl BufRead, line: &mut TextLine) -> Result<()> {
        self.is_empty = self.text.read_header(file, &mut self.header.line)?;
        if self.is_empty {
            return Ok(());
        }
        self.has_header = !self.header.line.is_empty();
        if self.has_header {
            self.text.split_head(&mut self.header);
        }
        self.is_done = self.text.read_line(file, &mut line.line)?;
        if self.is_done {
            return Ok(());
        }
        self.text.split(line);

        if !self.has_header {
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
            self.header.split(self.text.delim);
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

/// `FileLocItem` with column name
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

/// List of `FileLoc`
#[derive(Default, Debug, Clone)]
pub struct FileLocList {
    v: Vec<FileLoc>,
}
impl FileLocList {
    /// new
    #[must_use] pub fn new() -> Self {
        Self::default()
    }
    /// new
    #[must_use] pub fn is_empty(&self) -> bool {
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

/// Text file reader. Lines broken into columns, with lookback
pub struct Reader {
    file: Infile,
    lines: Vec<TextLine>,
    cont: InfileContext,
    do_split: bool,
    curr: usize,
    loc: FileLocData,
}
impl fmt::Debug for Reader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Reader")
    }
}

impl Default for Reader {
    fn default() -> Self {
        Self::new(&TextFileMode::default())
    }
}

impl Reader {
    /// loc
    #[must_use] pub const fn loc(&self) -> &FileLocData {
        &self.loc
    }
    /// make a new Reader
    #[must_use] pub fn new(text_in: &TextFileMode) -> Self {
        Self::new_with(1, text_in)
    }
    /// set to false to skip breaking into columns
    pub fn do_split(&mut self, val: bool) {
        self.do_split = val;
    }
    /// make a new Reader, with explicit lookback
    #[must_use] pub fn new_with(lookback: usize, text_in: &TextFileMode) -> Self {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        Self {
            file: Infile::default(),
            lines,
            cont: InfileContext::new(text_in),
            do_split: true,
            curr: 0,
            loc: FileLocData::default(),
        }
    }
    /// make a new Reader, with deault text, FIXME - delete this
    pub fn new_open2(name: &str) -> Result<Self> {
        Self::new_open_with(name, 1, &TextFileMode::default())
    }
    /// make a new Reader
    pub fn new_open(name: &str, text_in: &TextFileMode) -> Result<Self> {
        Self::new_open_with(name, 1, text_in)
    }
    /// make a new Reader
    pub fn new_open_with(name: &str, lookback: usize, text_in: &TextFileMode) -> Result<Self> {
        let mut lines: Vec<TextLine> = Vec::new();
        lines.resize(lookback + 1, TextLine::new());
        let mut tmp = Self {
            file: get_reader(name)?,
            lines,
            cont: InfileContext::new(text_in),
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
    #[must_use] pub fn curr_nl(&self) -> &[u8] {
        let line = self.curr_line();
        &line.line[0..line.line.len() - 1]
    }
    /// get previous line contents, without the trailing newline
    #[must_use] pub fn prev_nl(&self, n: usize) -> &[u8] {
        let line = self.prev_line(n);
        &line.line[0..line.line.len() - 1]
    }
    /// get delimiter
    #[must_use] pub const fn delim(&self) -> u8 {
        self.cont.text.delim
    }
    /// get column names
    #[must_use] pub fn names(&self) -> Vec<&str> {
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
    #[must_use] pub const fn header_line(&self) -> &String {
        &self.cont.header.line
    }
    /// was file zero bytes?
    #[must_use] pub const fn is_empty(&self) -> bool {
        self.cont.is_empty
    }
    /// have we hit EOF?
    #[must_use] pub const fn is_done(&self) -> bool {
        self.cont.is_done
    }
    /// line number of `curr_line`
    #[must_use] pub const fn line_number(&self) -> usize {
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
        if self
            .cont
            .text
            .read_line(&mut *self.file, &mut self.lines[self.curr].line)?
        {
            self.cont.is_done = true;
        } else if self.do_split {
            self.cont.text.split(&mut self.lines[self.curr]);
        }
        Ok(self.cont.is_done)
    }
    /// get current line of text
    #[must_use] pub fn curr_line(&self) -> &TextLine {
        &self.lines[self.curr]
    }
    /// get current line of text
    pub fn curr_mut(&mut self) -> &mut TextLine {
        &mut self.lines[self.curr]
    }
    /// get current line of text
    #[must_use] pub fn curr(&self) -> &TextLine {
        &self.lines[self.curr]
    }
    /// get a previous line of text
    /// looking back from the start of the file shows empty lines.
    #[must_use] pub fn prev_line(&self, lookback: usize) -> &TextLine {
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
    #[must_use] pub const fn header(&self) -> &StringLine {
        &self.cont.header
    }
    /// write header
    #[must_use] pub const fn has_header(&self) -> bool {
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
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    /// Ignore CDX if present
    Ignore,
}

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

/// Object to enforce `HeaderMode`
#[derive(Debug, Default, Clone)]
pub struct HeaderChecker {
    /// the mode to enforce
    pub mode: HeaderMode,
    /// the first header seen
    head: Vec<u8>,
    /// true after first header has been processed
    saw_one: bool,
}

/// Is this line a CDX header?
#[must_use] pub fn is_cdx(data: &[u8]) -> bool {
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
    #[must_use] pub fn new() -> Self {
        Self::default()
    }
    /// new with mode
    #[must_use] pub fn from_mode(mode: HeaderMode) -> Self {
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
    /// less than
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
    #[must_use] pub const fn invert(&self) -> Self {
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
        if !self.invert().line_ok(line, comp) {
            eprint!("Line {line_num} : ");
            prerr_n(&[&line.line]);
            eprint!("should have been {self:?} ");
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
    /// does Ordering satisfy `CompareOp`
    #[must_use] pub fn ord_ok(&self, o: Ordering) -> bool {
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
    #[must_use] pub fn buff_ok(&self, right: &[u8]) -> bool {
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
#[must_use] pub fn chomp(mut x: &[u8]) -> &[u8] {
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
    fn read_line() -> Result<()> {
        let mut mode = TextFileMode::default();
        let data1 = b"aaa\tbbb\n";
        let data2 = b"aaa\tbbb";
        let data3 = b"aaa\tbb\"\n\"b\n";
        let data4 = b"aaa\tbb\"\n";
        let mut line = Vec::new();
        mode.read_line(&mut &data1[..], &mut line)?;
        assert_eq!(data1, &line[..]);
        mode.read_line(&mut &data2[..], &mut line)?;
        assert_eq!(data1, &line[..]);
        mode.read_line(&mut &data3[..], &mut line)?;
        assert_eq!(data4, &line[..]);
        mode.col_mode = QuoteMode::Quote;
        mode.read_line(&mut &data3[..], &mut line)?;
        assert_eq!(data3, &line[..]);
        mode.col_mode = QuoteMode::Backslash;
        mode.read_line(&mut &data3[..], &mut line)?;
        assert_eq!(data4, &line[..]);
        Ok(())
    }
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
