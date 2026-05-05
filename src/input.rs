//! Column parsing utilities for delimited text formats like CSV and TSV.
//!
//! The parser is designed for high throughput and low allocation churn:
//! parsed column buffers are reused across calls to [`Columns::read`].

use crate::prelude::*;
use crate::util::Infile;
use crate::recyclable_vec::RecyclableVec;
use memchr::{memchr2, memchr3};
use std::collections::HashSet;
use std::io;

pub(crate) fn new_to_old(input: &[String]) -> Vec<&str> {
    input.iter().map(String::as_str).collect()
}

/// How input records are split into columns.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    /// columns are separated by a single character, e.g. comma, tab, etc.
    Char(u8),
    /// columns are separated by a full byte string, e.g. `b", "` or `b".."`.
    /// The delimiter is matched exactly and fully.
    String(Vec<u8>),
    /// Each line of input is a single column, i.e. the whole line is the value of the column.
    Whole,
    /// Columns are separated by one or more ASCII whitespace bytes.
    /// Repeated, leading, and trailing whitespace do not create empty columns.
    WhitespaceDrop,
    /// Columns are separated by one or more ASCII whitespace bytes.
    /// Delimiter whitespace is preserved as a prefix of the following column.
    /// Trailing whitespace at end-of-line is discarded.
    WhitespaceKeep,
}
impl Default for Delimiter {
    fn default() -> Self {
        Self::Char(b'\t')
    }
}

/// Whether backslash escape decoding is enabled while parsing.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum BackslashMode {
    /// Backslash has no special meaning.
    #[default]
    Off,
    /// Backslash enables escape decoding (`\n`, `\r`, `\t`, `\\`, and unknown-escape passthrough).
    On,
}

/// Quote handling strategy used while parsing.
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub enum Quotes {
    /// No quote handling.
    #[default]
    None,
    /// A single open/close quote pair.
    Single(u8, u8),
    /// Multiple open/close quote pairs; any open byte can start a quoted region.
    Multi(Vec<(u8, u8)>),
}

/// Behavior when end-of-line is reached while still inside a quote.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum UnterminatedQuoteMode {
    /// Continue reading physical lines until quote state closes or EOF is reached.
    #[default]
    ContinueRecord,
    /// End the record at end-of-line even if quote state is still open.
    EndAtEol,
    /// Return `InvalidData` if end-of-line is reached while quote state is still open.
    Error,
}

/// Whether input is expected to include a header record.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum Header {
    /// Use CDX if present, otherwise first line is column names
    #[default]
    Yes,
    /// Use CDX if present, otherwise first line is data
    No,
}

/// How to handle CDX header
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum CdxMode {
    /// Use CDX if available
    #[default]
    Optional,
    /// Fail if CDX absent
    Required,
    /// Fail if CDX present
    Forbidden,
    /// Ignore CDX if present
    Ignore,
}

/// What was actually observed on the input stream regarding a header record.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SawHeader {
    /// A normal header record was observed.
    Yes,
    /// No header record was observed.
    No,
    /// A CDX-style header record was observed.
    Cdx,
    /// The file contained zero bytes
    Empty,
}

/// Complete parser configuration used by [`Columns::read`].
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Config {
    /// Column delimiter mode.
    pub delimiter: Delimiter,
    /// Quote handling mode.
    pub quotes: Quotes,
    /// Backslash escape handling mode.
    pub backslash: BackslashMode,
    /// Unterminated quote behavior.
    pub unterminated_quote: UnterminatedQuoteMode,
    /// Whether input is expected to include a header record.
    pub header: Header,
    /// How CDX-style headers are handled.
    pub cdx: CdxMode,
    /// What header state has been observed so far.
    pub saw_header: Option<SawHeader>,
}

impl Config {
    /// Build options with sane defaults for CSV/TSV-like parsing:
    /// no quotes, no backslash escapes, quoted records continue across lines, and header enabled.
    #[must_use]
    pub const fn new(delimiter: Delimiter) -> Self {
        Self {
            delimiter,
            quotes: Quotes::None,
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
            header: Header::Yes,
            cdx: CdxMode::Optional,
            saw_header: None,
        }
    }

    /// Construct standard CSV parsing options.
    ///
    /// This uses:
    /// - comma delimiter
    /// - double-quote quoting with doubled-close escaping (`""`)
    /// - backslash decoding off
    /// - records may continue across physical lines while inside quotes
    /// - header enabled
    ///
    /// # Examples
    /// ```rust
    /// use cdx::input::{Config, Delimiter, Quotes};
    ///
    /// let options = Config::csv();
    /// assert_eq!(options.delimiter, Delimiter::Char(b','));
    /// assert_eq!(options.quotes, Quotes::Single(b'"', b'"'));
    /// ```
    #[must_use]
    pub const fn csv() -> Self {
        Self {
            delimiter: Delimiter::Char(b','),
            quotes: Quotes::Single(b'"', b'"'),
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
            header: Header::Yes,
            cdx: CdxMode::Optional,
            saw_header: None,
        }
    }

    /// Construct standard TSV parsing options.
    ///
    /// This uses:
    /// - tab delimiter
    /// - no quote handling
    /// - backslash decoding on
    /// - records may continue across physical lines while inside quotes (unused with `Quotes::None`)
    /// - header enabled
    ///
    /// # Examples
    /// ```rust
    /// use cdx::input::{BackslashMode, Config, Delimiter, Quotes};
    ///
    /// let options = Config::tsv();
    /// assert_eq!(options.delimiter, Delimiter::Char(b'\t'));
    /// assert_eq!(options.quotes, Quotes::None);
    /// assert_eq!(options.backslash, BackslashMode::On);
    /// ```
    #[must_use]
    pub const fn tsv() -> Self {
        Self {
            delimiter: Delimiter::Char(b'\t'),
            quotes: Quotes::None,
            backslash: BackslashMode::On,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
            header: Header::Yes,
            cdx: CdxMode::Optional,
            saw_header: None,
        }
    }

    /// Construct whole-line parsing options.
    ///
    /// This uses:
    /// - whole-line delimiter mode (one column per physical line)
    /// - no quote handling
    /// - backslash decoding off
    /// - end record at end-of-line (`UnterminatedQuoteMode::EndAtEol`)
    /// - header enabled
    ///
    /// # Examples
    /// ```rust
    /// use cdx::input::{Config, Delimiter, UnterminatedQuoteMode};
    ///
    /// let options = Config::whole();
    /// assert_eq!(options.delimiter, Delimiter::Whole);
    /// assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::EndAtEol);
    /// ```
    #[must_use]
    pub const fn whole() -> Self {
        Self {
            delimiter: Delimiter::Whole,
            quotes: Quotes::None,
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::EndAtEol,
            header: Header::Yes,
            cdx: CdxMode::Optional,
            saw_header: None,
        }
    }

    /// Construct Common Log Format (CLF)-style parsing options.
    ///
    /// This uses:
    /// - space delimiter
    /// - multi-quote handling for `"`...`"` and `[`...`]`
    /// - backslash decoding on
    /// - end record at end-of-line (`UnterminatedQuoteMode::EndAtEol`)
    /// - header enabled
    ///
    /// # Examples
    /// ```rust
    /// use cdx::input::{BackslashMode, Config, Delimiter, Quotes, UnterminatedQuoteMode};
    ///
    /// let options = Config::clf();
    /// assert_eq!(options.delimiter, Delimiter::Char(b' '));
    /// assert_eq!(options.quotes, Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]));
    /// assert_eq!(options.backslash, BackslashMode::On);
    /// assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::EndAtEol);
    /// ```
    #[must_use]
    pub fn clf() -> Self {
        Self {
            delimiter: Delimiter::Char(b' '),
            quotes: Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]),
            backslash: BackslashMode::On,
            unterminated_quote: UnterminatedQuoteMode::EndAtEol,
            header: Header::Yes,
            cdx: CdxMode::Optional,
            saw_header: None,
        }
    }

    /// Parse a command-line style config specification.
    ///
    /// Format:
    /// - `base,key=value,key=value`
    /// - `base` is one of `csv`, `tsv`, `whole`, `clf`, or `log` (`log` aliases `clf`)
    ///
    /// Supported keys:
    /// - `d` / `delimiter`
    /// - `q` / `quotes`
    /// - `b` / `backslash`
    /// - `u` / `unterminated`
    /// - `h` / `header`
    /// - `x` / `cdx`
    pub fn from_spec(spec: &str) -> anyhow::Result<Self> {
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Err(anyhow::anyhow!("empty input config spec"));
        }

        let mut parts = trimmed.split(',').map(str::trim);
        let base = parts.next().ok_or_else(|| anyhow::anyhow!("missing input config base"))?;

        let base_norm = normalize_token(base);
        let mut config = match base_norm.as_str() {
            "csv" => Self::csv(),
            "tsv" => Self::tsv(),
            "whole" => Self::whole(),
            "clf" | "log" => Self::clf(),
            _ => {
                return Err(anyhow::anyhow!(format!(
                    "unknown input config base `{base}`; expected one of: csv, tsv, whole, clf, log"
                )));
            }
        };

        let mut seen_keys: HashSet<&'static str> = HashSet::new();
        for part in parts {
            if part.is_empty() {
                return Err(anyhow::anyhow!("empty override segment in input config spec"));
            }

            let (key_raw, value_raw) = part.split_once('=').ok_or_else(|| {
                anyhow::anyhow!(format!("input config override `{part}` must be in key=value form"))
            })?;

            let key_norm = normalize_token(key_raw);
            let key = canonical_input_key(&key_norm).ok_or_else(|| {
                anyhow::anyhow!(format!(
                    "unknown input config key `{key_raw}`; expected one of: d|delimiter, q|quotes, b|backslash, u|unterminated, h|header, x|cdx"
                ))
            })?;
            if !seen_keys.insert(key) {
                return Err(anyhow::anyhow!(format!("duplicate input config key `{key_raw}`")));
            }

            match key {
                "delimiter" => {
                    config.delimiter = parse_input_delimiter(value_raw)?;
                }
                "quotes" => {
                    config.quotes = parse_input_quotes(value_raw)?;
                }
                "backslash" => {
                    config.backslash = parse_backslash_mode(value_raw)?;
                }
                "unterminated" => {
                    config.unterminated_quote = parse_unterminated_mode(value_raw)?;
                }
                "header" => {
                    config.header = parse_header_mode(value_raw)?;
                }
                "cdx" => {
                    config.cdx = parse_cdx_mode(value_raw)?;
                }
                _ => unreachable!("all input keys are canonicalized above"),
            }
        }

        Ok(config)
    }
}

impl FromStr for Config {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        Self::from_spec(s)
    }
}

/// Normalize a token for case-insensitive matching and dashed/underscored aliases.
fn normalize_token(token: &str) -> String {
    token
        .trim()
        .chars()
        .filter(|ch| *ch != '_' && *ch != '-')
        .flat_map(char::to_lowercase)
        .collect()
}

/// Canonicalize one input-config override key.
fn canonical_input_key(normalized: &str) -> Option<&'static str> {
    match normalized {
        "d" | "delimiter" => Some("delimiter"),
        "q" | "quotes" => Some("quotes"),
        "b" | "backslash" => Some("backslash"),
        "u" | "unterminated" => Some("unterminated"),
        "h" | "header" => Some("header"),
        "x" | "cdx" => Some("cdx"),
        _ => None,
    }
}

/// Parse common single-byte delimiter tokens.
fn parse_byte_token(value: &str, label: &str) -> anyhow::Result<u8> {
    if let Some(prefix) = value.get(..3)
        && prefix.eq_ignore_ascii_case("ch:")
    {
        let literal = &value[3..];
        let bytes = literal.as_bytes();
        if bytes.len() == 1 && bytes[0].is_ascii() {
            return Ok(bytes[0]);
        }
        return Err(anyhow::anyhow!(format!(
            "{label} value `{value}` must use exactly one ASCII character after `ch:`"
        )));
    }

    let trimmed = value.trim();
    match normalize_token(trimmed).as_str() {
        "t" | "tab" => Ok(b'\t'),
        "s" | "space" => Ok(b' '),
        "n" | "lf" | "newline" => Ok(b'\n'),
        "r" | "cr" => Ok(b'\r'),
        "c" | "comma" => Ok(b','),
        "p" | "pipe" => Ok(b'|'),
        "sc" | "semi" | "semicolon" => Ok(b';'),
        _ => {
            let bytes = trimmed.as_bytes();
            if bytes.len() == 1 && bytes[0].is_ascii() {
                Ok(bytes[0])
            } else {
                Err(anyhow::anyhow!(format!(
                    "unknown {label} value `{value}`; expected one of: t|tab, s|space, n|lf|newline, r|cr, c|comma, p|pipe, sc|semi|semicolon, one ASCII character, or ch:<char>"
                )))
            }
        }
    }
}

/// Parse delimiter override value for input config specs.
fn parse_input_delimiter(value: &str) -> anyhow::Result<Delimiter> {
    if let Some(prefix) = value.get(..4)
        && prefix.eq_ignore_ascii_case("str:")
    {
        return Ok(Delimiter::String(value.as_bytes()[4..].to_vec()));
    }

    let normalized = normalize_token(value);
    match normalized.as_str() {
        "whole" => Ok(Delimiter::Whole),
        "wsd" | "whitespace" => Ok(Delimiter::WhitespaceDrop),
        "wsk" | "whitespacekeep" => Ok(Delimiter::WhitespaceKeep),
        _ => parse_byte_token(value, "delimiter")
            .map(Delimiter::Char)
            .map_err(|_| {
                anyhow::anyhow!(format!(
                    "unknown delimiter value `{value}`; expected one of: whole, wsd|whitespace_drop, wsk|whitespace_keep, str:<text>, t|tab, s|space, n|lf|newline, r|cr, c|comma, p|pipe, sc|semi|semicolon, one ASCII character, or ch:<char>"
                ))
            }),
    }
}

/// Parse quotes override value for input config specs.
fn parse_input_quotes(value: &str) -> anyhow::Result<Quotes> {
    if let Some(prefix) = value.get(..5)
        && prefix.eq_ignore_ascii_case("pair:")
    {
        let pair = &value[5..];
        if let [open, close] = pair.as_bytes()
            && open.is_ascii()
            && close.is_ascii()
        {
            return Ok(Quotes::Single(*open, *close));
        }
        return Err(anyhow::anyhow!(format!(
            "quotes value `{value}` must be `pair:<xy>` with exactly two ASCII characters"
        )));
    }

    match normalize_token(value).as_str() {
        "none" => Ok(Quotes::None),
        "dq" | "double" | "doublequote" | "doublequotes" => Ok(Quotes::Single(b'"', b'"')),
        "sq" | "single" | "singlequote" | "singlequotes" => Ok(Quotes::Single(b'\'', b'\'')),
        "clf" | "log" => Ok(Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')])),
        _ => Err(anyhow::anyhow!(format!(
            "unknown quotes value `{value}`; expected one of: none, dq|double|doublequote, sq|single|singlequote, clf|log, or pair:<xy>"
        ))),
    }
}

/// Parse backslash-mode override value for input config specs.
fn parse_backslash_mode(value: &str) -> anyhow::Result<BackslashMode> {
    match normalize_token(value).as_str() {
        "on" | "1" | "true" => Ok(BackslashMode::On),
        "off" | "0" | "false" => Ok(BackslashMode::Off),
        _ => Err(anyhow::anyhow!(format!(
            "unknown backslash value `{value}`; expected one of: on, off, 1, 0, true, false"
        ))),
    }
}

/// Parse unterminated-quote override value for input config specs.
fn parse_unterminated_mode(value: &str) -> anyhow::Result<UnterminatedQuoteMode> {
    match normalize_token(value).as_str() {
        "cont" | "continue" | "continuerecord" => Ok(UnterminatedQuoteMode::ContinueRecord),
        "eol" | "endateol" => Ok(UnterminatedQuoteMode::EndAtEol),
        "err" | "error" => Ok(UnterminatedQuoteMode::Error),
        _ => Err(anyhow::anyhow!(format!(
            "unknown unterminated value `{value}`; expected one of: cont|continue, eol|endateol, err|error"
        ))),
    }
}

/// Parse header-mode override value for input config specs.
fn parse_header_mode(value: &str) -> anyhow::Result<Header> {
    match normalize_token(value).as_str() {
        "y" | "yes" | "true" | "on" | "1" => Ok(Header::Yes),
        "n" | "no" | "false" | "off" | "0" => Ok(Header::No),
        _ => Err(anyhow::anyhow!(format!(
            "unknown header value `{value}`; expected one of: y|yes|true|on|1, n|no|false|off|0"
        ))),
    }
}

/// Parse CDX-mode override value for input config specs.
fn parse_cdx_mode(value: &str) -> anyhow::Result<CdxMode> {
    match normalize_token(value).as_str() {
        "o" | "optional" => Ok(CdxMode::Optional),
        "r" | "required" => Ok(CdxMode::Required),
        "f" | "forbidden" => Ok(CdxMode::Forbidden),
        "i" | "ignore" => Ok(CdxMode::Ignore),
        _ => Err(anyhow::anyhow!(format!(
            "unknown cdx value `{value}`; expected one of: o|optional, r|required, f|forbidden, i|ignore"
        ))),
    }
}

/// Backwards-compatible alias for older API users.
///
/// Prefer [`Config`].
pub type ParseOptions = Config;

/// Internal escape mode used by parser dispatch paths.
#[derive(Clone, Copy, Default)]
enum Escape {
    /// No escaping is used, i.e. no column value can contain LF, CR or the `Delimiter`
    #[default]
    None,
    /// Individual characters can be backslash escaped, e.g. `\n`, `\r`, `\t`, `\\`, and the delimiter character can be escaped by prefixing it with a backslash.
    /// Unknown escape sequences decode to just the escaped byte, i.e. `\x` becomes `x`.
    Backslash,
    /// Characters can be escaped by wrapping them in a pair of quote characters, e.g. `"foo,bar"` is a single column value containing a comma.
    /// The quote character itself can be escaped by doubling it, e.g. `"foo""bar"` contains a literal quote.
    /// Quoted fields may include line terminators; those bytes are preserved in the field value.
    Quote(u8, u8),
}

#[derive(Default, Debug)]
#[allow(clippy::struct_field_names)]
/// Reusable parser state and output buffer for one parsed record at a time.
pub struct Columns {
    /// Output columns for the last parsed record.
    columns: RecyclableVec<Vec<u8>>,
    /// Scratch buffer used by `WhitespaceKeep` modes to carry pending delimiter bytes.
    pending_whitespace: Vec<u8>,
    /// Scratch buffer used by buffered parsing paths (for example `Delimiter::String`).
    record_buf: Vec<u8>,
    /// Scratch buffer used to read one physical line while building a logical record.
    line_buf: Vec<u8>,
}

/// An input text file
#[derive(Debug)]
pub struct TextFile {
    /// what to read
    pub reader: Infile,
    /// how to interpret
    pub options: Config,
    /// The names of the columns. c1, c1... are auto generated if needed
    pub column_names: Vec<String>,
    /// The column values
    pub column_values: Columns,
    /// Whether EOF has been observed before reading another record.
    pub done: bool,
}

impl TextFile {
    /// Build a `TextFile` from file name and an input parser config.
    pub fn new(input: &str, options: &Config) -> Result<Self> {
        let mut file = Self {
            reader: crate::util::get_reader(input)?,
            options: options.clone(),
            column_names: Vec::default(),
            column_values: Columns::default(),
            done: false,
        };
        file.read_first_line()?;
        Ok(file)
    }

    /// Build a `TextFile` from in-memory text and an input parser config.
    #[must_use]
    pub fn from_string(input: impl Into<String>, options: Config) -> Self {
        let input = input.into();
        Self {
            reader: Infile::from_reader(io::Cursor::new(input.into_bytes()), "<string>"),
            options,
            column_names: Vec::default(),
            column_values: Columns::default(),
            done: false,
        }
    }

    /// The names of the columns
    #[must_use]
    pub fn new_names(&self) -> &[String] {
        &self.column_names
    }

    /// The names of the columns
    #[must_use]
    pub fn names(&self) -> Vec<&str> {
        new_to_old(&self.column_names)
    }

    /// Read the next record into [`TextFile::column_values`].
    ///
    /// Returns `true` when EOF is reached before any more data is read.
    pub fn get_line(&mut self) -> Result<bool> {
        if self.reader.0.fill_buf()?.is_empty() {
            self.done = true;
            return Ok(true);
        }

        self.column_values.read(&mut self.reader, &self.options)?;
        Ok(false)
    }

    /// Was the input file zero bytes?
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.options.saw_header == Some(SawHeader::Empty)
    }

    fn copy_column_names(&mut self) -> Result<()> {
        for v in &self.column_values.columns {
            self.column_names.push(String::from_utf8(v.clone())?);
        }
        Ok(())
    }

    /// Read column names and the first data record according to header and CDX settings.
    pub fn read_first_line(&mut self) -> anyhow::Result<()> {
        if self.options.saw_header.is_some() {
            return Err(anyhow::anyhow!(
                "read_first_line cannot be called after header state is set",
            ));
        }

        let mut first_line = Vec::new();
        if read_physical_line_into(&mut self.reader, &mut first_line)?.is_none() {
            self.options.saw_header = Some(SawHeader::Empty);
            self.done = true;
            return Ok(());
        }

        let has_cdx = first_line.starts_with(b" CDX");
        match (self.options.cdx, has_cdx) {
            (CdxMode::Forbidden, true) => {
                return Err(anyhow::anyhow!("CDX header is forbidden"));
            }
            (CdxMode::Required, false) => {
                return Err(anyhow::anyhow!("CDX header is required"));
            }
            (CdxMode::Optional | CdxMode::Required, true) => {
                let delimiter = *first_line
                    .get(4)
                    .ok_or_else(|| anyhow::anyhow!("CDX header is missing delimiter byte"))?;
                self.options.delimiter = Delimiter::Char(delimiter);
                self.column_values.parse_general_record(&first_line[5..], &self.options);
                self.options.saw_header = Some(SawHeader::Cdx);
                self.copy_column_names()?;
                self.get_line()?;
            }
            (CdxMode::Optional | CdxMode::Forbidden | CdxMode::Ignore, false)
            | (CdxMode::Ignore, true) => match self.options.header {
                Header::Yes => {
                    self.column_values.parse_general_record(&first_line, &self.options);
                    self.options.saw_header = Some(SawHeader::Yes);
                    self.copy_column_names()?;
                    self.get_line()?;
                }
                Header::No => {
                    self.column_values.parse_general_record(&first_line, &self.options);
                    self.column_names =
                        Self::generate_column_names(self.column_values.columns.len());
                    self.options.saw_header = Some(SawHeader::No);
                }
            },
        }

        Ok(())
    }

    /// Return whether [`TextFile::get_line`] has observed EOF.
    #[must_use]
    pub const fn is_done(&self) -> bool {
        self.done
    }

    /// Generate default column names (`c1`, `c2`, ...) matching current values.
    fn generate_column_names(count: usize) -> Vec<String> {
        let mut names = Vec::with_capacity(count);
        for index in 0..count {
            names.push(format!("c{}", index + 1));
        }
        names
    }
}

/// Return bytes to consume for a line terminator at `index`, and whether CR-at-buffer-end needs
/// one-byte lookahead to detect a following LF.
fn terminator_consumed(buf: &[u8], index: usize) -> (usize, bool) {
    if buf[index] == b'\n' {
        return (index + 1, false);
    }

    if index + 1 < buf.len() && buf[index + 1] == b'\n' {
        (index + 2, false)
    } else {
        (index + 1, index + 1 == buf.len())
    }
}
/*
/// Input source and buffered reader used by parser read routines.
pub struct Infile(
    /// Buffered bytes for this input source.
    pub io::BufReader<Box<dyn Read>>,
    /// Source path/name used for diagnostics.
    pub String,
);

impl fmt::Debug for Infile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Infile")
            .field("source_name", &self.1)
            .finish_non_exhaustive()
    }
}

impl Infile {
    /// Build an `Infile` from an existing boxed reader and source name.
    #[must_use]
    pub fn new(reader: io::BufReader<Box<dyn Read>>, source_name: impl Into<String>) -> Self {
        Self(reader, source_name.into())
    }

    /// Build an `Infile` from any readable source using default buffering.
    #[must_use]
    pub fn from_reader<R>(reader: R, source_name: impl Into<String>) -> Self
    where
        R: Read + 'static,
    {
        Self(io::BufReader::new(Box::new(reader)), source_name.into())
    }

    /// Build an `Infile` from any readable source with an explicit buffer capacity.
    #[must_use]
    pub fn with_capacity<R>(capacity: usize, reader: R, source_name: impl Into<String>) -> Self
    where
        R: Read + 'static,
    {
        Self(
            io::BufReader::with_capacity(capacity, Box::new(reader)),
            source_name.into(),
        )
    }
}
*/
/// Consume one LF byte if the previous chunk ended with CR and the next byte is LF.
fn consume_optional_lf(reader: &mut Infile, needs_peek: bool) -> anyhow::Result<()> {
    if !needs_peek {
        return Ok(());
    }

    let next = reader.0.fill_buf()?;
    if !next.is_empty() && next[0] == b'\n' {
        reader.0.consume(1);
    }

    Ok(())
}

/// Physical record terminator observed by `read_physical_line_into`.
#[derive(Clone, Copy)]
enum LineTerminator {
    Lf,
    Cr,
    CrLf,
    Eof,
}

/// Append the byte representation of a line terminator back into an output field value.
fn append_terminator(output: &mut Vec<u8>, terminator: LineTerminator) {
    match terminator {
        LineTerminator::Lf => output.push(b'\n'),
        LineTerminator::Cr => output.push(b'\r'),
        LineTerminator::CrLf => {
            output.push(b'\r');
            output.push(b'\n');
        }
        LineTerminator::Eof => {}
    }
}

/// Update quote-open state for a line fragment using doubled-close escaping semantics.
fn update_quote_state(bytes: &[u8], in_quote: &mut bool, open_quote: u8, close_quote: u8) {
    let mut i = 0;

    while i < bytes.len() {
        let byte = bytes[i];

        if *in_quote {
            if byte == close_quote {
                if i + 1 < bytes.len() && bytes[i + 1] == close_quote {
                    i += 2;
                    continue;
                }
                *in_quote = false;
            }
            i += 1;
            continue;
        }

        if byte == open_quote {
            *in_quote = true;
        }

        i += 1;
    }
}

/// Read one physical line into `output`.
/// Returns `None` at true EOF with no bytes, or `Some(LineTerminator::Eof)` for a final line
/// that has bytes but no trailing line terminator.
fn read_physical_line_into(
    reader: &mut Infile,
    output: &mut Vec<u8>,
) -> anyhow::Result<Option<LineTerminator>> {
    output.clear();

    loop {
        let (consumed, terminator, needs_peek) = {
            let buf = reader.0.fill_buf()?;

            if buf.is_empty() {
                return if output.is_empty() { Ok(None) } else { Ok(Some(LineTerminator::Eof)) };
            }

            if let Some(index) = memchr2(b'\n', b'\r', buf) {
                output.extend_from_slice(&buf[..index]);
                let (consumed, needs_peek) = terminator_consumed(buf, index);
                let terminator = if buf[index] == b'\n' {
                    LineTerminator::Lf
                } else if consumed == index + 2 {
                    LineTerminator::CrLf
                } else {
                    LineTerminator::Cr
                };
                (consumed, Some(terminator), needs_peek)
            } else {
                output.extend_from_slice(buf);
                (buf.len(), None, false)
            }
        };

        reader.0.consume(consumed);

        if let Some(terminator) = terminator {
            if matches!(terminator, LineTerminator::Cr) && needs_peek {
                let consume_lf = {
                    let next = reader.0.fill_buf()?;
                    !next.is_empty() && next[0] == b'\n'
                };
                if consume_lf {
                    reader.0.consume(1);
                    return Ok(Some(LineTerminator::CrLf));
                }
            }
            return Ok(Some(terminator));
        }
    }
}

/// Read one logical record into `record` using the legacy `Escape` contract.
fn read_record_into(
    reader: &mut Infile,
    escape: Escape,
    record: &mut Vec<u8>,
    line: &mut Vec<u8>,
) -> anyhow::Result<bool> {
    record.clear();

    match escape {
        Escape::Quote(open_quote, close_quote) => {
            let mut in_quote = false;

            loop {
                let Some(terminator) = read_physical_line_into(reader, line)? else {
                    return Ok(!record.is_empty());
                };

                update_quote_state(line, &mut in_quote, open_quote, close_quote);
                record.extend_from_slice(line);

                match terminator {
                    LineTerminator::Lf | LineTerminator::Cr | LineTerminator::CrLf if in_quote => {
                        append_terminator(record, terminator);
                    }
                    LineTerminator::Lf
                    | LineTerminator::Cr
                    | LineTerminator::CrLf
                    | LineTerminator::Eof => return Ok(true),
                }
            }
        }
        Escape::None | Escape::Backslash => Ok(read_physical_line_into(reader, record)?.is_some()),
    }
}

/// For a given byte, return the matching close quote if it starts a quote region.
fn quote_close_for_open(quotes: &Quotes, byte: u8) -> Option<u8> {
    match quotes {
        Quotes::None => None,
        Quotes::Single(open, close) => {
            if *open == byte {
                Some(*close)
            } else {
                None
            }
        }
        Quotes::Multi(pairs) => {
            pairs.iter().find_map(|(open, close)| if *open == byte { Some(*close) } else { None })
        }
    }
}

/// Update active quote state for composable parse options.
fn update_quote_state_with_options(
    bytes: &[u8],
    quotes: &Quotes,
    backslash: BackslashMode,
    active_close: &mut Option<u8>,
) {
    let mut i = 0;
    let mut escaped = false;

    while i < bytes.len() {
        let byte = bytes[i];

        if matches!(backslash, BackslashMode::On) {
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if byte == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }
        }

        if let Some(close) = *active_close {
            if byte == close {
                if i + 1 < bytes.len() && bytes[i + 1] == close {
                    i += 2;
                    continue;
                }
                *active_close = None;
            }
            i += 1;
            continue;
        }

        if let Some(close) = quote_close_for_open(quotes, byte) {
            *active_close = Some(close);
        }

        i += 1;
    }
}

/// Read one logical record into `record` using composable [`Config`] quote semantics.
fn read_record_with_options(
    reader: &mut Infile,
    quotes: &Quotes,
    backslash: BackslashMode,
    unterminated: UnterminatedQuoteMode,
    record: &mut Vec<u8>,
    line: &mut Vec<u8>,
) -> anyhow::Result<bool> {
    record.clear();

    if matches!(quotes, Quotes::None) {
        return Ok(read_physical_line_into(reader, record)?.is_some());
    }

    let mut active_close = None;

    loop {
        let Some(terminator) = read_physical_line_into(reader, line)? else {
            return Ok(!record.is_empty());
        };

        update_quote_state_with_options(line, quotes, backslash, &mut active_close);
        record.extend_from_slice(line);

        match terminator {
            LineTerminator::Lf | LineTerminator::Cr | LineTerminator::CrLf => {
                if active_close.is_none() {
                    return Ok(true);
                }

                match unterminated {
                    UnterminatedQuoteMode::ContinueRecord => append_terminator(record, terminator),
                    UnterminatedQuoteMode::EndAtEol => return Ok(true),
                    UnterminatedQuoteMode::Error => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "unterminated quote at end of line",
                        )
                        .into());
                    }
                }
            }
            LineTerminator::Eof => return Ok(true),
        }
    }
}

impl Columns {
    /// Construct an empty reusable parser container.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the columns parsed from the last record.
    #[must_use]
    pub fn columns(&self) -> &[Vec<u8>] {
        &self.columns
    }

    /// Ensure `columns[index]` exists and reset it for writing.
    fn clear_column(&mut self, index: usize) {
        if index == self.columns.len() {
            self.columns.push(Vec::new());
        }
        self.columns[index].clear();
    }

    /// Truncate output to the number of parsed columns for the current record.
    fn finish_columns(&mut self, used: usize) {
        self.columns.truncate(used);
    }

    /// Return true when `delimiter` fully matches the record at `index`.
    fn delimiter_matches_at(record: &[u8], index: usize, delimiter: &[u8]) -> bool {
        !delimiter.is_empty()
            && index + delimiter.len() <= record.len()
            && &record[index..index + delimiter.len()] == delimiter
    }

    /// Parse a buffered record with a string delimiter and no escapes.
    ///
    /// Fast path notes:
    /// - `delimiter.len() == 1` uses `memchr` to split spans in bulk.
    /// - `delimiter.len() > 1` scans only candidate start bytes and verifies full matches.
    fn parse_string_none_record(&mut self, record: &[u8], delimiter: &[u8]) {
        self.clear_column(0);

        if delimiter.is_empty() {
            self.columns[0].extend_from_slice(record);
            self.finish_columns(1);
            return;
        }

        if delimiter.len() == 1 {
            let delim = delimiter[0];
            let mut i = 0usize;
            let mut column_index = 0usize;
            while let Some(found_rel) = memchr::memchr(delim, &record[i..]) {
                let found = i + found_rel;
                if found > i {
                    self.columns[column_index].extend_from_slice(&record[i..found]);
                }
                column_index += 1;
                self.clear_column(column_index);
                i = found + 1;
            }
            self.columns[column_index].extend_from_slice(&record[i..]);
            self.finish_columns(column_index + 1);
            return;
        }

        let delimiter_first = delimiter[0];
        let mut i = 0usize;
        let mut column_index = 0usize;
        while i < record.len() {
            let Some(found_rel) = memchr::memchr(delimiter_first, &record[i..]) else {
                self.columns[column_index].extend_from_slice(&record[i..]);
                break;
            };

            let found = i + found_rel;
            if found > i {
                self.columns[column_index].extend_from_slice(&record[i..found]);
            }

            if Self::delimiter_matches_at(record, found, delimiter) {
                column_index += 1;
                self.clear_column(column_index);
                i = found + delimiter.len();
                continue;
            }

            self.columns[column_index].push(record[found]);
            i = found + 1;
        }

        self.finish_columns(column_index + 1);
    }

    /// Parse a buffered record with a string delimiter and backslash escapes.
    fn parse_string_backslash_record(&mut self, record: &[u8], delimiter: &[u8]) {
        self.clear_column(0);

        if delimiter.is_empty() {
            let mut i = 0;
            let mut escaped = false;
            while i < record.len() {
                let byte = record[i];
                if escaped {
                    match byte {
                        b'n' => self.columns[0].push(b'\n'),
                        b'r' => self.columns[0].push(b'\r'),
                        b't' => self.columns[0].push(b'\t'),
                        b'\\' => self.columns[0].push(b'\\'),
                        _ => self.columns[0].push(byte),
                    }
                    escaped = false;
                    i += 1;
                    continue;
                }
                if byte == b'\\' {
                    escaped = true;
                    i += 1;
                    continue;
                }
                self.columns[0].push(byte);
                i += 1;
            }
            if escaped {
                self.columns[0].push(b'\\');
            }
            self.finish_columns(1);
            return;
        }

        let mut i = 0;
        let mut column_index = 0usize;
        let mut escaped = false;
        while i < record.len() {
            let byte = record[i];

            if escaped {
                match byte {
                    b'n' => self.columns[column_index].push(b'\n'),
                    b'r' => self.columns[column_index].push(b'\r'),
                    b't' => self.columns[column_index].push(b'\t'),
                    b'\\' => self.columns[column_index].push(b'\\'),
                    _ => self.columns[column_index].push(byte),
                }
                escaped = false;
                i += 1;
                continue;
            }

            if byte == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }

            if Self::delimiter_matches_at(record, i, delimiter) {
                column_index += 1;
                self.clear_column(column_index);
                i += delimiter.len();
                continue;
            }

            self.columns[column_index].push(byte);
            i += 1;
        }

        if escaped {
            self.columns[column_index].push(b'\\');
        }

        self.finish_columns(column_index + 1);
    }

    /// Parse a buffered record with a string delimiter and quote escapes.
    fn parse_string_quote_record(
        &mut self,
        record: &[u8],
        delimiter: &[u8],
        open_quote: u8,
        close_quote: u8,
    ) {
        self.clear_column(0);

        if delimiter.is_empty() {
            let mut i = 0;
            let mut in_quote = false;
            while i < record.len() {
                let byte = record[i];
                if in_quote {
                    if byte == close_quote {
                        if i + 1 < record.len() && record[i + 1] == close_quote {
                            self.columns[0].push(close_quote);
                            i += 2;
                            continue;
                        }
                        in_quote = false;
                        i += 1;
                        continue;
                    }
                    self.columns[0].push(byte);
                    i += 1;
                    continue;
                }
                if byte == open_quote {
                    in_quote = true;
                    i += 1;
                    continue;
                }
                self.columns[0].push(byte);
                i += 1;
            }
            self.finish_columns(1);
            return;
        }

        let mut i = 0;
        let mut column_index = 0usize;
        let mut in_quote = false;
        while i < record.len() {
            let byte = record[i];

            if in_quote {
                if byte == close_quote {
                    if i + 1 < record.len() && record[i + 1] == close_quote {
                        self.columns[column_index].push(close_quote);
                        i += 2;
                        continue;
                    }
                    in_quote = false;
                    i += 1;
                    continue;
                }
                self.columns[column_index].push(byte);
                i += 1;
                continue;
            }

            if byte == open_quote {
                in_quote = true;
                i += 1;
                continue;
            }

            if Self::delimiter_matches_at(record, i, delimiter) {
                column_index += 1;
                self.clear_column(column_index);
                i += delimiter.len();
                continue;
            }

            self.columns[column_index].push(byte);
            i += 1;
        }

        self.finish_columns(column_index + 1);
    }

    /// Generic parser used for option combinations not covered by legacy fast paths.
    fn parse_general_record(&mut self, record: &[u8], options: &Config) {
        match &options.delimiter {
            Delimiter::Whole => {
                self.clear_column(0);
                if matches!(options.backslash, BackslashMode::Off) {
                    self.columns[0].extend_from_slice(record);
                } else {
                    let mut i = 0;
                    let mut escaped = false;
                    while i < record.len() {
                        let byte = record[i];
                        if escaped {
                            match byte {
                                b'n' => self.columns[0].push(b'\n'),
                                b'r' => self.columns[0].push(b'\r'),
                                b't' => self.columns[0].push(b'\t'),
                                b'\\' => self.columns[0].push(b'\\'),
                                _ => self.columns[0].push(byte),
                            }
                            escaped = false;
                            i += 1;
                            continue;
                        }
                        if byte == b'\\' {
                            escaped = true;
                            i += 1;
                            continue;
                        }
                        self.columns[0].push(byte);
                        i += 1;
                    }
                    if escaped {
                        self.columns[0].push(b'\\');
                    }
                }
                self.finish_columns(1);
            }
            Delimiter::Char(delimiter_byte) => {
                self.clear_column(0);
                let mut column_index = 0usize;
                let mut i = 0;
                let mut escaped = false;
                let mut active_close = None;

                while i < record.len() {
                    let byte = record[i];

                    if matches!(options.backslash, BackslashMode::On) {
                        if escaped {
                            match byte {
                                b'n' => self.columns[column_index].push(b'\n'),
                                b'r' => self.columns[column_index].push(b'\r'),
                                b't' => self.columns[column_index].push(b'\t'),
                                b'\\' => self.columns[column_index].push(b'\\'),
                                _ => self.columns[column_index].push(byte),
                            }
                            escaped = false;
                            i += 1;
                            continue;
                        }
                        if byte == b'\\' {
                            escaped = true;
                            i += 1;
                            continue;
                        }
                    }

                    if let Some(close) = active_close {
                        if byte == close {
                            if i + 1 < record.len() && record[i + 1] == close {
                                self.columns[column_index].push(close);
                                i += 2;
                                continue;
                            }
                            active_close = None;
                            i += 1;
                            continue;
                        }
                        self.columns[column_index].push(byte);
                        i += 1;
                        continue;
                    }

                    if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                        active_close = Some(close);
                        i += 1;
                        continue;
                    }

                    if byte == *delimiter_byte {
                        column_index += 1;
                        self.clear_column(column_index);
                        i += 1;
                        continue;
                    }

                    self.columns[column_index].push(byte);
                    i += 1;
                }

                if escaped {
                    self.columns[column_index].push(b'\\');
                }

                self.finish_columns(column_index + 1);
            }
            Delimiter::String(delimiter_bytes) => {
                self.clear_column(0);
                let mut column_index = 0usize;
                let mut i = 0;
                let mut escaped = false;
                let mut active_close = None;

                if delimiter_bytes.is_empty() {
                    let mut derived = options.clone();
                    derived.delimiter = Delimiter::Whole;
                    self.parse_general_record(record, &derived);
                    return;
                }

                while i < record.len() {
                    let byte = record[i];

                    if matches!(options.backslash, BackslashMode::On) {
                        if escaped {
                            match byte {
                                b'n' => self.columns[column_index].push(b'\n'),
                                b'r' => self.columns[column_index].push(b'\r'),
                                b't' => self.columns[column_index].push(b'\t'),
                                b'\\' => self.columns[column_index].push(b'\\'),
                                _ => self.columns[column_index].push(byte),
                            }
                            escaped = false;
                            i += 1;
                            continue;
                        }
                        if byte == b'\\' {
                            escaped = true;
                            i += 1;
                            continue;
                        }
                    }

                    if let Some(close) = active_close {
                        if byte == close {
                            if i + 1 < record.len() && record[i + 1] == close {
                                self.columns[column_index].push(close);
                                i += 2;
                                continue;
                            }
                            active_close = None;
                            i += 1;
                            continue;
                        }
                        self.columns[column_index].push(byte);
                        i += 1;
                        continue;
                    }

                    if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                        active_close = Some(close);
                        i += 1;
                        continue;
                    }

                    if Self::delimiter_matches_at(record, i, delimiter_bytes) {
                        column_index += 1;
                        self.clear_column(column_index);
                        i += delimiter_bytes.len();
                        continue;
                    }

                    self.columns[column_index].push(byte);
                    i += 1;
                }

                if escaped {
                    self.columns[column_index].push(b'\\');
                }

                self.finish_columns(column_index + 1);
            }
            Delimiter::WhitespaceDrop | Delimiter::WhitespaceKeep => {
                let keep = matches!(&options.delimiter, Delimiter::WhitespaceKeep);
                self.pending_whitespace.clear();

                let mut fields = 0usize;
                let mut current = 0usize;
                let mut in_field = false;
                let mut i = 0;
                let mut escaped = false;
                let mut active_close = None;

                while i < record.len() {
                    let byte = record[i];

                    if matches!(options.backslash, BackslashMode::On) {
                        if escaped {
                            if !in_field {
                                current = fields;
                                self.clear_column(current);
                                fields += 1;
                                if keep && !self.pending_whitespace.is_empty() {
                                    self.columns[current]
                                        .extend_from_slice(&self.pending_whitespace);
                                    self.pending_whitespace.clear();
                                }
                                in_field = true;
                            }
                            match byte {
                                b'n' => self.columns[current].push(b'\n'),
                                b'r' => self.columns[current].push(b'\r'),
                                b't' => self.columns[current].push(b'\t'),
                                b'\\' => self.columns[current].push(b'\\'),
                                _ => self.columns[current].push(byte),
                            }
                            escaped = false;
                            i += 1;
                            continue;
                        }
                        if byte == b'\\' {
                            escaped = true;
                            i += 1;
                            continue;
                        }
                    }

                    if let Some(close) = active_close {
                        if byte == close {
                            if i + 1 < record.len() && record[i + 1] == close {
                                self.columns[current].push(close);
                                i += 2;
                                continue;
                            }
                            active_close = None;
                            i += 1;
                            continue;
                        }
                        self.columns[current].push(byte);
                        i += 1;
                        continue;
                    }

                    if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            if keep && !self.pending_whitespace.is_empty() {
                                self.columns[current].extend_from_slice(&self.pending_whitespace);
                                self.pending_whitespace.clear();
                            }
                            in_field = true;
                        }
                        active_close = Some(close);
                        i += 1;
                        continue;
                    }

                    if byte.is_ascii_whitespace() {
                        if keep {
                            if in_field {
                                in_field = false;
                                self.pending_whitespace.clear();
                            }
                            self.pending_whitespace.push(byte);
                        } else {
                            in_field = false;
                        }
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if keep && !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                if escaped {
                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if keep && !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                    }
                    self.columns[current].push(b'\\');
                }

                self.pending_whitespace.clear();
                self.finish_columns(fields);
            }
        }
    }

    /// Read one record for `Delimiter::String` using legacy escape behavior.
    fn read_string_delimiter(
        &mut self,
        reader: &mut Infile,
        delimiter: &[u8],
        escape: Escape,
    ) -> anyhow::Result<bool> {
        let mut record = std::mem::take(&mut self.record_buf);
        let mut line = std::mem::take(&mut self.line_buf);
        if !read_record_into(reader, escape, &mut record, &mut line)? {
            self.record_buf = record;
            self.line_buf = line;
            self.finish_columns(0);
            return Ok(false);
        }

        match escape {
            Escape::None => self.parse_string_none_record(&record, delimiter),
            Escape::Backslash => self.parse_string_backslash_record(&record, delimiter),
            Escape::Quote(open_quote, close_quote) => {
                self.parse_string_quote_record(&record, delimiter, open_quote, close_quote);
            }
        }

        self.record_buf = record;
        self.line_buf = line;
        Ok(true)
    }

    /// Optimized path for `Delimiter::Whole` and no escape handling.
    fn read_whole_plain(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        self.clear_column(0);
        let mut saw_any = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    self.finish_columns(1);
                    return Ok(true);
                }

                if let Some(index) = memchr2(b'\n', b'\r', buf) {
                    self.columns[0].extend_from_slice(&buf[..index]);
                    let (consumed, needs_peek) = terminator_consumed(buf, index);
                    (consumed, true, needs_peek)
                } else {
                    self.columns[0].extend_from_slice(buf);
                    (buf.len(), false, false)
                }
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for `Delimiter::Whole` with backslash decoding.
    fn read_whole_backslash(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        self.clear_column(0);
        let mut saw_any = false;
        let mut escaped = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    if escaped {
                        self.columns[0].push(b'\\');
                    }
                    self.finish_columns(1);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if escaped {
                        match byte {
                            b'n' => self.columns[0].push(b'\n'),
                            b'r' => self.columns[0].push(b'\r'),
                            b't' => self.columns[0].push(b'\t'),
                            b'\\' => self.columns[0].push(b'\\'),
                            _ => self.columns[0].push(byte),
                        }
                        escaped = false;
                        i += 1;
                        continue;
                    }

                    if byte == b'\\' {
                        escaped = true;
                        i += 1;
                        continue;
                    }

                    self.columns[0].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                if escaped {
                    self.columns[0].push(b'\\');
                }
                self.finish_columns(1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for `Delimiter::Whole` with quote handling.
    fn read_whole_quote(
        &mut self,
        reader: &mut Infile,
        open_quote: u8,
        close_quote: u8,
    ) -> anyhow::Result<bool> {
        self.clear_column(0);

        let mut saw_any = false;
        let mut in_quote = false;
        let mut pending_close = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    self.finish_columns(1);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if pending_close {
                        if byte == close_quote {
                            self.columns[0].push(byte);
                            pending_close = false;
                            i += 1;
                            continue;
                        }

                        pending_close = false;
                        in_quote = false;
                        continue;
                    }

                    if in_quote {
                        if byte == close_quote {
                            if i + 1 < buf.len() {
                                if buf[i + 1] == close_quote {
                                    self.columns[0].push(byte);
                                    self.columns[0].push(close_quote);
                                    i += 2;
                                    continue;
                                }

                                self.columns[0].push(byte);
                                in_quote = false;
                                i += 1;
                                continue;
                            }

                            self.columns[0].push(byte);
                            pending_close = true;
                            i += 1;
                            continue;
                        }

                        self.columns[0].push(byte);
                        i += 1;
                        continue;
                    }

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte == open_quote {
                        in_quote = true;
                    }

                    self.columns[0].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for single-byte delimiter with no escape handling.
    fn read_char_none(&mut self, reader: &mut Infile, delimiter: u8) -> anyhow::Result<bool> {
        self.clear_column(0);

        let mut saw_any = false;
        let mut column_index = 0usize;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    self.finish_columns(column_index + 1);
                    return Ok(true);
                }

                let mut start = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while start < buf.len() {
                    let Some(found_rel) = memchr3(delimiter, b'\n', b'\r', &buf[start..]) else {
                        self.columns[column_index].extend_from_slice(&buf[start..]);
                        break;
                    };

                    let i = start + found_rel;
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        self.columns[column_index].extend_from_slice(&buf[start..i]);
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    self.columns[column_index].extend_from_slice(&buf[start..i]);
                    column_index += 1;
                    self.clear_column(column_index);
                    start = i + 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(column_index + 1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for single-byte delimiter with backslash decoding.
    fn read_char_backslash(&mut self, reader: &mut Infile, delimiter: u8) -> anyhow::Result<bool> {
        self.clear_column(0);

        let mut saw_any = false;
        let mut column_index = 0usize;
        let mut escaped = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    if escaped {
                        self.columns[column_index].push(b'\\');
                    }

                    self.finish_columns(column_index + 1);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if escaped {
                        match byte {
                            b'n' => self.columns[column_index].push(b'\n'),
                            b'r' => self.columns[column_index].push(b'\r'),
                            b't' => self.columns[column_index].push(b'\t'),
                            b'\\' => self.columns[column_index].push(b'\\'),
                            _ => self.columns[column_index].push(byte),
                        }
                        escaped = false;
                        i += 1;
                        continue;
                    }

                    if byte == b'\\' {
                        escaped = true;
                        i += 1;
                        continue;
                    }

                    if byte == delimiter {
                        column_index += 1;
                        self.clear_column(column_index);
                    } else {
                        self.columns[column_index].push(byte);
                    }

                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                if escaped {
                    self.columns[column_index].push(b'\\');
                }
                self.finish_columns(column_index + 1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for single-byte delimiter with quote handling.
    fn read_char_quote(
        &mut self,
        reader: &mut Infile,
        delimiter: u8,
        open_quote: u8,
        close_quote: u8,
    ) -> anyhow::Result<bool> {
        self.clear_column(0);

        let mut saw_any = false;
        let mut column_index = 0usize;
        let mut in_quote = false;
        let mut pending_close = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    self.finish_columns(column_index + 1);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if pending_close {
                        if byte == close_quote {
                            self.columns[column_index].push(close_quote);
                            pending_close = false;
                            i += 1;
                            continue;
                        }

                        pending_close = false;
                        in_quote = false;
                        continue;
                    }

                    if in_quote {
                        if byte == close_quote {
                            if i + 1 < buf.len() {
                                if buf[i + 1] == close_quote {
                                    self.columns[column_index].push(close_quote);
                                    i += 2;
                                    continue;
                                }

                                in_quote = false;
                                i += 1;
                                continue;
                            }

                            pending_close = true;
                            i += 1;
                            continue;
                        }

                        self.columns[column_index].push(byte);
                        i += 1;
                        continue;
                    }

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte == open_quote {
                        in_quote = true;
                        i += 1;
                        continue;
                    }

                    if byte == delimiter {
                        column_index += 1;
                        self.clear_column(column_index);
                    } else {
                        self.columns[column_index].push(byte);
                    }

                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(column_index + 1);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (drop mode) with no escapes.
    fn read_whitespace_drop_none(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte.is_ascii_whitespace() {
                        in_field = false;
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (drop mode) with backslash decoding.
    fn read_whitespace_drop_backslash(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;
        let mut escaped = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    if escaped {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                        }
                        self.columns[current].push(b'\\');
                    }

                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if escaped {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            in_field = true;
                        }

                        match byte {
                            b'n' => self.columns[current].push(b'\n'),
                            b'r' => self.columns[current].push(b'\r'),
                            b't' => self.columns[current].push(b'\t'),
                            b'\\' => self.columns[current].push(b'\\'),
                            _ => self.columns[current].push(byte),
                        }

                        escaped = false;
                        i += 1;
                        continue;
                    }

                    if byte == b'\\' {
                        escaped = true;
                        i += 1;
                        continue;
                    }

                    if byte.is_ascii_whitespace() {
                        in_field = false;
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                if escaped {
                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                    }
                    self.columns[current].push(b'\\');
                }
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (drop mode) with quote handling.
    fn read_whitespace_drop_quote(
        &mut self,
        reader: &mut Infile,
        open_quote: u8,
        close_quote: u8,
    ) -> anyhow::Result<bool> {
        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;
        let mut in_quote = false;
        let mut pending_close = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if pending_close {
                        if byte == close_quote {
                            self.columns[current].push(close_quote);
                            pending_close = false;
                            i += 1;
                            continue;
                        }

                        pending_close = false;
                        in_quote = false;
                        continue;
                    }

                    if in_quote {
                        if byte == close_quote {
                            if i + 1 < buf.len() {
                                if buf[i + 1] == close_quote {
                                    self.columns[current].push(close_quote);
                                    i += 2;
                                    continue;
                                }

                                in_quote = false;
                                i += 1;
                                continue;
                            }

                            pending_close = true;
                            i += 1;
                            continue;
                        }

                        self.columns[current].push(byte);
                        i += 1;
                        continue;
                    }

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte == open_quote {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            in_field = true;
                        }
                        in_quote = true;
                        i += 1;
                        continue;
                    }

                    if byte.is_ascii_whitespace() {
                        in_field = false;
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (keep mode) with no escapes.
    fn read_whitespace_keep_none(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        self.pending_whitespace.clear();

        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    self.pending_whitespace.clear();
                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte.is_ascii_whitespace() {
                        if in_field {
                            in_field = false;
                            self.pending_whitespace.clear();
                        }
                        self.pending_whitespace.push(byte);
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.pending_whitespace.clear();
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (keep mode) with backslash decoding.
    fn read_whitespace_keep_backslash(&mut self, reader: &mut Infile) -> anyhow::Result<bool> {
        self.pending_whitespace.clear();

        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;
        let mut escaped = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }

                    if escaped {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            if !self.pending_whitespace.is_empty() {
                                self.columns[current].extend_from_slice(&self.pending_whitespace);
                                self.pending_whitespace.clear();
                            }
                        }
                        self.columns[current].push(b'\\');
                    }

                    self.pending_whitespace.clear();
                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if escaped {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            if !self.pending_whitespace.is_empty() {
                                self.columns[current].extend_from_slice(&self.pending_whitespace);
                                self.pending_whitespace.clear();
                            }
                            in_field = true;
                        }

                        match byte {
                            b'n' => self.columns[current].push(b'\n'),
                            b'r' => self.columns[current].push(b'\r'),
                            b't' => self.columns[current].push(b'\t'),
                            b'\\' => self.columns[current].push(b'\\'),
                            _ => self.columns[current].push(byte),
                        }

                        escaped = false;
                        i += 1;
                        continue;
                    }

                    if byte == b'\\' {
                        escaped = true;
                        i += 1;
                        continue;
                    }

                    if byte.is_ascii_whitespace() {
                        if in_field {
                            in_field = false;
                            self.pending_whitespace.clear();
                        }
                        self.pending_whitespace.push(byte);
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                if escaped {
                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                    }
                    self.columns[current].push(b'\\');
                }

                self.pending_whitespace.clear();
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Optimized path for whitespace delimiter (keep mode) with quote handling.
    fn read_whitespace_keep_quote(
        &mut self,
        reader: &mut Infile,
        open_quote: u8,
        close_quote: u8,
    ) -> anyhow::Result<bool> {
        self.pending_whitespace.clear();

        let mut saw_any = false;
        let mut fields = 0usize;
        let mut in_field = false;
        let mut current = 0usize;
        let mut in_quote = false;
        let mut pending_close = false;

        loop {
            let (consumed, done, needs_peek) = {
                let buf = reader.0.fill_buf()?;
                if buf.is_empty() {
                    if !saw_any {
                        self.finish_columns(0);
                        return Ok(false);
                    }
                    self.pending_whitespace.clear();
                    self.finish_columns(fields);
                    return Ok(true);
                }

                let mut i = 0;
                let mut consumed = buf.len();
                let mut done = false;
                let mut needs_peek = false;

                while i < buf.len() {
                    let byte = buf[i];

                    if pending_close {
                        if byte == close_quote {
                            self.columns[current].push(close_quote);
                            pending_close = false;
                            i += 1;
                            continue;
                        }

                        pending_close = false;
                        in_quote = false;
                        continue;
                    }

                    if in_quote {
                        if byte == close_quote {
                            if i + 1 < buf.len() {
                                if buf[i + 1] == close_quote {
                                    self.columns[current].push(close_quote);
                                    i += 2;
                                    continue;
                                }

                                in_quote = false;
                                i += 1;
                                continue;
                            }

                            pending_close = true;
                            i += 1;
                            continue;
                        }

                        self.columns[current].push(byte);
                        i += 1;
                        continue;
                    }

                    if byte == b'\n' || byte == b'\r' {
                        let (c, peek) = terminator_consumed(buf, i);
                        consumed = c;
                        done = true;
                        needs_peek = peek;
                        break;
                    }

                    if byte == open_quote {
                        if !in_field {
                            current = fields;
                            self.clear_column(current);
                            fields += 1;
                            if !self.pending_whitespace.is_empty() {
                                self.columns[current].extend_from_slice(&self.pending_whitespace);
                                self.pending_whitespace.clear();
                            }
                            in_field = true;
                        }
                        in_quote = true;
                        i += 1;
                        continue;
                    }

                    if byte.is_ascii_whitespace() {
                        if in_field {
                            in_field = false;
                            self.pending_whitespace.clear();
                        }
                        self.pending_whitespace.push(byte);
                        i += 1;
                        continue;
                    }

                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if !self.pending_whitespace.is_empty() {
                            self.columns[current].extend_from_slice(&self.pending_whitespace);
                            self.pending_whitespace.clear();
                        }
                        in_field = true;
                    }

                    self.columns[current].push(byte);
                    i += 1;
                }

                (consumed, done, needs_peek)
            };

            reader.0.consume(consumed);
            saw_any = true;

            if done {
                consume_optional_lf(reader, needs_peek)?;
                self.pending_whitespace.clear();
                self.finish_columns(fields);
                return Ok(true);
            }
        }
    }

    /// Convert composable parse options into legacy escape mode when equivalent.
    const fn legacy_escape_from_options(options: &Config) -> Option<Escape> {
        match (&options.quotes, options.backslash) {
            (Quotes::None, BackslashMode::Off) => Some(Escape::None),
            (Quotes::None, BackslashMode::On) => Some(Escape::Backslash),
            (Quotes::Single(open, close), BackslashMode::Off) => Some(Escape::Quote(*open, *close)),
            _ => None,
        }
    }

    /// Read a single record from `reader` and split it into columns according to `options`.
    ///
    /// Record terminators are CR, LF, or CRLF. The terminator ending the record is not included in
    /// any output column. Depending on `options.quotes` and `options.unterminated_quote`,
    /// records may span multiple physical lines.
    ///
    /// Performance: for `Delimiter::Char`, `Delimiter::Whole`, and whitespace delimiters,
    /// parsing is streaming and uses `BufRead::fill_buf`/`consume` directly. For
    /// `Delimiter::String` and advanced quote/backslash combinations, parsing buffers one
    /// logical record and then splits it.
    /// Existing column buffers are reused across calls. If a subsequent record has the same
    /// column count and each field fits in the existing capacities, parsing performs no
    /// allocations.
    pub fn read(&mut self, reader: &mut Infile, options: &Config) -> anyhow::Result<()> {
        if matches!(options.unterminated_quote, UnterminatedQuoteMode::ContinueRecord)
            && let Some(escape) = Self::legacy_escape_from_options(options)
        {
            let read_any = match options.delimiter {
                Delimiter::Whole => match escape {
                    Escape::Quote(open, close) => self.read_whole_quote(reader, open, close)?,
                    Escape::None => self.read_whole_plain(reader)?,
                    Escape::Backslash => self.read_whole_backslash(reader)?,
                },
                Delimiter::String(ref delimiter_bytes) => {
                    self.read_string_delimiter(reader, delimiter_bytes, escape)?
                }
                Delimiter::Char(delimiter_byte) => match escape {
                    Escape::None => self.read_char_none(reader, delimiter_byte)?,
                    Escape::Backslash => self.read_char_backslash(reader, delimiter_byte)?,
                    Escape::Quote(open, close) => {
                        self.read_char_quote(reader, delimiter_byte, open, close)?
                    }
                },
                Delimiter::WhitespaceDrop => match escape {
                    Escape::None => self.read_whitespace_drop_none(reader)?,
                    Escape::Backslash => self.read_whitespace_drop_backslash(reader)?,
                    Escape::Quote(open, close) => {
                        self.read_whitespace_drop_quote(reader, open, close)?
                    }
                },
                Delimiter::WhitespaceKeep => match escape {
                    Escape::None => self.read_whitespace_keep_none(reader)?,
                    Escape::Backslash => self.read_whitespace_keep_backslash(reader)?,
                    Escape::Quote(open, close) => {
                        self.read_whitespace_keep_quote(reader, open, close)?
                    }
                },
            };

            if !read_any {
                self.finish_columns(0);
            }

            return Ok(());
        }

        if !read_record_with_options(
            reader,
            &options.quotes,
            options.backslash,
            options.unterminated_quote,
            &mut self.record_buf,
            &mut self.line_buf,
        )? {
            self.finish_columns(0);
            return Ok(());
        }

        let record = std::mem::take(&mut self.record_buf);
        self.parse_general_record(&record, options);
        self.record_buf = record;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rvec;
    use std::collections::HashMap;
    use std::io::Cursor;

    /// Build equivalent composable options for legacy delimiter/escape pairs used in tests.
    fn legacy_options(delimiter: Delimiter, escape: Escape) -> Config {
        let mut options = Config::new(delimiter);
        match escape {
            Escape::None => {
                options.quotes = Quotes::None;
                options.backslash = BackslashMode::Off;
            }
            Escape::Backslash => {
                options.quotes = Quotes::None;
                options.backslash = BackslashMode::On;
            }
            Escape::Quote(open, close) => {
                options.quotes = Quotes::Single(open, close);
                options.backslash = BackslashMode::Off;
            }
        }
        options
    }

    /// Compatibility helper for existing test cases expressed in legacy parameters.
    fn read_legacy(
        columns: &mut Columns,
        reader: &mut Infile,
        delimiter: Delimiter,
        escape: Escape,
    ) -> anyhow::Result<()> {
        columns.read(reader, &legacy_options(delimiter, escape))
    }

    /// Build an in-memory [`Infile`] for tests from raw bytes.
    fn test_infile(data: &[u8]) -> Infile {
        Infile::from_reader(Cursor::new(data.to_vec()), "<test-bytes>")
    }

    /// Verifies `TextFile::from_string` builds an in-memory reader with default column buffers.
    #[test]
    fn text_file_from_string_uses_config_and_defaults() {
        let options = Config::tsv();
        let mut text_file = TextFile::from_string("a\tb\n", options.clone());

        assert_eq!(text_file.options, options);
        assert!(text_file.column_names.is_empty());
        assert!(text_file.column_values.columns().is_empty());
        assert!(!text_file.is_done());

        text_file.column_values.read(&mut text_file.reader, &text_file.options).unwrap();
        assert_eq!(text_file.column_values.columns(), &[b"a".to_vec(), b"b".to_vec()]);
    }

    /// Verifies `TextFile::get_line` reports EOF only when no record was read.
    #[test]
    fn text_file_get_line_reads_until_done() {
        let mut text_file = TextFile::from_string("a\tb\nc\td", Config::tsv());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"a".to_vec(), b"b".to_vec()]);
        assert!(!text_file.is_done());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"c".to_vec(), b"d".to_vec()]);
        assert!(!text_file.is_done());

        assert!(text_file.get_line().unwrap());
        assert!(text_file.is_done());
        assert!(text_file.get_line().unwrap());
        assert!(text_file.is_done());
    }

    /// Verifies `TextFile::get_line` reads newline-terminated records before reporting EOF.
    #[test]
    fn text_file_get_line_reads_newline_terminated_lines() {
        let mut text_file = TextFile::from_string("a\tb\nc\td\ne\tf\n", Config::tsv());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"a".to_vec(), b"b".to_vec()]);
        assert!(!text_file.is_done());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"c".to_vec(), b"d".to_vec()]);
        assert!(!text_file.is_done());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"e".to_vec(), b"f".to_vec()]);
        assert!(!text_file.is_done());

        assert!(text_file.get_line().unwrap());
        assert!(text_file.is_done());
    }

    /// Verifies `TextFile::get_line` reads a final record with no trailing CR or LF.
    #[test]
    fn text_file_get_line_reads_final_line_without_line_ending() {
        let mut text_file = TextFile::from_string("a\tb\nc\td\ne\tf", Config::tsv());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"a".to_vec(), b"b".to_vec()]);

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"c".to_vec(), b"d".to_vec()]);

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"e".to_vec(), b"f".to_vec()]);
        assert!(!text_file.is_done());

        assert!(text_file.get_line().unwrap());
        assert!(text_file.is_done());
    }

    /// Verifies `TextFile::get_line` reports EOF immediately for empty input.
    #[test]
    fn text_file_get_line_empty_input_is_done() {
        let mut text_file = TextFile::from_string("", Config::tsv());

        assert!(!text_file.is_done());
        assert!(text_file.column_values.columns().is_empty());
        assert!(text_file.get_line().unwrap());
        assert!(text_file.is_done());
        assert!(text_file.column_values.columns().is_empty());
    }

    /// Verifies `TextFile::read_first_line` uses the first line as names when headers are enabled.
    #[test]
    fn text_file_read_first_line_header_yes_reads_names_then_values() {
        let mut options = Config::tsv();
        options.header = Header::Yes;
        let mut text_file = TextFile::from_string("left\tright\n1\t2\n3\t4\n", options);

        text_file.read_first_line().unwrap();
        assert_eq!(text_file.options.saw_header, Some(SawHeader::Yes));
        assert_eq!(text_file.column_names, &["left".to_string(), "right".to_string()]);
        assert_eq!(text_file.column_values.columns(), &[b"1".to_vec(), b"2".to_vec()]);
        assert!(!text_file.is_done());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"3".to_vec(), b"4".to_vec()]);
    }

    /// Verifies `TextFile::read_first_line` generates names when headers are disabled.
    #[test]
    fn text_file_read_first_line_header_no_generates_names() {
        let mut options = Config::tsv();
        options.header = Header::No;
        let mut text_file = TextFile::from_string("1\t2\n3\t4\n", options);

        text_file.read_first_line().unwrap();
        assert_eq!(text_file.options.saw_header, Some(SawHeader::No));
        assert_eq!(text_file.column_names, &["c1".to_string(), "c2".to_string()]);
        assert_eq!(text_file.column_values.columns(), &[b"1".to_vec(), b"2".to_vec()]);
        assert!(!text_file.is_done());

        assert!(!text_file.get_line().unwrap());
        assert_eq!(text_file.column_values.columns(), &[b"3".to_vec(), b"4".to_vec()]);
    }

    /// Verifies `TextFile::read_first_line` marks empty input without reading names or values.
    #[test]
    fn text_file_read_first_line_empty_input_sets_empty_header() {
        let mut text_file = TextFile::from_string("", Config::tsv());

        text_file.read_first_line().unwrap();
        assert_eq!(text_file.options.saw_header, Some(SawHeader::Empty));
        assert!(text_file.column_names.is_empty());
        assert!(text_file.column_values.columns().is_empty());
        assert!(text_file.is_done());
    }

    /// Verifies optional CDX updates the delimiter and treats the CDX line as column names.
    #[test]
    fn text_file_read_first_line_optional_cdx_uses_cdx_header() {
        let mut options = Config::tsv();
        options.header = Header::No;
        options.cdx = CdxMode::Optional;
        let mut text_file = TextFile::from_string(" CDX,alpha,beta\n1,2\n", options);

        text_file.read_first_line().unwrap();
        assert_eq!(text_file.options.saw_header, Some(SawHeader::Cdx));
        assert_eq!(text_file.options.delimiter, Delimiter::Char(b','));
        assert_eq!(text_file.column_names, &["alpha".to_string(), "beta".to_string()]);
        assert_eq!(text_file.column_values.columns(), &[b"1".to_vec(), b"2".to_vec()]);
    }

    /// Verifies required CDX rejects input without a CDX header.
    #[test]
    fn text_file_read_first_line_required_cdx_rejects_absent_cdx() {
        let mut options = Config::tsv();
        options.cdx = CdxMode::Required;
        let mut text_file = TextFile::from_string("alpha\tbeta\n1\t2\n", options);

        let err = text_file.read_first_line().unwrap_err();
        assert!(err.to_string().contains("required"));
        assert_eq!(text_file.options.saw_header, None);
    }

    /// Verifies forbidden CDX rejects input with a CDX header.
    #[test]
    fn text_file_read_first_line_forbidden_cdx_rejects_present_cdx() {
        let mut options = Config::tsv();
        options.cdx = CdxMode::Forbidden;
        let mut text_file = TextFile::from_string(" CDX,alpha,beta\n1,2\n", options);

        let err = text_file.read_first_line().unwrap_err();
        assert!(err.to_string().contains("forbidden"));
        assert_eq!(text_file.options.saw_header, None);
    }

    /// Verifies ignored CDX is parsed like an ordinary first line.
    #[test]
    fn text_file_read_first_line_ignore_cdx_treats_line_normally() {
        let mut options = Config::tsv();
        options.header = Header::Yes;
        options.cdx = CdxMode::Ignore;
        let mut text_file = TextFile::from_string(" CDX,alpha,beta\n1,2\n", options);

        text_file.read_first_line().unwrap();
        assert_eq!(text_file.options.saw_header, Some(SawHeader::Yes));
        assert_eq!(text_file.options.delimiter, Delimiter::Char(b'\t'));
        assert_eq!(text_file.column_names, &[" CDX,alpha,beta".to_string()]);
        assert_eq!(text_file.column_values.columns(), &[b"1,2".to_vec()]);
    }

    /// Verifies `TextFile::read_first_line` can only initialize header state once.
    #[test]
    fn text_file_read_first_line_rejects_second_call() {
        let mut text_file = TextFile::from_string("a\tb\n1\t2\n", Config::tsv());

        text_file.read_first_line().unwrap();
        let err = text_file.read_first_line().unwrap_err();
        assert!(err.to_string().contains("header state"));
    }

    /// Parse all records from `data` with one [`Config`] value.
    fn read_all_with_options(data: &[u8], options: &Config) -> anyhow::Result<Vec<RecyclableVec<Vec<u8>>>> {
        let mut reader = test_infile(data);
        let mut columns = Columns::new();
        let mut records = Vec::new();

        loop {
            if reader.0.fill_buf()?.is_empty() {
                break;
            }
            columns.read(&mut reader, options)?;
            records.push(columns.columns.clone());
        }

        Ok(records)
    }

    /// Test-time delimiter choices used by 20-way matrix cases.
    #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
    enum DelimiterConfig {
        CharTab,
        CharComma,
        Whole,
        WhitespaceDrop,
        WhitespaceKeep,
    }

    /// Test-time escape choices used by 20-way matrix cases.
    #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
    enum EscapeConfig {
        None,
        Backslash,
        QuoteDouble,
        QuoteAngle,
    }

    /// One parser configuration in the 20-way matrix.
    #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
    struct ParseConfig {
        delimiter: DelimiterConfig,
        escape: EscapeConfig,
    }

    /// All delimiter/escape combinations exercised by matrix tests.
    const ALL_20_CONFIGS: [ParseConfig; 20] = [
        ParseConfig { delimiter: DelimiterConfig::CharTab, escape: EscapeConfig::None },
        ParseConfig { delimiter: DelimiterConfig::CharTab, escape: EscapeConfig::Backslash },
        ParseConfig { delimiter: DelimiterConfig::CharTab, escape: EscapeConfig::QuoteDouble },
        ParseConfig { delimiter: DelimiterConfig::CharTab, escape: EscapeConfig::QuoteAngle },
        ParseConfig { delimiter: DelimiterConfig::CharComma, escape: EscapeConfig::None },
        ParseConfig { delimiter: DelimiterConfig::CharComma, escape: EscapeConfig::Backslash },
        ParseConfig { delimiter: DelimiterConfig::CharComma, escape: EscapeConfig::QuoteDouble },
        ParseConfig { delimiter: DelimiterConfig::CharComma, escape: EscapeConfig::QuoteAngle },
        ParseConfig { delimiter: DelimiterConfig::Whole, escape: EscapeConfig::None },
        ParseConfig { delimiter: DelimiterConfig::Whole, escape: EscapeConfig::Backslash },
        ParseConfig { delimiter: DelimiterConfig::Whole, escape: EscapeConfig::QuoteDouble },
        ParseConfig { delimiter: DelimiterConfig::Whole, escape: EscapeConfig::QuoteAngle },
        ParseConfig { delimiter: DelimiterConfig::WhitespaceDrop, escape: EscapeConfig::None },
        ParseConfig { delimiter: DelimiterConfig::WhitespaceDrop, escape: EscapeConfig::Backslash },
        ParseConfig {
            delimiter: DelimiterConfig::WhitespaceDrop,
            escape: EscapeConfig::QuoteDouble,
        },
        ParseConfig {
            delimiter: DelimiterConfig::WhitespaceDrop,
            escape: EscapeConfig::QuoteAngle,
        },
        ParseConfig { delimiter: DelimiterConfig::WhitespaceKeep, escape: EscapeConfig::None },
        ParseConfig { delimiter: DelimiterConfig::WhitespaceKeep, escape: EscapeConfig::Backslash },
        ParseConfig {
            delimiter: DelimiterConfig::WhitespaceKeep,
            escape: EscapeConfig::QuoteDouble,
        },
        ParseConfig {
            delimiter: DelimiterConfig::WhitespaceKeep,
            escape: EscapeConfig::QuoteAngle,
        },
    ];

    /// Build runtime delimiter value from matrix config.
    fn make_delimiter(config: DelimiterConfig) -> Delimiter {
        match config {
            DelimiterConfig::CharTab => Delimiter::Char(b'\t'),
            DelimiterConfig::CharComma => Delimiter::Char(b','),
            DelimiterConfig::Whole => Delimiter::Whole,
            DelimiterConfig::WhitespaceDrop => Delimiter::WhitespaceDrop,
            DelimiterConfig::WhitespaceKeep => Delimiter::WhitespaceKeep,
        }
    }

    /// Build runtime escape value from matrix config.
    fn make_escape(config: EscapeConfig) -> Escape {
        match config {
            EscapeConfig::None => Escape::None,
            EscapeConfig::Backslash => Escape::Backslash,
            EscapeConfig::QuoteDouble => Escape::Quote(b'"', b'"'),
            EscapeConfig::QuoteAngle => Escape::Quote(b'<', b'>'),
        }
    }

    /// Human-readable label for assertion failures in matrix tests.
    fn config_name(config: ParseConfig) -> &'static str {
        match (config.delimiter, config.escape) {
            (DelimiterConfig::CharTab, EscapeConfig::None) => "Char(Tab)+None",
            (DelimiterConfig::CharTab, EscapeConfig::Backslash) => "Char(Tab)+Backslash",
            (DelimiterConfig::CharTab, EscapeConfig::QuoteDouble) => "Char(Tab)+Quote(\"\"\")",
            (DelimiterConfig::CharTab, EscapeConfig::QuoteAngle) => "Char(Tab)+Quote(<> )",
            (DelimiterConfig::CharComma, EscapeConfig::None) => "Char(,)+None",
            (DelimiterConfig::CharComma, EscapeConfig::Backslash) => "Char(,)+Backslash",
            (DelimiterConfig::CharComma, EscapeConfig::QuoteDouble) => "Char(,)+Quote(\"\"\")",
            (DelimiterConfig::CharComma, EscapeConfig::QuoteAngle) => "Char(,)+Quote(<> )",
            (DelimiterConfig::Whole, EscapeConfig::None) => "Whole+None",
            (DelimiterConfig::Whole, EscapeConfig::Backslash) => "Whole+Backslash",
            (DelimiterConfig::Whole, EscapeConfig::QuoteDouble) => "Whole+Quote(\"\"\")",
            (DelimiterConfig::Whole, EscapeConfig::QuoteAngle) => "Whole+Quote(<> )",
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::None) => "WhitespaceDrop+None",
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::Backslash) => {
                "WhitespaceDrop+Backslash"
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteDouble) => {
                "WhitespaceDrop+Quote(\"\"\")"
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteAngle) => {
                "WhitespaceDrop+Quote(<> )"
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::None) => "WhitespaceKeep+None",
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::Backslash) => {
                "WhitespaceKeep+Backslash"
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteDouble) => {
                "WhitespaceKeep+Quote(\"\"\")"
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteAngle) => {
                "WhitespaceKeep+Quote(<> )"
            }
        }
    }

    /// Record list produced while parsing one full input under one config.
    type Records = Vec<RecyclableVec<Vec<u8>>>;

    /// Parse the same input in all 20 configurations and collect every produced record.
    fn parse_all_20(data: &[u8]) -> HashMap<ParseConfig, Records> {
        let mut all = HashMap::new();

        for config in ALL_20_CONFIGS {
            let mut reader = test_infile(data);
            let mut columns = Columns::new();
            let mut records = Vec::new();

            loop {
                if reader.0.fill_buf().unwrap().is_empty() {
                    break;
                }
                read_legacy(
                    &mut columns,
                    &mut reader,
                    make_delimiter(config.delimiter),
                    make_escape(config.escape),
                )
                .unwrap();
                records.push(columns.columns.clone());
            }

            all.insert(config, records);
        }

        all
    }

    /// Assert that all 20 parse configurations match config-specific expectations.
    fn assert_all_20<F>(data: &[u8], expected: F)
    where
        F: Fn(ParseConfig) -> Records,
    {
        let actual = parse_all_20(data);
        for config in ALL_20_CONFIGS {
            let got = actual.get(&config).unwrap();
            let want = expected(config);
            assert_eq!(got, &want, "Mismatch for config {}", config_name(config));
        }
    }

    /// Matrix case covering baseline delimiter behavior.
    #[test]
    fn parse_20_configs_case_1_basic_delimiters() {
        let data = b"\"aa\",bb\tcc dd\n";

        assert_all_20(data, |config| match (config.delimiter, config.escape) {
            (DelimiterConfig::Whole, _) => vec![rvec![b"\"aa\",bb\tcc dd".to_vec()]],
            (DelimiterConfig::CharComma, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"aa".to_vec(), b"bb\tcc dd".to_vec()]]
            }
            (DelimiterConfig::CharComma, _) => {
                vec![rvec![b"\"aa\"".to_vec(), b"bb\tcc dd".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"aa,bb".to_vec(), b"cc dd".to_vec()]]
            }
            (DelimiterConfig::CharTab, _) => {
                vec![rvec![b"\"aa\",bb".to_vec(), b"cc dd".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"aa,bb".to_vec(), b"cc".to_vec(), b"dd".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, _) => {
                vec![rvec![b"\"aa\",bb".to_vec(), b"cc".to_vec(), b"dd".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"aa,bb".to_vec(), b"\tcc".to_vec(), b" dd".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, _) => {
                vec![rvec![b"\"aa\",bb".to_vec(), b"\tcc".to_vec(), b" dd".to_vec()]]
            }
        });
    }

    /// Matrix case covering backslash escape decoding differences.
    #[test]
    fn parse_20_configs_case_2_backslash_sequences() {
        let data = b"\"a\\n\"\tb\\t\\n\\q\n";

        assert_all_20(data, |config| match (config.delimiter, config.escape) {
            (DelimiterConfig::Whole, EscapeConfig::None) => {
                vec![rvec![b"\"a\\n\"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::Whole, EscapeConfig::Backslash) => {
                vec![rvec![b"\"a\n\"\tb\t\nq".to_vec()]]
            }
            (DelimiterConfig::Whole, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"\"a\\n\"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::Whole, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a\\n\"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::None) => {
                vec![rvec![b"\"a\\n\"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::Backslash) => {
                vec![rvec![b"\"a\n\"\tb\t\nq".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"a\\n\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a\\n\"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::None) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::Backslash) => {
                vec![rvec![b"\"a\n\"".to_vec(), b"b\t\nq".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"a\\n".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::None) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::Backslash) => {
                vec![rvec![b"\"a\n\"".to_vec(), b"b\t\nq".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"a\\n".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"b\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::None) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::Backslash) => {
                vec![rvec![b"\"a\n\"".to_vec(), b"\tb\t\nq".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"a\\n".to_vec(), b"\tb\\t\\n\\q".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a\\n\"".to_vec(), b"\tb\\t\\n\\q".to_vec()]]
            }
        });
    }

    /// Matrix case showing differences between quote styles.
    #[test]
    fn parse_20_configs_case_3_quote_style_difference() {
        let data = b"\"p,q\",<r,s>\n";

        assert_all_20(data, |config| match (config.delimiter, config.escape) {
            (
                DelimiterConfig::Whole,
                EscapeConfig::None
                | EscapeConfig::Backslash
                | EscapeConfig::QuoteDouble
                | EscapeConfig::QuoteAngle,
            ) => {
                vec![rvec![b"\"p,q\",<r,s>".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::None | EscapeConfig::Backslash) => {
                vec![rvec![b"\"p".to_vec(), b"q\"".to_vec(), b"<r".to_vec(), b"s>".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"p,q".to_vec(), b"<r".to_vec(), b"s>".to_vec()]]
            }
            (DelimiterConfig::CharComma, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"p".to_vec(), b"q\"".to_vec(), b"r,s".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::None | EscapeConfig::Backslash) => {
                vec![rvec![b"\"p,q\",<r,s>".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"p,q,<r,s>".to_vec()]]
            }
            (DelimiterConfig::CharTab, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"p,q\",r,s".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::None | EscapeConfig::Backslash) => {
                vec![rvec![b"\"p,q\",<r,s>".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"p,q,<r,s>".to_vec()]]
            }
            (DelimiterConfig::WhitespaceDrop, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"p,q\",r,s".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::None | EscapeConfig::Backslash) => {
                vec![rvec![b"\"p,q\",<r,s>".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"p,q,<r,s>".to_vec()]]
            }
            (DelimiterConfig::WhitespaceKeep, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"p,q\",r,s".to_vec()]]
            }
        });
    }

    /// Matrix case for multiline records with double-quote escaping.
    #[test]
    fn parse_20_configs_case_4_double_quote_multiline_records() {
        let data = b"\"a\r\nb\"\r\n\"c\"\r\n";

        assert_all_20(data, |config| match (config.delimiter, config.escape) {
            (DelimiterConfig::Whole, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"\"a\r\nb\"".to_vec()], rvec![b"\"c\"".to_vec()]]
            }
            (_, EscapeConfig::QuoteDouble) => vec![rvec![b"a\r\nb".to_vec()], rvec![b"c".to_vec()]],
            (_, EscapeConfig::None | EscapeConfig::Backslash | EscapeConfig::QuoteAngle) => {
                vec![rvec![b"\"a".to_vec()], rvec![b"b\"".to_vec()], rvec![b"\"c\"".to_vec()]]
            }
        });
    }

    /// Matrix case for multiline records with angle-quote escaping.
    #[test]
    fn parse_20_configs_case_5_angle_quote_multiline_records() {
        let data = b"<a\"x\r\nb>\r\n<c\"y>\r\n";

        assert_all_20(data, |config| match (config.delimiter, config.escape) {
            (DelimiterConfig::Whole, EscapeConfig::QuoteDouble) => {
                vec![rvec![b"<a\"x\r\nb>\r\n<c\"y>".to_vec()]]
            }
            (DelimiterConfig::Whole, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"<a\"x\r\nb>".to_vec()], rvec![b"<c\"y>".to_vec()]]
            }
            (_, EscapeConfig::QuoteDouble) => vec![rvec![b"<ax\r\nb>\r\n<cy>".to_vec()]],
            (_, EscapeConfig::QuoteAngle) => {
                vec![rvec![b"a\"x\r\nb".to_vec()], rvec![b"c\"y".to_vec()]]
            }
            (_, EscapeConfig::None | EscapeConfig::Backslash) => {
                vec![rvec![b"<a\"x".to_vec()], rvec![b"b>".to_vec()], rvec![b"<c\"y>".to_vec()]]
            }
        });
    }

    /// Verifies quoted fields can span physical lines in char-delimited mode.
    #[test]
    fn quote_escape_can_span_lines() {
        let data = b"\"ab\r\ncd\",ef\r\nxy,z\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::Char(b','), Escape::Quote(b'"', b'"'))
            .unwrap();
        assert_eq!(columns.columns, rvec![b"ab\r\ncd".to_vec(), b"ef".to_vec()]);

        read_legacy(&mut columns, &mut reader, Delimiter::Char(b','), Escape::Quote(b'"', b'"'))
            .unwrap();
        assert_eq!(columns.columns, rvec![b"xy".to_vec(), b"z".to_vec()]);
    }

    /// Verifies custom angle quotes with comma delimiter.
    #[test]
    fn quote_angle_with_char_delimiter() {
        let data = b"<a\"b>,c\"d,<e>>f>\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::Char(b','), Escape::Quote(b'<', b'>'))
            .unwrap();

        assert_eq!(columns.columns, rvec![b"a\"b".to_vec(), b"c\"d".to_vec(), b"e>f".to_vec()]);
    }

    /// Verifies angle-quote mode keeps whole-line parsing behavior.
    #[test]
    fn quote_angle_with_whole_delimiter() {
        let data = b"<aa\"bb>>cc>\"dd\"\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::Whole, Escape::Quote(b'<', b'>'))
            .unwrap();

        assert_eq!(columns.columns, rvec![b"<aa\"bb>>cc>\"dd\"".to_vec()]);
    }

    /// Verifies angle quotes with whitespace-drop delimiter semantics.
    #[test]
    fn quote_angle_with_whitespace_drop_delimiter() {
        let data = b"  <a\"b>   c\"d\t<e>>f>  \n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::WhitespaceDrop,
            Escape::Quote(b'<', b'>'),
        )
        .unwrap();

        assert_eq!(columns.columns, rvec![b"a\"b".to_vec(), b"c\"d".to_vec(), b"e>f".to_vec()]);
    }

    /// Verifies angle quotes with whitespace-keep delimiter semantics.
    #[test]
    fn quote_angle_with_whitespace_keep_delimiter() {
        let data = b"  <a\"b>   c\"d\t<e>>f>  \n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::WhitespaceKeep,
            Escape::Quote(b'<', b'>'),
        )
        .unwrap();

        assert_eq!(
            columns.columns,
            rvec![b"  a\"b".to_vec(), b"   c\"d".to_vec(), b"\te>f".to_vec()]
        );
    }

    /// Verifies full-match string delimiter splitting with no escapes.
    #[test]
    fn string_delimiter_none_splits_on_full_match() {
        let data = b"aa..bb..cc\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::String(b"..".to_vec()), Escape::None)
            .unwrap();

        assert_eq!(columns.columns, rvec![b"aa".to_vec(), b"bb".to_vec(), b"cc".to_vec()]);
    }

    /// Verifies backslash decoding with string delimiters.
    #[test]
    fn string_delimiter_backslash_handles_escapes() {
        let data = b"a\\,\\ b, c\\,\\ d, e\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::String(b", ".to_vec()),
            Escape::Backslash,
        )
        .unwrap();

        assert_eq!(columns.columns, rvec![b"a, b".to_vec(), b"c, d".to_vec(), b"e".to_vec()]);
    }

    /// Verifies angle-quote handling with string delimiters.
    #[test]
    fn string_delimiter_quote_angle_handles_quotes() {
        let data = b"<a, \"b\">, c, <d>>e>\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::String(b", ".to_vec()),
            Escape::Quote(b'<', b'>'),
        )
        .unwrap();

        assert_eq!(columns.columns, rvec![b"a, \"b\"".to_vec(), b"c".to_vec(), b"d>e".to_vec()]);
    }

    /// Verifies multiline quoted fields with string delimiters.
    #[test]
    fn string_delimiter_quote_double_can_span_lines() {
        let data = b"\"a\r\nb\"..c\r\nx..y\r\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::String(b"..".to_vec()),
            Escape::Quote(b'"', b'"'),
        )
        .unwrap();
        assert_eq!(columns.columns, rvec![b"a\r\nb".to_vec(), b"c".to_vec()]);

        read_legacy(
            &mut columns,
            &mut reader,
            Delimiter::String(b"..".to_vec()),
            Escape::Quote(b'"', b'"'),
        )
        .unwrap();
        assert_eq!(columns.columns, rvec![b"x".to_vec(), b"y".to_vec()]);
    }

    /// Verifies whitespace-keep preserves delimiter bytes as field prefixes.
    #[test]
    fn whitespace_keep_preserves_prefix_whitespace() {
        let data = b"  a   b   \n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::WhitespaceKeep, Escape::None).unwrap();

        assert_eq!(columns.columns, rvec![b"  a".to_vec(), b"   b".to_vec()]);
    }

    /// Verifies unknown backslash escapes drop the backslash and keep the escaped byte.
    #[test]
    fn unknown_backslash_escape_drops_backslash() {
        let data = b"a\\qb\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::Char(b','), Escape::Backslash).unwrap();

        assert_eq!(columns.columns, rvec![b"aqb".to_vec()]);
    }

    /// Verifies whole-line mode decodes backslash escapes.
    #[test]
    fn whole_backslash_decodes_escapes() {
        let data = b"a\\n\\tb\\\\c\\q\\\\\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        read_legacy(&mut columns, &mut reader, Delimiter::Whole, Escape::Backslash).unwrap();

        assert_eq!(columns.columns, rvec![b"a\n\tb\\cq\\".to_vec()]);
    }

    /// Verifies CRLF is treated as one record boundary across modes.
    #[test]
    fn crlf_counts_as_single_record_separator_for_all_modes() {
        // Count parsed logical records for one delimiter/escape combination.
        fn count_records(
            data: &[u8],
            make_delimiter: fn() -> Delimiter,
            make_escape: fn() -> Escape,
        ) -> anyhow::Result<usize> {
            let mut reader =
                Infile::with_capacity(1, Cursor::new(data.to_vec()), "<crlf-count-records>");
            let mut columns = Columns::new();
            let mut records = 0usize;

            loop {
                if reader.0.fill_buf()?.is_empty() {
                    break;
                }
                read_legacy(&mut columns, &mut reader, make_delimiter(), make_escape())?;
                records += 1;
            }

            Ok(records)
        }

        let data = b"a\tb\r\nc\td\r\n";

        assert_eq!(count_records(data, || Delimiter::Char(b'\t'), || Escape::None).unwrap(), 2);
        assert_eq!(
            count_records(data, || Delimiter::Char(b'\t'), || Escape::Backslash).unwrap(),
            2
        );
        assert_eq!(
            count_records(data, || Delimiter::Char(b'\t'), || Escape::Quote(b'"', b'"')).unwrap(),
            2
        );

        assert_eq!(count_records(data, || Delimiter::Whole, || Escape::None).unwrap(), 2);
        assert_eq!(count_records(data, || Delimiter::Whole, || Escape::Backslash).unwrap(), 2);
        assert_eq!(
            count_records(data, || Delimiter::Whole, || Escape::Quote(b'"', b'"')).unwrap(),
            2
        );

        assert_eq!(count_records(data, || Delimiter::WhitespaceDrop, || Escape::None).unwrap(), 2);
        assert_eq!(
            count_records(data, || Delimiter::WhitespaceDrop, || Escape::Backslash).unwrap(),
            2
        );
        assert_eq!(
            count_records(data, || Delimiter::WhitespaceDrop, || Escape::Quote(b'"', b'"'))
                .unwrap(),
            2
        );

        assert_eq!(count_records(data, || Delimiter::WhitespaceKeep, || Escape::None).unwrap(), 2);
        assert_eq!(
            count_records(data, || Delimiter::WhitespaceKeep, || Escape::Backslash).unwrap(),
            2
        );
        assert_eq!(
            count_records(data, || Delimiter::WhitespaceKeep, || Escape::Quote(b'"', b'"'))
                .unwrap(),
            2
        );
    }

    /// Verifies `Quotes::Multi` supports multiple quote pairs and doubled close escaping.
    #[test]
    fn multi_quote_supports_multiple_pairs() {
        let data = b"\"a,b\",[c]]d],e\n";

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]);

        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(records, vec![rvec![b"a,b".to_vec(), b"c]d".to_vec(), b"e".to_vec()]]);
    }

    /// Verifies `ParseOptions::csv` config values match expected standard CSV behavior.
    #[test]
    fn parse_options_csv_constructor_values() {
        let options = ParseOptions::csv();
        assert_eq!(options.delimiter, Delimiter::Char(b','));
        assert_eq!(options.quotes, Quotes::Single(b'"', b'"'));
        assert_eq!(options.backslash, BackslashMode::Off);
        assert_eq!(options.header, Header::Yes);
        assert_eq!(options.cdx, CdxMode::Optional);
        assert_eq!(options.saw_header, None);
        assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::ContinueRecord);
    }

    /// Verifies `ParseOptions::tsv` config values match expected standard TSV behavior.
    #[test]
    fn parse_options_tsv_constructor_values() {
        let options = ParseOptions::tsv();
        assert_eq!(options.delimiter, Delimiter::Char(b'\t'));
        assert_eq!(options.quotes, Quotes::None);
        assert_eq!(options.backslash, BackslashMode::On);
        assert_eq!(options.header, Header::Yes);
        assert_eq!(options.cdx, CdxMode::Optional);
        assert_eq!(options.saw_header, None);
        assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::ContinueRecord);
    }

    /// Verifies CSV constructor enables quote handling and multiline quoted records.
    #[test]
    fn parse_options_csv_constructor_parsing_behavior() {
        let options = ParseOptions::csv();
        let records = read_all_with_options(b"\"a,b\",c\n\"x\ny\",z\n", &options).unwrap();
        assert_eq!(
            records,
            vec![rvec![b"a,b".to_vec(), b"c".to_vec()], rvec![b"x\ny".to_vec(), b"z".to_vec()]]
        );
    }

    /// Verifies TSV constructor keeps quote bytes literal and decodes backslash escapes.
    #[test]
    fn parse_options_tsv_constructor_parsing_behavior() {
        let options = ParseOptions::tsv();
        let records = read_all_with_options(b"\"a\tb\"\tc\nx\\ty\\n\tz\n", &options).unwrap();
        assert_eq!(
            records,
            vec![
                rvec![b"\"a".to_vec(), b"b\"".to_vec(), b"c".to_vec()],
                rvec![b"x\ty\n".to_vec(), b"z".to_vec()]
            ]
        );
    }

    /// Verifies `ParseOptions::whole` config values match expected whole-line behavior.
    #[test]
    fn parse_options_whole_constructor_values() {
        let options = ParseOptions::whole();
        assert_eq!(options.delimiter, Delimiter::Whole);
        assert_eq!(options.quotes, Quotes::None);
        assert_eq!(options.backslash, BackslashMode::Off);
        assert_eq!(options.header, Header::Yes);
        assert_eq!(options.cdx, CdxMode::Optional);
        assert_eq!(options.saw_header, None);
        assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::EndAtEol);
    }

    /// Verifies whole constructor returns one column per physical line.
    #[test]
    fn parse_options_whole_constructor_parsing_behavior() {
        let options = ParseOptions::whole();
        let records = read_all_with_options(b"a,b\tc\n\"x\ny\"\n", &options).unwrap();
        assert_eq!(
            records,
            vec![rvec![b"a,b\tc".to_vec()], rvec![b"\"x".to_vec()], rvec![b"y\"".to_vec()]]
        );
    }

    /// Verifies `ParseOptions::clf` config values match expected CLF behavior.
    #[test]
    fn parse_options_clf_constructor_values() {
        let options = ParseOptions::clf();
        assert_eq!(options.delimiter, Delimiter::Char(b' '));
        assert_eq!(options.quotes, Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]));
        assert_eq!(options.backslash, BackslashMode::On);
        assert_eq!(options.header, Header::Yes);
        assert_eq!(options.cdx, CdxMode::Optional);
        assert_eq!(options.saw_header, None);
        assert_eq!(options.unterminated_quote, UnterminatedQuoteMode::EndAtEol);
    }

    /// Verifies CLF constructor groups bracketed timestamp and quoted request into single columns.
    #[test]
    fn parse_options_clf_constructor_parsing_behavior() {
        let options = ParseOptions::clf();
        let data =
            b"127.0.0.1 - - [01/May/2025:07:20:10 +0000] \"GET /index.html HTTP/1.1\" 200 123\n";
        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(
            records,
            vec![rvec![
                b"127.0.0.1".to_vec(),
                b"-".to_vec(),
                b"-".to_vec(),
                b"01/May/2025:07:20:10 +0000".to_vec(),
                b"GET /index.html HTTP/1.1".to_vec(),
                b"200".to_vec(),
                b"123".to_vec(),
            ]]
        );
    }

    /// Verifies CLF constructor decodes backslash escapes and does not span records on unterminated quotes.
    #[test]
    fn parse_options_clf_constructor_backslash_and_end_at_eol_behavior() {
        let options = ParseOptions::clf();
        let data = b"host - - [01/May/2025:07:20:10 +0000] \"GET /a\\tb HTTP/1.1\" 200 1\nhost - - [01/May/2025:07:20:11 +0000] \"unterminated\nhost - - [01/May/2025:07:20:12 +0000] \"GET /ok HTTP/1.1\" 200 2\n";
        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(
            records,
            vec![
                rvec![
                    b"host".to_vec(),
                    b"-".to_vec(),
                    b"-".to_vec(),
                    b"01/May/2025:07:20:10 +0000".to_vec(),
                    b"GET /a\tb HTTP/1.1".to_vec(),
                    b"200".to_vec(),
                    b"1".to_vec(),
                ],
                rvec![
                    b"host".to_vec(),
                    b"-".to_vec(),
                    b"-".to_vec(),
                    b"01/May/2025:07:20:11 +0000".to_vec(),
                    b"unterminated".to_vec(),
                ],
                rvec![
                    b"host".to_vec(),
                    b"-".to_vec(),
                    b"-".to_vec(),
                    b"01/May/2025:07:20:12 +0000".to_vec(),
                    b"GET /ok HTTP/1.1".to_vec(),
                    b"200".to_vec(),
                    b"2".to_vec(),
                ],
            ]
        );
    }

    /// Verifies input config parser accepts base aliases and key/value overrides.
    #[test]
    fn input_config_from_spec_parses_aliases_and_overrides() {
        let parsed = Config::from_spec("log,d=whitespace,q=none,b=off,u=err,h=no").unwrap();
        assert_eq!(parsed.delimiter, Delimiter::WhitespaceDrop);
        assert_eq!(parsed.quotes, Quotes::None);
        assert_eq!(parsed.backslash, BackslashMode::Off);
        assert_eq!(parsed.unterminated_quote, UnterminatedQuoteMode::Error);
        assert_eq!(parsed.header, Header::No);
        assert_eq!(parsed.cdx, CdxMode::Optional);
        assert_eq!(parsed.saw_header, None);
    }

    /// Verifies input config parser supports quote pairs and string delimiters.
    #[test]
    fn input_config_from_spec_pair_and_string_delimiter() {
        let parsed = Config::from_spec("csv,d=str:..,q=pair:<>").unwrap();
        assert_eq!(parsed.delimiter, Delimiter::String(b"..".to_vec()));
        assert_eq!(parsed.quotes, Quotes::Single(b'<', b'>'));
        assert_eq!(parsed.header, Header::Yes);
        assert_eq!(parsed.cdx, CdxMode::Optional);
        assert_eq!(parsed.saw_header, None);
    }

    /// Verifies input config parser supports header overrides.
    #[test]
    fn input_config_from_spec_header_values() {
        let yes = Config::from_spec("csv,h=yes").unwrap();
        assert_eq!(yes.header, Header::Yes);
        assert_eq!(yes.cdx, CdxMode::Optional);
        assert_eq!(yes.saw_header, None);

        let no = Config::from_spec("csv,header=off").unwrap();
        assert_eq!(no.header, Header::No);
        assert_eq!(no.cdx, CdxMode::Optional);
        assert_eq!(no.saw_header, None);
    }

    /// Verifies input config parser supports CDX-mode overrides.
    #[test]
    fn input_config_from_spec_cdx_values() {
        let optional = Config::from_spec("csv,x=o").unwrap();
        assert_eq!(optional.cdx, CdxMode::Optional);

        let required = Config::from_spec("csv,cdx=required").unwrap();
        assert_eq!(required.cdx, CdxMode::Required);

        let forbidden = Config::from_spec("csv,x=f").unwrap();
        assert_eq!(forbidden.cdx, CdxMode::Forbidden);

        let ignore = Config::from_spec("csv,cdx=ignore").unwrap();
        assert_eq!(ignore.cdx, CdxMode::Ignore);
    }

    /// Verifies `FromStr` delegates to input config spec parsing.
    #[test]
    fn input_config_from_str_works() {
        let parsed: Config = "tsv,d=t,b=on".parse().unwrap();
        assert_eq!(parsed.delimiter, Delimiter::Char(b'\t'));
        assert_eq!(parsed.backslash, BackslashMode::On);
        assert_eq!(parsed.header, Header::Yes);
        assert_eq!(parsed.cdx, CdxMode::Optional);
        assert_eq!(parsed.saw_header, None);
    }

    /// Verifies input config parser rejects duplicate keys.
    #[test]
    fn input_config_from_spec_rejects_duplicate_keys() {
        let err = Config::from_spec("csv,d=t,delimiter=c").unwrap_err();
        assert!(err.to_string().contains("duplicate"));
    }

    /// Verifies input config parser rejects unknown values.
    #[test]
    fn input_config_from_spec_rejects_unknown_values() {
        let err = Config::from_spec("csv,u=nope").unwrap_err();
        assert!(err.to_string().contains("unknown unterminated"));
    }

    /// Verifies unknown input config base errors list expected base names.
    #[test]
    fn input_config_from_spec_unknown_base_lists_expected_values() {
        let err = Config::from_spec("nope").unwrap_err();
        let text = err.to_string();
        assert!(text.contains("expected one of"));
        assert!(text.contains("csv"));
        assert!(text.contains("tsv"));
        assert!(text.contains("whole"));
        assert!(text.contains("clf"));
        assert!(text.contains("log"));
    }

    /// Verifies `EndAtEol` ends records even when quote state is still open.
    #[test]
    fn unterminated_quote_end_at_eol_splits_record() {
        let data = b"\"a\nx,y\n";

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Single(b'"', b'"');
        options.unterminated_quote = UnterminatedQuoteMode::EndAtEol;

        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(records, vec![rvec![b"a".to_vec()], rvec![b"x".to_vec(), b"y".to_vec()]]);
    }

    /// Verifies `Error` mode returns `InvalidData` for unterminated quotes at end-of-line.
    #[test]
    fn unterminated_quote_error_returns_invalid_data() {
        let data = b"\"a\n";
        let mut reader = test_infile(&data[..]);
        let mut columns = Columns::new();

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Single(b'"', b'"');
        options.unterminated_quote = UnterminatedQuoteMode::Error;

        let err = columns.read(&mut reader, &options).unwrap_err();
        let err =
            err.downcast_ref::<io::Error>().expect("unterminated quote should preserve io::Error");
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
    }

    /// Verifies quote and backslash handling compose in the general parser path.
    #[test]
    fn quote_and_backslash_on_decode_inside_quotes() {
        let data = b"\"a\\n,b\",x\n\"c\\\"d\",y\n";

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Single(b'"', b'"');
        options.backslash = BackslashMode::On;

        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(
            records,
            vec![rvec![b"a\n,b".to_vec(), b"x".to_vec()], rvec![b"c\"d".to_vec(), b"y".to_vec()]]
        );
    }

    /// Verifies empty string delimiter behaves like whole-line parsing.
    #[test]
    fn string_delimiter_empty_matches_whole_line() {
        let options = ParseOptions::new(Delimiter::String(Vec::new()));

        let records = read_all_with_options(b"abc\n", &options).unwrap();
        assert_eq!(records, vec![rvec![b"abc".to_vec()]]);
    }

    /// Verifies overlapping string delimiters are matched left-to-right.
    #[test]
    fn string_delimiter_overlapping_matches_left_to_right() {
        let options = ParseOptions::new(Delimiter::String(b"::".to_vec()));

        let records = read_all_with_options(b"a:::b\n", &options).unwrap();
        assert_eq!(records, vec![rvec![b"a".to_vec(), b":b".to_vec()]]);
    }

    /// Verifies string delimiter matches at record boundaries produce empty fields.
    #[test]
    fn string_delimiter_boundary_matches_create_empty_fields() {
        let options = ParseOptions::new(Delimiter::String(b"..".to_vec()));

        let records = read_all_with_options(b"..a..\n", &options).unwrap();
        assert_eq!(records, vec![rvec![Vec::new(), b"a".to_vec(), Vec::new()]]);
    }

    /// Verifies EOF without a trailing newline is handled across delimiter families.
    #[test]
    fn eof_without_trailing_newline_across_delimiters() {
        let cases = [
            (
                ParseOptions::new(Delimiter::Char(b',')),
                b"a,b".as_slice(),
                vec![rvec![b"a".to_vec(), b"b".to_vec()]],
            ),
            (ParseOptions::new(Delimiter::Whole), b"abc".as_slice(), vec![rvec![b"abc".to_vec()]]),
            (
                ParseOptions::new(Delimiter::WhitespaceDrop),
                b"  a b".as_slice(),
                vec![rvec![b"a".to_vec(), b"b".to_vec()]],
            ),
            (
                ParseOptions::new(Delimiter::WhitespaceKeep),
                b"  a b".as_slice(),
                vec![rvec![b"  a".to_vec(), b" b".to_vec()]],
            ),
            (
                ParseOptions::new(Delimiter::String(b"..".to_vec())),
                b"a..b".as_slice(),
                vec![rvec![b"a".to_vec(), b"b".to_vec()]],
            ),
        ];

        for (options, data, expected) in cases {
            let records = read_all_with_options(data, &options).unwrap();
            assert_eq!(records, expected);
        }
    }

    /// Verifies quote-continued records can end cleanly at EOF without trailing newline.
    #[test]
    fn unterminated_quote_continue_record_at_eof() {
        let data = b"\"a\nb";

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Single(b'"', b'"');
        options.unterminated_quote = UnterminatedQuoteMode::ContinueRecord;

        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(records, vec![rvec![b"a\nb".to_vec()]]);
    }

    /// Verifies mixed CR/LF/CRLF line endings with quote+backslash parsing.
    #[test]
    fn mixed_line_endings_with_quote_and_backslash_on() {
        let data = b"\"a\\n,b\",x\r\"c\\t,d\",y\n\"e\\r,f\",z\r\n";

        let mut options = ParseOptions::new(Delimiter::Char(b','));
        options.quotes = Quotes::Single(b'"', b'"');
        options.backslash = BackslashMode::On;

        let records = read_all_with_options(data, &options).unwrap();
        assert_eq!(
            records,
            vec![
                rvec![b"a\n,b".to_vec(), b"x".to_vec()],
                rvec![b"c\t,d".to_vec(), b"y".to_vec()],
                rvec![b"e\r,f".to_vec(), b"z".to_vec()]
            ]
        );
    }

    /// Verifies legacy fast paths and forced-general path produce identical records.
    #[test]
    fn legacy_fast_and_general_paths_match() {
        fn assert_equivalent(data: &[u8], fast: &ParseOptions) {
            let mut general = fast.clone();
            general.unterminated_quote = UnterminatedQuoteMode::EndAtEol;

            let fast_records = read_all_with_options(data, fast).unwrap();
            let general_records = read_all_with_options(data, &general).unwrap();
            assert_eq!(fast_records, general_records);
        }

        assert_equivalent(b"a,b\nx,y\n", &ParseOptions::new(Delimiter::Char(b',')));

        let mut char_backslash = ParseOptions::new(Delimiter::Char(b','));
        char_backslash.backslash = BackslashMode::On;
        assert_equivalent(b"a\\,b,c\\n\nx,y\n", &char_backslash);

        let mut char_quote = ParseOptions::new(Delimiter::Char(b','));
        char_quote.quotes = Quotes::Single(b'"', b'"');
        assert_equivalent(b"\"a,b\",c\n\"x\",y\n", &char_quote);

        let mut string_quote = ParseOptions::new(Delimiter::String(b", ".to_vec()));
        string_quote.quotes = Quotes::Single(b'"', b'"');
        assert_equivalent(b"\"a, b\", c\nx, y\n", &string_quote);

        let mut whitespace_drop_backslash = ParseOptions::new(Delimiter::WhitespaceDrop);
        whitespace_drop_backslash.backslash = BackslashMode::On;
        assert_equivalent(b"a\\ b c\\n\nx y\n", &whitespace_drop_backslash);

        let mut whole_quote = ParseOptions::new(Delimiter::Whole);
        whole_quote.quotes = Quotes::Single(b'"', b'"');
        assert_equivalent(b"\"a\"b\nx\"y\"\n", &whole_quote);
    }

    /// Large-file consistency check across selected non-whitespace-keep modes.
    #[test]
    #[ignore = "Large data-file consistency check; run explicitly"]
    fn data_txt_non_whitespacekeep_combinations_match() {
        use std::fs::File;
        use std::io::BufRead;
        use std::path::PathBuf;

        // One parser instance in the synchronized large-file comparison.
        struct Case {
            name: &'static str,
            reader: Infile,
            columns: Columns,
            delimiter: fn() -> Delimiter,
            escape: fn() -> Escape,
        }

        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data.txt");

        let mut cases = [
            Case {
                name: "Char(Tab)+None",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::Char(b'\t'),
                escape: || Escape::None,
            },
            Case {
                name: "Char(Tab)+Backslash",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::Char(b'\t'),
                escape: || Escape::Backslash,
            },
            Case {
                name: "Char(Tab)+Quote",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::Char(b'\t'),
                escape: || Escape::Quote(b'"', b'"'),
            },
            Case {
                name: "WhitespaceDrop+None",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::WhitespaceDrop,
                escape: || Escape::None,
            },
            Case {
                name: "WhitespaceDrop+Backslash",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::WhitespaceDrop,
                escape: || Escape::Backslash,
            },
            Case {
                name: "WhitespaceDrop+Quote",
                reader: Infile::with_capacity(1 << 20, File::open(&path).unwrap(), "data.txt"),
                columns: Columns::new(),
                delimiter: || Delimiter::WhitespaceDrop,
                escape: || Escape::Quote(b'"', b'"'),
            },
        ];

        let mut records = 0usize;

        loop {
            let mut eof_flags = vec![false; cases.len()];
            let mut any_non_eof = false;

            for (i, case) in cases.iter_mut().enumerate() {
                if case.reader.0.fill_buf().unwrap().is_empty() {
                    eof_flags[i] = true;
                    continue;
                }

                read_legacy(
                    &mut case.columns,
                    &mut case.reader,
                    (case.delimiter)(),
                    (case.escape)(),
                )
                .unwrap();
                any_non_eof = true;
            }

            if !any_non_eof {
                break;
            }

            if eof_flags.iter().any(|&eof| eof) {
                let eof_cases: Vec<&str> = cases
                    .iter()
                    .zip(eof_flags.iter())
                    .filter_map(|(case, &eof)| if eof { Some(case.name) } else { None })
                    .collect();
                panic!(
                    "EOF mismatch at record {}. Reached EOF early in: {:?}",
                    records + 1,
                    eof_cases
                );
            }

            let baseline = cases[0].columns.columns.clone();
            for case in cases.iter().skip(1) {
                assert_eq!(
                    case.columns.columns,
                    baseline,
                    "Mismatch at record {} for {}",
                    records + 1,
                    case.name
                );
            }

            records += 1;
        }

        assert_eq!(records, 1_048_576);
    }
}
