//! Column parsing utilities for already-delimited records.
//!
//! The parser is designed for high throughput and low allocation churn:
//! parsed column buffers are reused across calls to [`Columns::read`].

use crate::prelude::*;
pub use crate::read_line::BackslashMode;
pub use crate::read_line::Config;
pub use crate::read_line::Quotes;
pub use crate::read_line::UnterminatedQuoteMode;
use crate::*;
use memchr::memchr_iter;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut};

/// How input records are split into columns.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    /// Columns are separated by a single character, e.g. comma, tab, etc.
    Char(u8),
    /// Columns are separated by a full byte string, e.g. `b", "` or `b".."`.
    /// The delimiter is matched exactly and fully.
    String(Vec<u8>),
    /// Each input record is a single column.
    Whole,
    /// Columns are separated by one or more ASCII whitespace bytes.
    /// Repeated, leading, and trailing whitespace do not create empty columns.
    Whitespace,
    /// Columns are separated by one or more ASCII whitespace bytes.
    /// Delimiter whitespace is preserved as a prefix of the following column.
    /// Trailing whitespace at end-of-record is discarded.
    WhitespaceKeep,
}

impl Default for Delimiter {
    fn default() -> Self {
        Self::Char(b'\t')
    }
}

impl Delimiter {
    /// Get the byte that represents this delimiter, if applicable.
    #[must_use]
    pub const fn delim(&self) -> u8 {
        match self {
            Self::Char(byte) => *byte,
            _ => b'\t',
        }
    }
}

impl Config {
    /// Build options with sane defaults for TSV-like parsing:
    /// no quotes and no backslash escapes.
    #[must_use]
    pub const fn new(delimiter: Delimiter) -> Self {
        Self {
            delimiter,
            quotes: Quotes::None,
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
        }
    }

    /// Build options with sane defaults for TSV-like parsing:
    /// no quotes and no backslash escapes.
    #[must_use]
    pub const fn simple(delimiter: u8) -> Self {
        Self {
            delimiter: Delimiter::Char(delimiter),
            quotes: Quotes::None,
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
        }
    }

    /// Construct standard CSV parsing options.
    ///
    /// This uses:
    /// - comma delimiter
    /// - double-quote quoting with doubled-close escaping (`""`)
    /// - backslash decoding off
    ///
    /// # Examples
    /// ```rust
    /// use cdx::read_line::{Config, Delimiter, Quotes};
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
        }
    }

    /// Construct standard TSV parsing options.
    ///
    /// This uses:
    /// - tab delimiter
    /// - no quote handling
    /// - backslash decoding on
    ///
    /// # Examples
    /// ```rust
    /// use cdx::read_line::{BackslashMode, Config, Delimiter, Quotes};
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
        }
    }

    /// Construct whole-record parsing options.
    ///
    /// This uses:
    /// - whole-record delimiter mode
    /// - no quote handling
    /// - backslash decoding off
    ///
    /// # Examples
    /// ```rust
    /// use cdx::read_line::{Config, Delimiter};
    ///
    /// let options = Config::whole();
    /// assert_eq!(options.delimiter, Delimiter::Whole);
    /// ```
    #[must_use]
    pub const fn whole() -> Self {
        Self {
            delimiter: Delimiter::Whole,
            quotes: Quotes::None,
            backslash: BackslashMode::Off,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
        }
    }

    /// Construct Common Log Format (CLF)-style parsing options.
    ///
    /// This uses:
    /// - space delimiter
    /// - multi-quote handling for `"`...`"` and `[`...`]`
    /// - backslash decoding on
    ///
    /// # Examples
    /// ```rust
    /// use cdx::read_line::{BackslashMode, Config, Delimiter, Quotes};
    ///
    /// let options = Config::clf();
    /// assert_eq!(options.delimiter, Delimiter::Char(b' '));
    /// assert_eq!(options.quotes, Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]));
    /// assert_eq!(options.backslash, BackslashMode::On);
    /// ```
    #[must_use]
    pub fn clf() -> Self {
        Self {
            delimiter: Delimiter::Char(b' '),
            quotes: Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]),
            backslash: BackslashMode::On,
            unterminated_quote: UnterminatedQuoteMode::ContinueRecord,
        }
    }

    pub(crate) fn from_base(base: &str) -> Result<Self> {
        let base_norm = normalize_token(base);
        let config = match base_norm.as_str() {
            "csv" => Self::csv(),
            "tsv" => Self::tsv(),
            "whole" => Self::whole(),
            "clf" | "log" => Self::clf(),
            _ => {
                return Err(anyhow!(format!(
                    "unknown input config base `{base}`; expected one of: csv, tsv, whole, clf, log"
                )));
            }
        };
        Ok(config)
    }

    pub(crate) fn add_spec(&mut self, key: &str, value: &str) -> Result<bool> {
        match key {
            "delimiter" => {
                self.delimiter = parse_input_delimiter(value)?;
                Ok(true)
            }
            "quotes" => {
                self.quotes = parse_input_quotes(value)?;
                Ok(true)
            }
            "backslash" => {
                self.backslash = parse_backslash_mode(value)?;
                Ok(true)
            }
            "unterminated" => {
                self.unterminated_quote = parse_unterminated_mode(value)?;
                Ok(true)
            }
            _ => Ok(false),
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
    pub fn from_spec(spec: &str) -> Result<Self> {
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Err(anyhow!("empty input config spec"));
        }

        let mut parts = trimmed.split(',').map(str::trim);
        let base = parts.next().ok_or_else(|| anyhow!("missing input config base"))?;

        let mut config = Self::from_base(base)?;

        let mut seen_keys: HashSet<&'static str> = HashSet::new();
        for part in parts {
            if part.is_empty() {
                return Err(anyhow!("empty override segment in input config spec"));
            }

            let (key_raw, value_raw) = part.split_once('=').ok_or_else(|| {
                anyhow!(format!("input config override `{part}` must be in key=value form"))
            })?;

            let key_norm = normalize_token(key_raw);
            let key = input_file::canonical_input_key(&key_norm)?;
            if !seen_keys.insert(key) {
                return Err(anyhow!(format!("duplicate input config key `{key_raw}`")));
            }
            config.add_spec(key, value_raw)?;
        }

        Ok(config)
    }
}

impl FromStr for Config {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::from_spec(s)
    }
}

/// Normalize a token for case-insensitive matching and dashed/underscored aliases.
pub(crate) fn normalize_token(token: &str) -> String {
    token
        .trim()
        .chars()
        .filter(|ch| *ch != '_' && *ch != '-')
        .flat_map(char::to_lowercase)
        .collect()
}

/// Parse common single-byte delimiter tokens.
fn parse_byte_token(value: &str, label: &str) -> Result<u8> {
    if let Some(prefix) = value.get(..3)
        && prefix.eq_ignore_ascii_case("ch:")
    {
        let literal = &value[3..];
        let bytes = literal.as_bytes();
        if bytes.len() == 1 && bytes[0].is_ascii() {
            return Ok(bytes[0]);
        }
        return Err(anyhow!(format!(
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
                Err(anyhow!(format!(
                    "unknown {label} value `{value}`; expected one of: t|tab, s|space, n|lf|newline, r|cr, c|comma, p|pipe, sc|semi|semicolon, one ASCII character, or ch:<char>"
                )))
            }
        }
    }
}

/// Parse delimiter override value for input config specs.
fn parse_input_delimiter(value: &str) -> Result<Delimiter> {
    if let Some(prefix) = value.get(..4)
        && prefix.eq_ignore_ascii_case("str:")
    {
        return Ok(Delimiter::String(value.as_bytes()[4..].to_vec()));
    }

    let normalized = normalize_token(value);
    match normalized.as_str() {
        "whole" => Ok(Delimiter::Whole),
        "ws" | "whitespace" => Ok(Delimiter::Whitespace),
        "wsk" | "whitespacekeep" => Ok(Delimiter::WhitespaceKeep),
        _ => parse_byte_token(value, "delimiter")
            .map(Delimiter::Char)
            .map_err(|_| {
                anyhow!(format!(
                    "unknown delimiter value `{value}`; expected one of: whole, ws|whitespace, wsk|whitespace_keep, str:<text>, t|tab, s|space, n|lf|newline, r|cr, c|comma, p|pipe, sc|semi|semicolon, one ASCII character, or ch:<char>"
                ))
            }),
    }
}

/// Parse quotes override value for input config specs.
fn parse_input_quotes(value: &str) -> Result<Quotes> {
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
        return Err(anyhow!(format!(
            "quotes value `{value}` must be `pair:<xy>` with exactly two ASCII characters"
        )));
    }

    match normalize_token(value).as_str() {
        "none" => Ok(Quotes::None),
        "d" | "dq" | "double" | "doublequote" | "doublequotes" => Ok(Quotes::Single(b'"', b'"')),
        "s" | "sq" | "single" | "singlequote" | "singlequotes" => Ok(Quotes::Single(b'\'', b'\'')),
        "clf" | "log" => Ok(Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')])),
        _ => Err(anyhow!(format!(
            "unknown quotes value `{value}`; expected one of: none, d|dq|double|doublequote, s|sq|single|singlequote, clf|log, or pair:<xy>"
        ))),
    }
}

/// Parse backslash-mode override value for input config specs.
fn parse_backslash_mode(value: &str) -> Result<BackslashMode> {
    match normalize_token(value).as_str() {
        "on" | "1" | "true" | "y" | "yes" => Ok(BackslashMode::On),
        "off" | "0" | "false" | "n" | "no" => Ok(BackslashMode::Off),
        _ => Err(anyhow!(format!(
            "unknown backslash value `{value}`; expected one of: on, off, 1, 0, true, false"
        ))),
    }
}

/// Parse unterminated-quote override value for input config specs.
fn parse_unterminated_mode(value: &str) -> Result<UnterminatedQuoteMode> {
    match normalize_token(value).as_str() {
        "cont" | "continue" | "continuerecord" => Ok(UnterminatedQuoteMode::ContinueRecord),
        "eol" | "endateol" => Ok(UnterminatedQuoteMode::EndAtEol),
        "err" | "error" => Ok(UnterminatedQuoteMode::Error),
        _ => Err(anyhow!(format!(
            "unknown unterminated value `{value}`; expected one of: cont|continue, eol|endateol, err|error"
        ))),
    }
}

/// Backwards-compatible alias for older API users.
///
/// Prefer [`Config`].
pub type ParseOptions = Config;

/// Reusable parser state and output buffer for one parsed record at a time.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Columns {
    /// Output columns for the last parsed record.
    pub columns: ColumnValues,
}

impl Deref for Columns {
    type Target = ColumnValues;

    fn deref(&self) -> &Self::Target {
        &self.columns
    }
}

impl DerefMut for Columns {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.columns
    }
}

impl From<RVec<Vec<u8>>> for Columns {
    fn from(columns: RVec<Vec<u8>>) -> Self {
        Self { columns }
    }
}

impl From<Columns> for RVec<Vec<u8>> {
    fn from(columns: Columns) -> Self {
        columns.columns
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

/// Decode a backslash escape byte.
const fn decode_backslash_escape(byte: u8) -> u8 {
    match byte {
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        b'\\' => b'\\',
        _ => byte,
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

    /// Get the columns parsed from the last record, as Strings, with lossy utf-8 decoding.
    #[must_use]
    pub fn as_strings(&self) -> Vec<String> {
        self.columns.iter().map(|v| String::from_utf8_lossy(v).into_owned()).collect()
    }

    /// Get the columns parsed from the last record, as Strings, fail if not utf8-encoded.
    pub fn as_strings_strict(&self) -> anyhow::Result<Vec<String>> {
        self.columns
            .iter()
            .map(|v| String::from_utf8(v.clone()).map_err(anyhow::Error::from))
            .collect()
    }

    /// Get the columns parsed from the last record, as Strings, fail if not utf8-encoded.
    pub fn into_strings(self) -> anyhow::Result<Vec<String>> {
        self.columns
            .into_iter()
            .map(|v| String::from_utf8(v).map_err(anyhow::Error::from))
            .collect()
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

    /// Read one complete record from `record` and split it into columns according to `options`.
    ///
    /// This function does not perform any I/O and does not interpret record terminators. The caller
    /// is responsible for passing exactly one complete record. If a quote is still open at the end
    /// of the slice, parsing ends there and the accumulated field is accepted.
    pub fn read(&mut self, record: &[u8], options: &Config) {
        match &options.delimiter {
            Delimiter::Whole => self.read_whole(record, options.backslash),
            Delimiter::Char(delimiter_byte) => self.read_char(record, *delimiter_byte, options),
            Delimiter::String(delimiter_bytes) => {
                self.read_string(record, delimiter_bytes, options);
            }
            Delimiter::Whitespace | Delimiter::WhitespaceKeep => {
                self.read_whitespace(record, options);
            }
        }
    }

    /// Parse a whole-record value.
    fn read_whole(&mut self, record: &[u8], backslash: BackslashMode) {
        self.clear_column(0);

        if matches!(backslash, BackslashMode::Off) {
            self.columns[0].extend_from_slice(record);
            self.finish_columns(1);
            return;
        }

        let mut i = 0;
        let mut escaped = false;
        while i < record.len() {
            let byte = record[i];
            if escaped {
                self.columns[0].push(decode_backslash_escape(byte));
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
    }

    /// Ensure `columns[index]` exists, reset it for writing, and return it.
    fn clear_column_mut(&mut self, index: usize) -> &mut Vec<u8> {
        if index == self.columns.len() {
            self.columns.push(Vec::new());
        }
        // let column = &mut self.columns[index];
        let column = unsafe { self.columns.get_unchecked_mut(index) };
        column.clear();
        column
    }

    /// Parse a record with a single-byte delimiter and no escape or quote handling.
    pub fn read_plain(&mut self, record: &[u8], delimiter: u8) {
        let mut column_index = 0usize;
        let mut start = 0usize;
        let mut column = self.clear_column_mut(column_index);

        for delimiter_index in memchr_iter(delimiter, record) {
            // column.extend_from_slice(&record[start..delimiter_index]);
            column.extend_from_slice(unsafe { record.get_unchecked(start..delimiter_index) });
            column_index += 1;
            column = self.clear_column_mut(column_index);
            start = delimiter_index + 1;
        }

        // column.extend_from_slice(&record[start..]);
        column.extend_from_slice(unsafe { record.get_unchecked(start..) });
        self.finish_columns(column_index + 1);
    }

    /// Parse a record with a single-byte delimiter.
    fn read_char(&mut self, record: &[u8], delimiter: u8, options: &Config) {
        if options.backslash == BackslashMode::Off && options.quotes == Quotes::None {
            return self.read_plain(record, delimiter);
        }
        self.clear_column(0);
        let mut column_index = 0usize;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;

        while i < record.len() {
            let byte = record[i];

            if matches!(options.backslash, BackslashMode::On) {
                if escaped {
                    self.columns[column_index].push(decode_backslash_escape(byte));
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

            if byte == delimiter {
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

    /// Parse a record with a byte-string delimiter.
    fn read_string(&mut self, record: &[u8], delimiter: &[u8], options: &Config) {
        if delimiter.is_empty() {
            self.read_whole(record, options.backslash);
            return;
        }

        self.clear_column(0);
        let mut column_index = 0usize;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;

        while i < record.len() {
            let byte = record[i];

            if matches!(options.backslash, BackslashMode::On) {
                if escaped {
                    self.columns[column_index].push(decode_backslash_escape(byte));
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

    /// Parse a record with whitespace delimiter behavior.
    fn read_whitespace(&mut self, record: &[u8], options: &Config) {
        let keep = matches!(&options.delimiter, Delimiter::WhitespaceKeep);

        let mut fields = 0usize;
        let mut current = 0usize;
        let mut in_field = false;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;
        let mut pending_whitespace_start = None;

        while i < record.len() {
            let byte = record[i];

            if matches!(options.backslash, BackslashMode::On) {
                if escaped {
                    if !in_field {
                        current = fields;
                        self.clear_column(current);
                        fields += 1;
                        if keep && let Some(start) = pending_whitespace_start.take() {
                            self.columns[current].extend_from_slice(&record[start..i - 1]);
                        }
                        in_field = true;
                    }
                    self.columns[current].push(decode_backslash_escape(byte));
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
                    if keep && let Some(start) = pending_whitespace_start.take() {
                        self.columns[current].extend_from_slice(&record[start..i]);
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
                        pending_whitespace_start = Some(i);
                    } else if pending_whitespace_start.is_none() {
                        pending_whitespace_start = Some(i);
                    }
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
                if keep && let Some(start) = pending_whitespace_start.take() {
                    self.columns[current].extend_from_slice(&record[start..i]);
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
                if keep && let Some(start) = pending_whitespace_start.take() {
                    self.columns[current].extend_from_slice(&record[start..record.len() - 1]);
                }
            }
            self.columns[current].push(b'\\');
        }

        self.finish_columns(fields);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse one record and clone the current output columns.
    fn parse_record(record: &[u8], options: &Config) -> RVec<Vec<u8>> {
        let mut columns = Columns::new();
        columns.read(record, options);
        columns.columns.clone()
    }

    #[test]
    fn char_delimiter_splits_basic_record() {
        assert_eq!(
            parse_record(b"a,b,", &Config::csv()),
            rvec![b"a".to_vec(), b"b".to_vec(), Vec::new()]
        );
    }

    #[test]
    fn columns_converts_to_and_from_vec_without_changing_data() {
        let data = rvec![b"a".to_vec(), b"b".to_vec()];
        let columns = Columns::from(data.clone());

        assert_eq!(columns.as_ref(), data.as_slice());

        let round_tripped: RVec<Vec<u8>> = columns.into();
        assert_eq!(round_tripped, data);
    }

    #[test]
    fn csv_quotes_remove_wrappers_and_decode_doubled_quote() {
        assert_eq!(
            parse_record(br#""a,b","c""d",e"#, &Config::csv()),
            rvec![b"a,b".to_vec(), b"c\"d".to_vec(), b"e".to_vec()]
        );
    }

    #[test]
    fn open_quote_is_accepted_at_record_end() {
        assert_eq!(parse_record(b"\"a,b", &Config::csv()), rvec![b"a,b".to_vec()]);
    }

    #[test]
    fn tsv_decodes_backslash_escapes() {
        assert_eq!(
            parse_record(b"a\\tb\\n\\q\tc\\\\", &Config::tsv()),
            rvec![b"a\tb\nq".to_vec(), b"c\\".to_vec()]
        );
    }

    #[test]
    fn whole_record_keeps_bytes_unless_backslash_is_enabled() {
        let mut options = Config::whole();
        assert_eq!(parse_record(b"a,b\tc", &options), rvec![b"a,b\tc".to_vec()]);

        options.backslash = BackslashMode::On;
        assert_eq!(parse_record(b"a\\nb", &options), rvec![b"a\nb".to_vec()]);
    }

    #[test]
    fn string_delimiter_matches_left_to_right() {
        let options = Config::new(Delimiter::String(b"::".to_vec()));
        assert_eq!(
            parse_record(b"a:::b::", &options),
            rvec![b"a".to_vec(), b":b".to_vec(), Vec::new()]
        );
    }

    #[test]
    fn empty_string_delimiter_matches_whole_record() {
        let options = Config::new(Delimiter::String(Vec::new()));
        assert_eq!(parse_record(b"abc", &options), rvec![b"abc".to_vec()]);
    }

    #[test]
    fn whitespace_discards_repeated_and_edge_whitespace() {
        let options = Config::new(Delimiter::Whitespace);
        assert_eq!(parse_record(b"  a\t b  ", &options), rvec![b"a".to_vec(), b"b".to_vec()]);
    }

    #[test]
    fn whitespace_keep_prefixes_next_field_and_drops_trailing_whitespace() {
        let options = Config::new(Delimiter::WhitespaceKeep);
        assert_eq!(parse_record(b"  a\t b  ", &options), rvec![b"  a".to_vec(), b"\t b".to_vec()]);
    }

    #[test]
    fn whitespace_keep_with_backslash_copies_pending_whitespace_from_record() {
        let mut options = Config::new(Delimiter::WhitespaceKeep);
        options.backslash = BackslashMode::On;

        assert_eq!(
            parse_record(b"  \\ta \\ b", &options),
            rvec![b"  \ta".to_vec(), b"  b".to_vec()]
        );
    }

    #[test]
    fn multi_quote_supports_multiple_pairs() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.quotes = Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]);

        assert_eq!(
            parse_record(b"\"a,b\",[c]]d],e", &options),
            rvec![b"a,b".to_vec(), b"c]d".to_vec(), b"e".to_vec()]
        );
    }

    #[test]
    fn clf_constructor_groups_bracketed_and_quoted_values() {
        let options = Config::clf();
        let record =
            b"127.0.0.1 - - [01/May/2025:07:20:10 +0000] \"GET /index.html HTTP/1.1\" 200 123";

        assert_eq!(
            parse_record(record, &options),
            rvec![
                b"127.0.0.1".to_vec(),
                b"-".to_vec(),
                b"-".to_vec(),
                b"01/May/2025:07:20:10 +0000".to_vec(),
                b"GET /index.html HTTP/1.1".to_vec(),
                b"200".to_vec(),
                b"123".to_vec(),
            ]
        );
    }

    #[test]
    fn constructors_set_expected_values() {
        let csv = Config::csv();
        assert_eq!(csv.delimiter, Delimiter::Char(b','));
        assert_eq!(csv.quotes, Quotes::Single(b'"', b'"'));
        assert_eq!(csv.backslash, BackslashMode::Off);

        let tsv = Config::tsv();
        assert_eq!(tsv.delimiter, Delimiter::Char(b'\t'));
        assert_eq!(tsv.quotes, Quotes::None);
        assert_eq!(tsv.backslash, BackslashMode::On);

        let whole = Config::whole();
        assert_eq!(whole.delimiter, Delimiter::Whole);
        assert_eq!(whole.quotes, Quotes::None);
        assert_eq!(whole.backslash, BackslashMode::Off);
    }

    #[test]
    fn input_config_from_spec_parses_aliases_and_overrides() {
        let parsed = Config::from_spec("log,d=whitespace,q=none,b=off").unwrap();
        assert_eq!(parsed.delimiter, Delimiter::Whitespace);
        assert_eq!(parsed.quotes, Quotes::None);
        assert_eq!(parsed.backslash, BackslashMode::Off);
    }

    #[test]
    fn input_config_from_spec_supports_quote_pairs_and_string_delimiters() {
        let parsed = Config::from_spec("csv,d=str:..,q=pair:<>").unwrap();
        assert_eq!(parsed.delimiter, Delimiter::String(b"..".to_vec()));
        assert_eq!(parsed.quotes, Quotes::Single(b'<', b'>'));
    }

    #[test]
    fn input_config_from_str_works() {
        let parsed: Config = "tsv,d=t,b=on".parse().unwrap();
        assert_eq!(parsed.delimiter, Delimiter::Char(b'\t'));
        assert_eq!(parsed.backslash, BackslashMode::On);
    }

    #[test]
    fn input_config_from_spec_rejects_duplicate_keys() {
        let err = Config::from_spec("csv,d=t,delimiter=c").unwrap_err();
        assert!(err.to_string().contains("duplicate"));
    }

    #[test]
    fn input_config_from_spec_rejects_unknown_values() {
        let err = Config::from_spec("csv,b=nope").unwrap_err();
        assert!(err.to_string().contains("unknown backslash"));
    }

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
}
