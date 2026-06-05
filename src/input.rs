//! Column parsing utilities for already-delimited records.
//!
//! The parser is designed for high throughput and low allocation churn:
//! parsed column buffers are reused across calls to [`Columns::read`].

use memchr::memchr_iter;
use memchr::memchr3_iter;
use std::collections::HashSet;
use std::iter::FusedIterator;
use std::ops::{Index, Range};

use crate::prelude::*;
pub use crate::read_line::BackslashMode;
pub use crate::read_line::Config;
pub use crate::read_line::Quotes;
use crate::read_line::ReadResult;
pub use crate::read_line::UnterminatedQuoteMode;
use crate::util::FileLocData;
use crate::*;

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
    WhitespaceDrop,
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

    /// Construct standard CSV parsing options.
    ///
    /// This uses:
    /// - comma delimiter
    /// - double-quote quoting with doubled-close escaping (`""`)
    /// - backslash decoding off
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
    /// use cdx::input::{Config, Delimiter};
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
    /// use cdx::input::{BackslashMode, Config, Delimiter, Quotes};
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
                    "unknown input config key `{key_raw}`; expected one of: d|delimiter, q|quotes, b|backslash"
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
                _ => unreachable!("all input keys are canonicalized above"),
            }
        }

        Ok(config)
    }
}

impl FromStr for Config {
    type Err = anyhow::Error;

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

/// Canonicalize one input-config override key.
fn canonical_input_key(normalized: &str) -> Option<&'static str> {
    match normalized {
        "d" | "delimiter" => Some("delimiter"),
        "q" | "quotes" => Some("quotes"),
        "b" | "backslash" => Some("backslash"),
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
        "wsd" | "whitespacedrop" => Ok(Delimiter::WhitespaceDrop),
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
        "d" | "dq" | "double" | "doublequote" | "doublequotes" => Ok(Quotes::Single(b'"', b'"')),
        "s" | "sq" | "single" | "singlequote" | "singlequotes" => Ok(Quotes::Single(b'\'', b'\'')),
        "clf" | "log" => Ok(Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')])),
        _ => Err(anyhow::anyhow!(format!(
            "unknown quotes value `{value}`; expected one of: none, d|dq|double|doublequote, s|sq|single|singlequote, clf|log, or pair:<xy>"
        ))),
    }
}

/// Parse backslash-mode override value for input config specs.
fn parse_backslash_mode(value: &str) -> anyhow::Result<BackslashMode> {
    match normalize_token(value).as_str() {
        "y" | "yes" | "on" | "1" | "true" => Ok(BackslashMode::On),
        "n" | "no" | "off" | "0" | "false" => Ok(BackslashMode::Off),
        _ => Err(anyhow::anyhow!(format!(
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
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Columns {
    /// Original input bytes for the current record.
    input: Vec<u8>,
    /// Decoded output bytes, populated only when a record needs transformation.
    decoded: Vec<u8>,
    /// Byte ranges in the active backing buffer for the last parsed record's columns.
    ptrs: Vec<Range<usize>>,
    /// Which buffer `ptrs` currently indexes.
    backing: Backing,
}

/// Backing buffer used by the current parsed record.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum Backing {
    /// Ranges point into the unmodified input record.
    #[default]
    Input,
    /// Ranges point into the decoded output buffer.
    Decoded,
}

impl From<Vec<Vec<u8>>> for Columns {
    fn from(columns: Vec<Vec<u8>>) -> Self {
        Self::from_vecs(&columns)
    }
}

impl From<&[&[u8]]> for Columns {
    fn from(columns: &[&[u8]]) -> Self {
        Self::from_slices(columns)
    }
}

impl From<&[Vec<u8>]> for Columns {
    fn from(columns: &[Vec<u8>]) -> Self {
        Self::from_vecs(columns)
    }
}

impl From<Columns> for Vec<Vec<u8>> {
    fn from(columns: Columns) -> Self {
        let data = match columns.backing {
            Backing::Input => columns.input,
            Backing::Decoded => columns.decoded,
        };

        columns.ptrs.into_iter().map(|range| data[range].to_vec()).collect()
    }
}

impl Index<usize> for Columns {
    type Output = [u8];

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
    }
}

impl<'a> IntoIterator for &'a Columns {
    type Item = &'a [u8];
    type IntoIter = ColumnsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterator over parsed column slices.
#[derive(Clone, Debug)]
pub struct ColumnsIter<'a> {
    data: &'a [u8],
    ranges: std::slice::Iter<'a, Range<usize>>,
}

impl<'a> Iterator for ColumnsIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        self.ranges.next().map(|range| &self.data[range.clone()])
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.ranges.size_hint()
    }
}

impl ExactSizeIterator for ColumnsIter<'_> {}

impl DoubleEndedIterator for ColumnsIter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.ranges.next_back().map(|range| &self.data[range.clone()])
    }
}

impl FusedIterator for ColumnsIter<'_> {}

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
    /// Construct a reusable parser container from an owned input record.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a reusable parser container from an owned input record.
    #[must_use]
    pub const fn with_record(record: Vec<u8>) -> Self {
        Self { input: record, decoded: Vec::new(), ptrs: Vec::new(), backing: Backing::Input }
    }

    /// Construct a reusable parser container from an owned input record.
    #[must_use]
    pub fn with_data(record: &[u8]) -> Self {
        Self::with_record(record.into())
    }

    /// Construct columns by copying borrowed byte slices into one contiguous input buffer.
    #[must_use]
    pub fn from_slices(columns: &[&[u8]]) -> Self {
        let total_len = columns.iter().map(|column| column.len()).sum();
        let mut input = Vec::with_capacity(total_len);
        let mut ptrs = Vec::with_capacity(columns.len());

        for column in columns {
            let start = input.len();
            input.extend_from_slice(column);
            ptrs.push(start..input.len());
        }

        Self { input, decoded: Vec::new(), ptrs, backing: Backing::Input }
    }

    /// Construct columns by copying borrowed byte vectors into one contiguous input buffer.
    #[must_use]
    pub fn from_vecs(columns: &[Vec<u8>]) -> Self {
        let total_len = columns.iter().map(Vec::len).sum();
        let mut input = Vec::with_capacity(total_len);
        let mut ptrs = Vec::with_capacity(columns.len());

        for column in columns {
            let start = input.len();
            input.extend_from_slice(column);
            ptrs.push(start..input.len());
        }

        Self { input, decoded: Vec::new(), ptrs, backing: Backing::Input }
    }

    /// Return the number of columns parsed from the last record.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.ptrs.len()
    }

    /// Return true if the last parsed record has no columns.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.ptrs.is_empty()
    }

    /// Get one parsed column by index.
    #[must_use]
    pub fn get(&self, index: usize) -> &[u8] {
        let Some(range) = self.ptrs.get(index) else {
            return &[];
        };
        // FIXME - test performance with get_unchecked
        &self.active_data()[range.start..range.end]
    }

    /// Iterate over the parsed columns from the last record.
    #[must_use]
    pub fn iter(&self) -> ColumnsIter<'_> {
        ColumnsIter { data: self.active_data(), ranges: self.ptrs.iter() }
    }

    /// Remove consecutive columns that compare equal under `same_bucket`.
    ///
    /// This mirrors [`Vec::dedup_by`] argument order: the predicate receives `(current, previous)`.
    /// When it returns `true`, the current column is removed. Column bytes are not modified; this
    /// only compacts the stored column ranges.
    pub fn dedup_by<F>(&mut self, mut same_bucket: F)
    where
        F: FnMut(&[u8], &[u8]) -> bool,
    {
        if self.ptrs.len() <= 1 {
            return;
        }

        let data = match self.backing {
            Backing::Input => &self.input,
            Backing::Decoded => &self.decoded,
        };
        let ptrs = &mut self.ptrs;

        let mut write = 1usize;
        for read in 1..ptrs.len() {
            let current = ptrs[read].clone();
            let previous = ptrs[write - 1].clone();

            if same_bucket(&data[current.clone()], &data[previous]) {
                continue;
            }

            if write != read {
                ptrs[write] = current;
            }
            write += 1;
        }

        ptrs.truncate(write);
    }

    /// Remove consecutive equal columns.
    ///
    /// Column bytes are not modified; this only compacts the stored column ranges.
    pub fn dedup(&mut self) {
        self.dedup_by(<[u8]>::eq);
    }

    /// Retain only columns for which `keep` returns true.
    ///
    /// Column bytes are not modified; this only compacts the stored column ranges.
    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&[u8]) -> bool,
    {
        let data = match self.backing {
            Backing::Input => &self.input,
            Backing::Decoded => &self.decoded,
        };
        self.ptrs.retain(|range| keep(&data[range.start..range.end]));
    }

    /// Sort columns with a comparator over column byte slices.
    ///
    /// This mirrors [`Vec::sort_by`] comparator shape. Column bytes are not moved or copied; only
    /// the stored column ranges are reordered.
    pub fn sort_by<F>(&mut self, mut compare: F)
    where
        F: FnMut(&[u8], &[u8]) -> Ordering,
    {
        let data = match self.backing {
            Backing::Input => &self.input,
            Backing::Decoded => &self.decoded,
        };

        self.ptrs.sort_by(|left, right| {
            compare(&data[left.start..left.end], &data[right.start..right.end])
        });
    }

    /// Sort columns by their byte slice ordering.
    ///
    /// Column bytes are not moved or copied; only the stored column ranges are reordered.
    pub fn sort(&mut self) {
        self.sort_by(<[u8]>::cmp);
    }

    /// Reverse the parsed column order.
    pub fn reverse(&mut self) {
        self.ptrs.reverse();
    }

    /// Swap two parsed columns.
    ///
    /// Panics if either index is out of bounds.
    pub fn swap(&mut self, a: usize, b: usize) {
        self.ptrs.swap(a, b);
    }

    /// Shorten the parsed columns to `len`.
    pub fn truncate(&mut self, len: usize) {
        self.ptrs.truncate(len);
    }

    /// Clear parsed columns while preserving the current input and decoded buffers.
    pub fn clear_cols(&mut self) {
        self.ptrs.clear();
    }

    /// Clear parsed columns while preserving the current input and decoded buffers.
    pub fn clear(&mut self) {
        self.ptrs.clear();
        self.input.clear();
        self.decoded.clear();
    }

    /// Remove a parsed column, preserving the order of remaining columns.
    ///
    /// Panics if `index` is out of bounds.
    pub fn remove(&mut self, index: usize) {
        self.ptrs.remove(index);
    }

    /// Remove a parsed column by swapping it with the last column first.
    ///
    /// Panics if `index` is out of bounds.
    pub fn swap_remove(&mut self, index: usize) {
        self.ptrs.swap_remove(index);
    }

    /// Binary search parsed columns with a comparator over column byte slices.
    ///
    /// This mirrors [`Vec::binary_search_by`].
    pub fn binary_search_by<F>(&self, mut compare: F) -> std::result::Result<usize, usize>
    where
        F: FnMut(&[u8]) -> Ordering,
    {
        let data = self.active_data();
        self.ptrs.binary_search_by(|range| compare(&data[range.start..range.end]))
    }

    /// Return true when the current parsed columns are backed by decoded bytes.
    #[must_use]
    pub const fn uses_decoded_buffer(&self) -> bool {
        self.is_decoded()
    }

    /// Replace the current input record with an owned buffer.
    pub fn set(&mut self, record: Vec<u8>) {
        self.input = record;
    }

    /// Get the columns parsed from the last record, as Strings, with lossy utf-8 decoding.
    #[must_use]
    pub fn as_strings(&self) -> Vec<String> {
        self.iter().map(|v| String::from_utf8_lossy(v).into_owned()).collect()
    }

    /// Get the columns parsed from the last record, as Strings, fail if not utf8-encoded.
    pub fn as_strings_strict(&self) -> anyhow::Result<Vec<String>> {
        self.iter().map(|v| String::from_utf8(v.into()).map_err(anyhow::Error::from)).collect()
    }

    /// Get the columns parsed from the last record, as Strings, fail if not utf8-encoded.
    pub fn into_strings(self) -> anyhow::Result<Vec<String>> {
        self.into_iter().map(|v| String::from_utf8(v.into()).map_err(anyhow::Error::from)).collect()
    }

    /// Replace the current input record while retaining reusable allocations.
    pub fn set_record(&mut self, record: &[u8]) {
        self.input.clear();
        self.input.extend_from_slice(record);
    }

    /// Mutably access the reusable input buffer.
    ///
    /// Call [`Columns::read`] after filling this buffer to parse it. Parsing will clear any previous
    /// output ranges before reading the current buffer contents.
    pub const fn input_mut(&mut self) -> &mut Vec<u8> {
        &mut self.input
    }

    /// access input buffer
    #[must_use]
    pub fn input(&self) -> &[u8] {
        &self.input
    }

    /// Mutably access the reusable input buffer.
    ///
    /// Call [`Columns::read`] after filling this buffer to parse it. Parsing will clear any previous
    /// output ranges before reading the current buffer contents.
    pub const fn ptrs_mut(&mut self) -> &mut Vec<Range<usize>> {
        &mut self.ptrs
    }

    /// access input buffer
    #[must_use]
    pub fn ptrs(&self) -> &[Range<usize>] {
        &self.ptrs
    }

    /// access decoded buffer, might old or empty.
    ///  See `active_data` if you want the thing currently in use
    #[must_use]
    pub const fn decoded(&self) -> &Vec<u8> {
        &self.decoded
    }

    /// Read one complete record from `record` and split it into columns according to `options`.
    ///
    /// This is a convenience wrapper around [`Columns::set_record`] and [`Columns::read`].
    pub fn read_record(&mut self, record: &[u8], options: &Config) {
        self.set_record(record);
        self.read(options);
    }

    /// Return the buffer currently indexed by `ptrs`.
    fn active_data(&self) -> &[u8] {
        match self.backing {
            Backing::Input => &self.input,
            Backing::Decoded => &self.decoded,
        }
    }

    /// Return true when output ranges have been promoted into the decoded buffer.
    const fn is_decoded(&self) -> bool {
        matches!(self.backing, Backing::Decoded)
    }

    /// Clear output while retaining allocations for the next parse of the current record.
    fn clear_output(&mut self) {
        self.decoded.clear();
        self.ptrs.clear();
        self.backing = Backing::Input;
    }

    /// Promote output from input ranges to decoded ranges before transforming `input_pos`.
    ///
    /// Ranges remain valid across promotion because `decoded` starts as an exact byte-for-byte copy
    /// of `input[..input_pos]`. Callers must promote immediately before the first byte whose output
    /// is not represented by the same byte at the same offset in `input`.
    fn promote_to_decoded(&mut self, input_pos: usize) {
        if self.is_decoded() {
            return;
        }

        self.decoded.clear();
        self.decoded.extend_from_slice(&self.input[..input_pos]);
        self.backing = Backing::Decoded;
    }

    /// Append one output byte after promotion to the decoded buffer.
    fn push_decoded(&mut self, byte: u8) {
        debug_assert!(self.is_decoded());
        self.decoded.push(byte);
    }

    /// Append an unchanged input slice after promotion to the decoded buffer.
    fn extend_decoded_from_input(&mut self, range: Range<usize>) {
        debug_assert!(self.is_decoded());
        self.decoded.extend_from_slice(&self.input[range]);
    }

    /// Finish the current column, which started at `start`.
    fn finish_column(&mut self, start: usize, input_end: usize) {
        let end = if self.is_decoded() { self.decoded.len() } else { input_end };
        self.ptrs.push(start..end);
    }

    /// Return the start offset for the next column after a delimiter ending at `input_start`.
    const fn next_column_start(&self, input_start: usize) -> usize {
        if self.is_decoded() { self.decoded.len() } else { input_start }
    }

    /// Return true when `delimiter` fully matches the record at `index`.
    fn delimiter_matches_at(record: &[u8], index: usize, delimiter: &[u8]) -> bool {
        !delimiter.is_empty()
            && index + delimiter.len() <= record.len()
            && &record[index..index + delimiter.len()] == delimiter
    }

    /// Resolve a just-closed wrapper quote before parsing the next char-delimited byte.
    ///
    /// If the next byte is a delimiter, the quoted field was an input-backed contiguous range and
    /// no promotion is needed. Otherwise, bytes after the closing quote continue the same field, so
    /// quote removal makes the output non-contiguous and the record must promote.
    fn resolve_pending_char_quote(
        &mut self,
        pending_quote_end: &mut Option<usize>,
        delimiter: u8,
        byte: u8,
        index: &mut usize,
        column_start: &mut usize,
    ) -> bool {
        let Some(quote_end) = *pending_quote_end else {
            return false;
        };

        if byte == delimiter {
            self.finish_column(*column_start, quote_end);
            *column_start = self.next_column_start(*index + 1);
            *pending_quote_end = None;
            *index += 1;
            return true;
        }

        self.promote_to_decoded(quote_end);
        *pending_quote_end = None;
        false
    }

    /// Resolve a just-closed wrapper quote before parsing the next string-delimited byte.
    fn resolve_pending_string_quote(
        &mut self,
        pending_quote_end: &mut Option<usize>,
        delimiter: &[u8],
        index: &mut usize,
        column_start: &mut usize,
    ) -> bool {
        let Some(quote_end) = *pending_quote_end else {
            return false;
        };

        if Self::delimiter_matches_at(&self.input, *index, delimiter) {
            self.finish_column(*column_start, quote_end);
            *column_start = self.next_column_start(*index + delimiter.len());
            *pending_quote_end = None;
            *index += delimiter.len();
            return true;
        }

        self.promote_to_decoded(quote_end);
        *pending_quote_end = None;
        false
    }

    /// Read the current record and split it into columns according to `options`.
    ///
    /// This function does not perform any I/O and does not interpret record terminators. The caller
    /// is responsible for loading exactly one complete record with [`Columns::set_record`]. If a
    /// quote is still open at the end of the input, parsing ends there and the accumulated field is
    /// accepted.
    pub fn read(&mut self, options: &Config) {
        match &options.delimiter {
            Delimiter::Whole => self.read_whole(options.backslash),
            Delimiter::Char(delimiter_byte) => self.read_char(*delimiter_byte, options),
            Delimiter::String(delimiter_bytes) => {
                self.read_string(delimiter_bytes, options);
            }
            Delimiter::WhitespaceDrop | Delimiter::WhitespaceKeep => {
                self.read_whitespace(options);
            }
        }
    }

    /// Parse a whole-record value.
    fn read_whole(&mut self, backslash: BackslashMode) {
        self.clear_output();
        let record_len = self.input.len();

        if matches!(backslash, BackslashMode::Off) {
            self.ptrs.push(0..record_len);
            return;
        }

        let mut i = 0;
        let mut escaped = false;
        while i < record_len {
            let byte = self.input[i];
            if escaped {
                self.push_decoded(decode_backslash_escape(byte));
                escaped = false;
                i += 1;
                continue;
            }
            if byte == b'\\' {
                self.promote_to_decoded(i);
                escaped = true;
                i += 1;
                continue;
            }
            if self.is_decoded() {
                self.push_decoded(byte);
            }
            i += 1;
        }
        if escaped {
            self.push_decoded(b'\\');
        }

        self.finish_column(0, record_len);
    }

    /// Parse a record with a single-byte delimiter.
    fn read_char(&mut self, delimiter: u8, options: &Config) {
        let plain = matches!(options.backslash, BackslashMode::Off)
            && matches!(options.quotes, Quotes::None);
        if plain {
            self.read_plain(delimiter);
            return;
        }

        self.clear_output();
        let record_len = self.input.len();
        let backslash_on = matches!(options.backslash, BackslashMode::On);
        let mut column_start = 0usize;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;
        let mut pending_quote_end = None;

        while i < record_len {
            let byte = self.input[i];

            if self.resolve_pending_char_quote(
                &mut pending_quote_end,
                delimiter,
                byte,
                &mut i,
                &mut column_start,
            ) {
                continue;
            }

            if backslash_on {
                if escaped {
                    self.push_decoded(decode_backslash_escape(byte));
                    escaped = false;
                    i += 1;
                    continue;
                }
                if byte == b'\\' {
                    self.promote_to_decoded(i);
                    escaped = true;
                    i += 1;
                    continue;
                }
            }

            if let Some(close) = active_close {
                if byte == close {
                    if i + 1 < record_len && self.input[i + 1] == close {
                        self.promote_to_decoded(i);
                        self.push_decoded(close);
                        i += 2;
                        continue;
                    }
                    if !self.is_decoded() {
                        pending_quote_end = Some(i);
                    }
                    active_close = None;
                    i += 1;
                    continue;
                }
                if self.is_decoded() {
                    self.push_decoded(byte);
                }
                i += 1;
                continue;
            }

            if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                if self.is_decoded() || i != column_start {
                    self.promote_to_decoded(i);
                } else {
                    column_start = i + 1;
                }
                active_close = Some(close);
                i += 1;
                continue;
            }

            if byte == delimiter {
                self.finish_column(column_start, i);
                column_start = self.next_column_start(i + 1);
                i += 1;
                continue;
            }

            if self.is_decoded() {
                self.push_decoded(byte);
            }
            i += 1;
        }

        if escaped {
            self.push_decoded(b'\\');
        }

        self.finish_column(column_start, pending_quote_end.unwrap_or(record_len));
    }

    /// Parse a record with a single-byte delimiter and no escape or quote handling.
    pub fn read_plain(&mut self, delimiter: u8) {
        self.clear_output();
        let record_len = self.input.len();

        let mut start = 0usize;

        for delimiter_index in memchr_iter(delimiter, &self.input) {
            self.ptrs.push(start..delimiter_index);
            start = delimiter_index + 1;
        }

        self.ptrs.push(start..record_len);
    }

    /// Parse a record with a byte-string delimiter.
    fn read_string(&mut self, delimiter: &[u8], options: &Config) {
        if delimiter.is_empty() {
            self.read_whole(options.backslash);
            return;
        }

        self.clear_output();
        let record_len = self.input.len();
        let backslash_on = matches!(options.backslash, BackslashMode::On);
        let mut column_start = 0usize;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;
        let mut pending_quote_end = None;

        while i < record_len {
            let byte = self.input[i];

            if self.resolve_pending_string_quote(
                &mut pending_quote_end,
                delimiter,
                &mut i,
                &mut column_start,
            ) {
                continue;
            }

            if backslash_on {
                if escaped {
                    self.push_decoded(decode_backslash_escape(byte));
                    escaped = false;
                    i += 1;
                    continue;
                }
                if byte == b'\\' {
                    self.promote_to_decoded(i);
                    escaped = true;
                    i += 1;
                    continue;
                }
            }

            if let Some(close) = active_close {
                if byte == close {
                    if i + 1 < record_len && self.input[i + 1] == close {
                        self.promote_to_decoded(i);
                        self.push_decoded(close);
                        i += 2;
                        continue;
                    }
                    if !self.is_decoded() {
                        pending_quote_end = Some(i);
                    }
                    active_close = None;
                    i += 1;
                    continue;
                }
                if self.is_decoded() {
                    self.push_decoded(byte);
                }
                i += 1;
                continue;
            }

            if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                if self.is_decoded() || i != column_start {
                    self.promote_to_decoded(i);
                } else {
                    column_start = i + 1;
                }
                active_close = Some(close);
                i += 1;
                continue;
            }

            if Self::delimiter_matches_at(&self.input, i, delimiter) {
                self.finish_column(column_start, i);
                column_start = self.next_column_start(i + delimiter.len());
                i += delimiter.len();
                continue;
            }

            if self.is_decoded() {
                self.push_decoded(byte);
            }
            i += 1;
        }

        if escaped {
            self.push_decoded(b'\\');
        }

        self.finish_column(column_start, pending_quote_end.unwrap_or(record_len));
    }

    /// Parse a record with whitespace delimiter behavior.
    fn read_whitespace(&mut self, options: &Config) {
        let keep = matches!(&options.delimiter, Delimiter::WhitespaceKeep);

        self.clear_output();
        let record_len = self.input.len();
        let backslash_on = matches!(options.backslash, BackslashMode::On);
        let mut column_start = 0usize;
        let mut in_field = false;
        let mut i = 0;
        let mut escaped = false;
        let mut active_close = None;
        let mut pending_whitespace_start = None;

        while i < record_len {
            let byte = self.input[i];

            if backslash_on {
                if escaped {
                    if !in_field {
                        column_start = self.next_column_start(i);
                        if keep && let Some(start) = pending_whitespace_start.take() {
                            if self.is_decoded() {
                                column_start = self.decoded.len();
                                self.extend_decoded_from_input(start..i - 1);
                            } else {
                                column_start = start;
                            }
                        }
                        in_field = true;
                    }
                    self.push_decoded(decode_backslash_escape(byte));
                    escaped = false;
                    i += 1;
                    continue;
                }
                if byte == b'\\' {
                    if !in_field {
                        column_start = i;
                        if keep && let Some(start) = pending_whitespace_start {
                            column_start = start;
                        }
                        in_field = true;
                    }
                    let was_decoded = self.is_decoded();
                    self.promote_to_decoded(i);
                    if keep
                        && was_decoded
                        && let Some(start) = pending_whitespace_start.take()
                    {
                        column_start = self.decoded.len();
                        self.extend_decoded_from_input(start..i);
                    } else {
                        pending_whitespace_start = None;
                    }
                    escaped = true;
                    i += 1;
                    continue;
                }
            }

            if let Some(close) = active_close {
                if byte == close {
                    if i + 1 < record_len && self.input[i + 1] == close {
                        self.promote_to_decoded(i);
                        self.push_decoded(close);
                        i += 2;
                        continue;
                    }
                    self.promote_to_decoded(i);
                    active_close = None;
                    i += 1;
                    continue;
                }
                if self.is_decoded() {
                    self.push_decoded(byte);
                }
                i += 1;
                continue;
            }

            if let Some(close) = quote_close_for_open(&options.quotes, byte) {
                if !in_field {
                    column_start = i;
                    if keep && let Some(start) = pending_whitespace_start.take() {
                        column_start = start;
                    }
                    in_field = true;
                }
                let was_decoded = self.is_decoded();
                self.promote_to_decoded(i);
                if keep && was_decoded && column_start < i {
                    let input_start = column_start;
                    column_start = self.decoded.len();
                    self.extend_decoded_from_input(input_start..i);
                }
                active_close = Some(close);
                i += 1;
                continue;
            }

            if byte.is_ascii_whitespace() {
                if in_field {
                    self.finish_column(column_start, i);
                }
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
                column_start = i;
                if keep && let Some(start) = pending_whitespace_start.take() {
                    if self.is_decoded() {
                        column_start = self.decoded.len();
                        self.extend_decoded_from_input(start..i);
                    } else {
                        column_start = start;
                    }
                }
                in_field = true;
            }

            if self.is_decoded() {
                self.push_decoded(byte);
            }
            i += 1;
        }

        if escaped {
            if !in_field {
                column_start = record_len - 1;
                if keep && let Some(start) = pending_whitespace_start.take() {
                    if self.is_decoded() {
                        column_start = self.decoded.len();
                        self.extend_decoded_from_input(start..record_len - 1);
                    } else {
                        column_start = start;
                    }
                }
                in_field = true;
            }
            self.push_decoded(b'\\');
        }

        if in_field {
            self.finish_column(column_start, record_len);
        }
    }

    /// If no quoting or escaping and single character delimiter
    /// extra fast all-in-one line and column parsing
    pub fn read_fast(
        &mut self,
        delim: u8,
        reader: &mut dyn BufRead,
        location: &mut FileLocData,
    ) -> anyhow::Result<ReadResult> {
        location.prev_bytes = location.bytes;
        location.line += 1;
        let mut start = 0usize;
        let mut first_time = true;
        loop {
            let available = reader.fill_buf()?;
            if available.is_empty() {
                if first_time {
                    location.line -= 1;
                    return Ok(ReadResult::Eof);
                }
                let end = self.input().len();
                self.ptrs.push(start..end);
                return Ok(ReadResult::NoTerminator);
            } else if first_time {
                first_time = false;
                self.clear();
            }

            for index in memchr3_iter(delim, b'\n', b'\r', available) {
                let end = self.input().len() + index;
                self.ptrs.push(start..end);
                start = end + 1;
                assert!(index < available.len());
                if available[index] == b'\r' {
                    self.input.extend_from_slice(&available[..index]);
                    let consume_len = if available.get(index + 1) == Some(&b'\n') {
                        index + 2
                    } else {
                        index + 1
                    };
                    let cr_ends_buffer = consume_len == available.len();
                    read_line::consume(reader, location, consume_len);
                    if cr_ends_buffer
                        && consume_len == index + 1
                        && read_line::consume_split_lf(reader, location)?
                    {
                        return Ok(ReadResult::CrLf);
                    }
                    return Ok(if consume_len == index + 2 {
                        ReadResult::CrLf
                    } else {
                        ReadResult::Cr
                    });
                } else if available[index] == b'\n' {
                    self.input.extend_from_slice(&available[..index]);
                    read_line::consume(reader, location, index + 1);
                    return Ok(ReadResult::Lf);
                }
            }
            self.input.extend_from_slice(available);
            let len = available.len();
            read_line::consume(reader, location, len);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::BufReader;
    use std::io::Cursor;

    fn run_fast_test(data: &[u8]) {
        let mut loc2 = FileLocData::new("test");
        let mut cols2 = Columns::new();
        let reader2 = Cursor::new(data);
        let mut f2 = BufReader::new(reader2);

        let config = Config::simple(b'\t');
        let eol2 = read_line::read(cols2.input_mut(), &mut f2, &config, &mut loc2).unwrap();
        cols2.read(&config);

        let mut cols3 = cols2.clone();
        let mut loc3 = loc2.clone();
        let eol3 = read_line::read(cols3.input_mut(), &mut f2, &config, &mut loc3).unwrap();
        cols3.read(&config);

        for capacity in [2, 3, 5, 7, 11, 100, 200] {
            let mut loc = FileLocData::new("test");
            let mut cols = Columns::new();
            let reader = Cursor::new(data);
            let mut f = BufReader::with_capacity(capacity, reader);
            let eol = cols.read_fast(b'\t', &mut f, &mut loc).unwrap();
            assert_eq!(eol, eol2);
            assert_eq!(cols, cols2);

            let eol = cols.read_fast(b'\t', &mut f, &mut loc).unwrap();
            assert_eq!(eol, eol3);
            assert_eq!(cols, cols3);
        }
    }

    #[test]
    fn fast_test_1() {
        run_fast_test(b"abc\tdef\tghi\n12345\t67\t891011\n");
        run_fast_test(b"abc\tdef\tghi\r12345\t67\t891011\r");
        run_fast_test(b"abc\tdef\tghi\r\n12345\t67\t891011\r\n");
        run_fast_test(b"abc\tdef\tghi\n12345\t67\t891011");
    }

    #[test]
    fn fast_tests() {
        run_fast_test(b"abc\tdef\tghi\n");
        run_fast_test(b"abc\tdef\tghi");
        run_fast_test(b"abc\tdef\tghi\r\n");
        run_fast_test(b"abc\tdef\tghi\r");

        run_fast_test(b"abc\tdef\tghi\naaaaa");
        run_fast_test(b"abc\tdef\tghi\r\naaaaa");
        run_fast_test(b"abc\tdef\tghi\raaaaa");
    }

    /// Parse one record and clone the current output columns.
    fn parse_record(record: &[u8], options: &Config) -> Vec<Vec<u8>> {
        let mut columns = Columns::default();
        columns.read_record(record, options);
        columns.iter().map(<[u8]>::to_vec).collect()
    }

    /// Parse one record and assert both parsed bytes and backing choice.
    fn assert_parse_backing(record: &[u8], options: &Config, backing: Backing, expected: &[&[u8]]) {
        let mut columns = Columns::default();
        columns.read_record(record, options);

        assert_eq!(columns.backing, backing);
        assert_eq!(columns.iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn char_delimiter_splits_basic_record() {
        assert_eq!(
            parse_record(b"a,b,", &Config::csv()),
            vec![b"a".to_vec(), b"b".to_vec(), Vec::new()]
        );
    }

    #[test]
    fn read_plain_splits_char_delimited_record() {
        let mut columns = Columns::default();
        columns.set_record(b",a,,b,");
        columns.read_plain(b',');

        assert_eq!(
            columns.iter().map(<[u8]>::to_vec).collect::<Vec<_>>(),
            vec![Vec::new(), b"a".to_vec(), Vec::new(), b"b".to_vec(), Vec::new()]
        );
    }

    #[test]
    fn plain_char_read_matches_read_plain() {
        let options = Config::new(Delimiter::Char(b','));
        let cases = [b"".as_slice(), b"a", b"a,b", b",a,,b,", b"\\n,\"quoted\",x"];

        for record in cases {
            let mut via_read = Columns::default();
            via_read.read_record(record, &options);

            let mut via_plain = Columns::default();
            via_plain.set_record(record);
            via_plain.read_plain(b',');

            assert!(via_read.iter().eq(via_plain.iter()));
        }
    }

    #[test]
    fn columns_converts_to_and_from_vec_without_changing_data() {
        let data = vec![b"a".to_vec(), b"b".to_vec()];
        let columns = Columns::from(data.clone());

        assert_eq!(columns.len(), 2);
        assert_eq!(&columns[0], b"a");
        assert_eq!(columns.get(1), b"b".as_slice());
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b"]);

        let round_tripped: Vec<Vec<u8>> = columns.into();
        assert_eq!(round_tripped, data);
    }

    #[test]
    fn columns_constructs_from_borrowed_slices() {
        let data = [b"a".as_slice(), b"bc".as_slice(), b"".as_slice()];

        let columns = Columns::from_slices(&data);
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"bc", b""]);

        let columns = Columns::from(data.as_slice());
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"bc", b""]);
    }

    #[test]
    fn columns_constructs_from_borrowed_vecs() {
        let data = vec![b"a".to_vec(), b"bc".to_vec(), Vec::new()];

        let columns = Columns::from_vecs(&data);
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"bc", b""]);

        let columns = Columns::from(data.as_slice());
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"bc", b""]);
    }

    #[test]
    fn owned_record_constructor_and_setter_parse_without_slice_copy_api() {
        let options = Config::new(Delimiter::Char(b','));
        let mut columns = Columns::with_record(b"a,b".to_vec());
        columns.read(&options);
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b"]);

        columns.set(b"x,y,z".to_vec());
        columns.read(&options);
        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"x".as_slice(), b"y", b"z"]);
    }

    #[test]
    fn input_mut_allows_filling_reusable_record_buffer() {
        let mut columns = Columns::default();
        columns.input_mut().extend_from_slice(b"a,b");
        columns.read(&Config::new(Delimiter::Char(b',')));

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b"]);
    }

    #[test]
    fn uses_decoded_buffer_reports_active_backing() {
        let mut columns = Columns::with_record(b"a\tb".to_vec());
        columns.read(&Config::tsv());
        assert!(!columns.uses_decoded_buffer());

        columns.set(b"a\\tb".to_vec());
        columns.read(&Config::tsv());
        assert!(columns.uses_decoded_buffer());
    }

    #[test]
    fn columns_iterator_supports_reverse_and_fused_iteration() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));

        let mut iter = columns.iter();
        assert_eq!(iter.next_back(), Some(b"c".as_slice()));
        assert_eq!(iter.next(), Some(b"a".as_slice()));
        assert_eq!(iter.next_back(), Some(b"b".as_slice()));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn dedup_by_removes_consecutive_matching_columns() {
        let mut columns = Columns::with_record(b"a,a,b,a".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.dedup_by(|current, previous| current == previous);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b", b"a"]);
    }

    #[test]
    fn dedup_by_uses_vec_dedup_by_argument_order() {
        let mut columns = Columns::with_record(b"a,aa,b".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.dedup_by(|current, previous| current.len() > previous.len());

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b"]);
    }

    #[test]
    fn dedup_by_works_with_decoded_backing() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.backslash = BackslashMode::On;

        let mut columns = Columns::with_record(b"a\\t,a\\t,b".to_vec());
        columns.read(&options);
        assert!(columns.uses_decoded_buffer());

        columns.dedup_by(|current, previous| current == previous);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a\t".as_slice(), b"b"]);
    }

    #[test]
    fn dedup_removes_consecutive_equal_columns() {
        let mut columns = Columns::with_record(b"a,a,b,b,a".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.dedup();

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b", b"a"]);
    }

    #[test]
    fn retain_keeps_matching_columns() {
        let mut columns = Columns::with_record(b"a,bbb,cc,d".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.retain(|column| column.len() > 1);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"bbb".as_slice(), b"cc"]);
    }

    #[test]
    fn retain_works_with_decoded_backing() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.backslash = BackslashMode::On;

        let mut columns = Columns::with_record(b"a\\t,b,c\\t".to_vec());
        columns.read(&options);
        assert!(columns.uses_decoded_buffer());

        columns.retain(|column| column.ends_with(b"\t"));

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a\t".as_slice(), b"c\t"]);
    }

    #[test]
    fn sort_by_reorders_input_backed_columns() {
        let mut columns = Columns::with_record(b"b,a,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.sort_by(<[u8]>::cmp);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b", b"c"]);
        assert!(!columns.uses_decoded_buffer());
    }

    #[test]
    fn sort_by_accepts_custom_order() {
        let mut columns = Columns::with_record(b"a,bbb,cc".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.sort_by(|left, right| right.len().cmp(&left.len()));

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"bbb".as_slice(), b"cc", b"a"]);
    }

    #[test]
    fn sort_by_works_with_decoded_backing() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.backslash = BackslashMode::On;

        let mut columns = Columns::with_record(b"b\\t,a\\t,c".to_vec());
        columns.read(&options);
        assert!(columns.uses_decoded_buffer());

        columns.sort_by(<[u8]>::cmp);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a\t".as_slice(), b"b\t", b"c"]);
    }

    #[test]
    fn sort_orders_columns_by_bytes() {
        let mut columns = Columns::with_record(b"b,a,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.sort();

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b", b"c"]);
    }

    #[test]
    fn reverse_reverses_column_order() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.reverse();

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"c".as_slice(), b"b", b"a"]);
    }

    #[test]
    fn swap_exchanges_columns() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.swap(0, 2);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"c".as_slice(), b"b", b"a"]);
    }

    #[test]
    fn truncate_shortens_columns() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.truncate(2);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"b"]);
    }

    #[test]
    fn clear_removes_columns_but_keeps_input() {
        let mut columns = Columns::with_record(b"a,b".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.clear_cols();

        assert!(columns.is_empty());
        assert_eq!(columns.input(), b"a,b");
    }

    #[test]
    fn remove_drops_column_and_preserves_order() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.remove(1);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"a".as_slice(), b"c"]);
    }

    #[test]
    fn swap_remove_drops_column_without_preserving_order() {
        let mut columns = Columns::with_record(b"a,b,c".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));
        columns.swap_remove(0);

        assert_eq!(columns.iter().collect::<Vec<_>>(), vec![b"c".as_slice(), b"b"]);
    }

    #[test]
    fn binary_search_by_searches_sorted_columns() {
        let mut columns = Columns::with_record(b"a,b,d".to_vec());
        columns.read(&Config::new(Delimiter::Char(b',')));

        assert_eq!(columns.binary_search_by(|column| column.cmp(b"b")), Ok(1));
        assert_eq!(columns.binary_search_by(|column| column.cmp(b"c")), Err(2));
    }

    #[test]
    fn binary_search_by_works_with_decoded_backing() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.backslash = BackslashMode::On;

        let mut columns = Columns::with_record(b"a\\t,b\\t,d\\t".to_vec());
        columns.read(&options);
        assert!(columns.uses_decoded_buffer());

        assert_eq!(columns.binary_search_by(|column| column.cmp(b"b\t")), Ok(1));
        assert_eq!(columns.binary_search_by(|column| column.cmp(b"c\t")), Err(2));
    }

    #[test]
    fn unchanged_backslash_mode_record_stays_input_backed() {
        assert_parse_backing(b"a\tb", &Config::tsv(), Backing::Input, &[b"a".as_slice(), b"b"]);
    }

    #[test]
    fn backslash_mode_promotes_only_when_escape_is_seen() {
        assert_parse_backing(b"a\\tb", &Config::tsv(), Backing::Decoded, &[b"a\tb"]);
    }

    #[test]
    fn quote_mode_without_quotes_stays_input_backed() {
        assert_parse_backing(b"a,b", &Config::csv(), Backing::Input, &[b"a".as_slice(), b"b"]);
    }

    #[test]
    fn plain_char_delimiter_stays_input_backed() {
        let options = Config::new(Delimiter::Char(b','));
        assert_parse_backing(b"a,b,", &options, Backing::Input, &[b"a".as_slice(), b"b", b""]);
    }

    #[test]
    fn wrapped_quoted_fields_stay_input_backed() {
        assert_parse_backing(
            br#""abc","def","ghi""#,
            &Config::csv(),
            Backing::Input,
            &[b"abc".as_slice(), b"def", b"ghi"],
        );
    }

    #[test]
    fn quoted_delimiter_stays_input_backed() {
        assert_parse_backing(
            br#""a,b",c"#,
            &Config::csv(),
            Backing::Input,
            &[b"a,b".as_slice(), b"c"],
        );
    }

    #[test]
    fn doubled_quote_promotes_to_decoded() {
        assert_parse_backing(
            br#""a""b",c"#,
            &Config::csv(),
            Backing::Decoded,
            &[br#"a"b"#.as_slice(), b"c"],
        );
    }

    #[test]
    fn mid_field_quote_promotes_to_decoded() {
        assert_parse_backing(
            br#"a"b",c"#,
            &Config::csv(),
            Backing::Decoded,
            &[b"ab".as_slice(), b"c"],
        );
    }

    #[test]
    fn post_quote_field_continuation_promotes_to_decoded() {
        assert_parse_backing(
            br#""a"b,c"#,
            &Config::csv(),
            Backing::Decoded,
            &[b"ab".as_slice(), b"c"],
        );
    }

    #[test]
    fn open_quote_at_record_end_stays_input_backed() {
        assert_parse_backing(b"\"abc", &Config::csv(), Backing::Input, &[b"abc"]);
    }

    #[test]
    fn whole_without_escape_stays_input_backed() {
        assert_parse_backing(b"a,b", &Config::whole(), Backing::Input, &[b"a,b"]);
    }

    #[test]
    fn whole_with_escape_promotes_to_decoded() {
        let mut options = Config::whole();
        options.backslash = BackslashMode::On;
        assert_parse_backing(b"a\\nb", &options, Backing::Decoded, &[b"a\nb"]);
    }

    #[test]
    fn string_delimiter_without_escape_stays_input_backed() {
        let options = Config::new(Delimiter::String(b"::".to_vec()));
        assert_parse_backing(b"a::b::", &options, Backing::Input, &[b"a".as_slice(), b"b", b""]);
    }

    #[test]
    fn string_delimiter_wrapped_quote_stays_input_backed() {
        let mut options = Config::new(Delimiter::String(b"::".to_vec()));
        options.quotes = Quotes::Single(b'"', b'"');
        assert_parse_backing(
            br#""a::b"::c"#,
            &options,
            Backing::Input,
            &[b"a::b".as_slice(), b"c"],
        );
    }

    #[test]
    fn whitespace_drop_plain_stays_input_backed() {
        let options = Config::new(Delimiter::WhitespaceDrop);
        assert_parse_backing(b"  a\t b  ", &options, Backing::Input, &[b"a".as_slice(), b"b"]);
    }

    #[test]
    fn whitespace_keep_plain_stays_input_backed() {
        let options = Config::new(Delimiter::WhitespaceKeep);
        assert_parse_backing(b"  a\t b  ", &options, Backing::Input, &[b"  a".as_slice(), b"\t b"]);
    }

    #[test]
    fn whitespace_keep_backslash_promotes_only_when_escape_is_seen() {
        let mut options = Config::new(Delimiter::WhitespaceKeep);
        options.backslash = BackslashMode::On;

        assert_parse_backing(b"  a\t b", &options, Backing::Input, &[b"  a".as_slice(), b"\t b"]);
        assert_parse_backing(b"  \\ta", &options, Backing::Decoded, &[b"  \ta".as_slice()]);
    }

    #[test]
    fn csv_quotes_remove_wrappers_and_decode_doubled_quote() {
        assert_eq!(
            parse_record(br#""a,b","c""d",e"#, &Config::csv()),
            vec![b"a,b".to_vec(), b"c\"d".to_vec(), b"e".to_vec()]
        );
    }

    #[test]
    fn open_quote_is_accepted_at_record_end() {
        assert_eq!(parse_record(b"\"a,b", &Config::csv()), vec![b"a,b".to_vec()]);
    }

    #[test]
    fn tsv_decodes_backslash_escapes() {
        assert_eq!(
            parse_record(b"a\\tb\\n\\q\tc\\\\", &Config::tsv()),
            vec![b"a\tb\nq".to_vec(), b"c\\".to_vec()]
        );
    }

    #[test]
    fn whole_record_keeps_bytes_unless_backslash_is_enabled() {
        let mut options = Config::whole();
        assert_eq!(parse_record(b"a,b\tc", &options), vec![b"a,b\tc".to_vec()]);

        options.backslash = BackslashMode::On;
        assert_eq!(parse_record(b"a\\nb", &options), vec![b"a\nb".to_vec()]);
    }

    #[test]
    fn string_delimiter_matches_left_to_right() {
        let options = Config::new(Delimiter::String(b"::".to_vec()));
        assert_eq!(
            parse_record(b"a:::b::", &options),
            vec![b"a".to_vec(), b":b".to_vec(), Vec::new()]
        );
    }

    #[test]
    fn empty_string_delimiter_matches_whole_record() {
        let options = Config::new(Delimiter::String(Vec::new()));
        assert_eq!(parse_record(b"abc", &options), vec![b"abc".to_vec()]);
    }

    #[test]
    fn whitespace_drop_discards_repeated_and_edge_whitespace() {
        let options = Config::new(Delimiter::WhitespaceDrop);
        assert_eq!(parse_record(b"  a\t b  ", &options), vec![b"a".to_vec(), b"b".to_vec()]);
    }

    #[test]
    fn whitespace_keep_prefixes_next_field_and_drops_trailing_whitespace() {
        let options = Config::new(Delimiter::WhitespaceKeep);
        assert_eq!(parse_record(b"  a\t b  ", &options), vec![b"  a".to_vec(), b"\t b".to_vec()]);
    }

    #[test]
    fn whitespace_keep_with_backslash_copies_pending_whitespace_from_record() {
        let mut options = Config::new(Delimiter::WhitespaceKeep);
        options.backslash = BackslashMode::On;

        assert_eq!(
            parse_record(b"  \\ta \\ b", &options),
            vec![b"  \ta".to_vec(), b"  b".to_vec()]
        );
    }

    #[test]
    fn multi_quote_supports_multiple_pairs() {
        let mut options = Config::new(Delimiter::Char(b','));
        options.quotes = Quotes::Multi(vec![(b'"', b'"'), (b'[', b']')]);

        assert_eq!(
            parse_record(b"\"a,b\",[c]]d],e", &options),
            vec![b"a,b".to_vec(), b"c]d".to_vec(), b"e".to_vec()]
        );
    }

    #[test]
    fn clf_constructor_groups_bracketed_and_quoted_values() {
        let options = Config::clf();
        let record =
            b"127.0.0.1 - - [01/May/2025:07:20:10 +0000] \"GET /index.html HTTP/1.1\" 200 123";

        assert_eq!(
            parse_record(record, &options),
            vec![
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
        let parsed = Config::from_spec("log,d=whitespace_drop,q=none,b=off").unwrap();
        assert_eq!(parsed.delimiter, Delimiter::WhitespaceDrop);
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
