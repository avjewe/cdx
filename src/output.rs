//! Column-oriented output formatting for delimited text.

use crate::*;
use memchr::{memchr2, memchr3};
use read_line::BackslashMode;
use read_line::Quotes;
use std::collections::HashSet;
use std::fmt;
use std::io;
use std::str::FromStr;

/// What to do if a column value contains the column delimiter or a newline
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Escape {
    /// Surround whole column with quotes, and double any quote bytes inside the column.
    QuoteDoubled(u8, u8), // (open, close)
    /// Always surround whole column with quotes, and double any quote bytes inside the column.
    AlwaysQuoteDoubled(u8, u8), // (open, close)
    /// Surround whole column with quotes, and use backslash to escape quote bytes inside it.
    QuoteBackslash(u8, u8), // (open, close)
    /// Always surround whole column with quotes, and use backslash to escape quote bytes inside it.
    AlwaysQuoteBackslash(u8, u8), // (open, close)
    /// Use backslash to escape column delimiters or newlines inside the column.
    Backslash,
    /// Throw an error if a column value contains the column delimiter or a newline.
    Error,
    /// Do nothing, even if the output is not valid according to the column delimiter.
    Trust,
    /// Replace column delimiters or newlines inside the column with the given byte.
    Replace(u8),
    /// Delete column delimiters or newlines inside the column.
    Delete,
}

/// Whether to emit a header record when writing tabular output.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Header {
    /// Emit a CDX header record.
    Cdx,
    /// Emit a plain header record.
    Yes,
    /// Do not emit a header record.
    No,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Configuration for outputting columns, including the column delimiter and how to escape values that contain the delimiter or newlines.
pub struct Config {
    /// The column delimiter to use when writing output columns.
    pub delimiter: u8,
    /// How to escape column values that contain the column delimiter or newlines.
    pub escape: Escape,
    /// Whether output should include a header record.
    pub header: Header,
}

impl Default for Config {
    fn default() -> Self {
        Self { delimiter: b'\t', escape: Escape::Replace(b' '), header: Header::Yes }
    }
}

/// Optional output overrides resolved against an [`input_file::Config`](crate::input_file::Config).
///
/// Each `Some(...)` value overrides the corresponding output setting.
/// Each `None` value is derived from the input configuration.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_copy_implementations)]
pub struct Spec {
    /// Optional output delimiter override.
    pub delimiter: Option<u8>,
    /// Optional output escape-mode override.
    pub escape: Option<Escape>,
    /// Optional output header-mode override.
    pub header: Option<Header>,
}

/// Trait alias used by [`LineWriter`] for thread-safe, debuggable sinks.
pub trait MyWrite: io::Write + Send + Sync + fmt::Debug {}

impl<T> MyWrite for T where T: io::Write + Send + Sync + fmt::Debug {}

/// Write delimiter-separated records to an output.
///
/// # Examples
/// Whole record at once:
/// ```rust
/// use cdx::output::{Config, Escape, LineWriter};
///
/// let mut line_writer = LineWriter::new(
///     Config::new(b',', Escape::Trust),
///     b"\n".to_vec(),
///     Box::new(Vec::<u8>::new()),
/// );
/// line_writer.write_record([&b"a"[..], &b"b"[..]])?;
/// # Ok::<(), anyhow::Error>(())
/// ```
///
/// Column-at-a-time:
/// ```rust
/// use cdx::output::{Config, Escape, LineWriter};
///
/// let mut line_writer = LineWriter::new(
///     Config::new(b',', Escape::Trust),
///     b"\n".to_vec(),
///     Box::new(Vec::<u8>::new()),
/// );
/// line_writer.write_column(b"a")?;
/// line_writer.write_column(b"b")?;
/// line_writer.finish_record()?;
/// # Ok::<(), anyhow::Error>(())
/// ```
///
/// Chunked column:
/// ```rust
/// use cdx::output::{Config, Escape, LineWriter};
///
/// let mut line_writer = LineWriter::new(
///     Config::new(b',', Escape::Trust),
///     b"\n".to_vec(),
///     Box::new(Vec::<u8>::new()),
/// );
/// line_writer.begin_column()?;
/// line_writer.write_column_chunk(b"part1")?;
/// line_writer.write_column_chunk(b"part2")?;
/// line_writer.end_column()?;
/// line_writer.finish_record()?;
/// # Ok::<(), anyhow::Error>(())
/// ```
#[derive(Debug)]
pub struct LineWriter {
    /// The configuration for how to write output columns.
    config: Config,
    /// The byte(s) to write at the end of each line (e.g. `\n` or `\r\n`).
    eol: Vec<u8>,
    /// The underlying writer to which the output will be written.
    writer: Box<dyn MyWrite>,
    /// Reused buffer for one full encoded record before writing to the underlying writer.
    record_buf: Vec<u8>,
    /// Reused buffer for chunked-column mode.
    column_buf: Vec<u8>,
    /// Whether chunked-column mode is currently active.
    column_open: bool,
    /// Whether at least one column has been written in the current record.
    wrote_any_column: bool,
}

/// Return whether `data` contains any byte that requires escaping in plain mode.
#[inline]
fn has_delimiter_or_newline(data: &[u8], delimiter: u8) -> bool {
    find_replace_target(data, delimiter).is_some()
}

/// Find the next close-quote byte that must be escaped in doubled-quote mode.
///
/// For asymmetric quote pairs (`open != close`), only the close quote is escaped.
#[inline]
fn find_close_quote_target(haystack: &[u8], _open: u8, close: u8) -> Option<usize> {
    memchr::memchr(close, haystack)
}

/// Encode one quoted column using doubled close-quote escaping.
///
/// Clean byte runs are copied in bulk for better throughput.
fn write_quoted_doubled(data: &[u8], out: &mut Vec<u8>, open: u8, close: u8) {
    out.push(open);

    let mut start = 0usize;
    while let Some(found_rel) = find_close_quote_target(&data[start..], open, close) {
        let found = start + found_rel;
        if found > start {
            out.extend_from_slice(&data[start..found]);
        }
        let byte = data[found];
        out.push(byte);
        out.push(byte);
        start = found + 1;
    }

    out.extend_from_slice(&data[start..]);
    out.push(close);
}

/// Find the next byte that must be escaped in quote-backslash mode.
///
/// Escapable bytes are close-quote and backslash.
#[inline]
fn find_close_quote_or_backslash_target(haystack: &[u8], open: u8, close: u8) -> Option<usize> {
    if open == close && close == b'\\' {
        return memchr::memchr(b'\\', haystack);
    }
    let quote = if open == close { open } else { close };
    if quote == b'\\' {
        return memchr::memchr(b'\\', haystack);
    }
    memchr2(quote, b'\\', haystack)
}

/// Encode one quoted column using backslash escapes inside the quoted content.
///
/// Clean byte runs are copied in bulk for better throughput.
fn write_quoted_backslash(data: &[u8], out: &mut Vec<u8>, open: u8, close: u8) {
    out.push(open);

    let mut start = 0usize;
    while let Some(found_rel) = find_close_quote_or_backslash_target(&data[start..], open, close) {
        let found = start + found_rel;
        if found > start {
            out.extend_from_slice(&data[start..found]);
        }
        out.push(b'\\');
        out.push(data[found]);
        start = found + 1;
    }

    out.extend_from_slice(&data[start..]);
    out.push(close);
}

/// Find the next byte that must be escaped in backslash mode.
///
/// Escapable bytes are newline, carriage return, backslash, and delimiter.
#[inline]
fn find_backslash_escape_target(haystack: &[u8], delimiter: u8) -> Option<usize> {
    if delimiter == b'\n' || delimiter == b'\r' || delimiter == b'\\' {
        return memchr3(b'\n', b'\r', b'\\', haystack);
    }

    match (memchr3(b'\n', b'\r', b'\\', haystack), memchr::memchr(delimiter, haystack)) {
        (Some(left), Some(right)) => Some(left.min(right)),
        (Some(left), None) => Some(left),
        (None, Some(right)) => Some(right),
        (None, None) => None,
    }
}

/// Encode one column using backslash escapes (`\n`, `\r`, `\\`, and escaped delimiter).
///
/// Clean byte runs are copied in bulk for better throughput.
fn write_backslash_escaped(data: &[u8], out: &mut Vec<u8>, delimiter: u8) {
    let mut start = 0usize;
    while let Some(found_rel) = find_backslash_escape_target(&data[start..], delimiter) {
        let found = start + found_rel;
        if found > start {
            out.extend_from_slice(&data[start..found]);
        }
        match data[found] {
            b'\n' => out.extend_from_slice(br"\n"),
            b'\r' => out.extend_from_slice(br"\r"),
            b'\\' => out.extend_from_slice(br"\\"),
            byte if byte == delimiter => {
                out.push(b'\\');
                out.push(delimiter);
            }
            _ => unreachable!("find_backslash_escape_target returned non-special byte"),
        }
        start = found + 1;
    }

    out.extend_from_slice(&data[start..]);
}

/// Find the next delimiter/newline byte targeted by replacement/deletion modes.
#[inline]
fn find_replace_target(haystack: &[u8], delimiter: u8) -> Option<usize> {
    if delimiter == b'\n' || delimiter == b'\r' {
        memchr2(b'\n', b'\r', haystack)
    } else {
        memchr3(delimiter, b'\n', b'\r', haystack)
    }
}

/// Return whether the column contains a close quote and may need quoting.
#[inline]
fn contains_quote_byte(data: &[u8], open: u8, close: u8) -> bool {
    find_close_quote_target(data, open, close).is_some()
}

/// Return whether the column contains a close quote or backslash.
#[inline]
fn contains_quote_or_backslash(data: &[u8], open: u8, close: u8) -> bool {
    find_close_quote_or_backslash_target(data, open, close).is_some()
}

/// Copy `data` while replacing or deleting delimiter/newline bytes.
///
/// If `replacement` is `Some(byte)`, each target byte is replaced by `byte`.
/// If `replacement` is `None`, target bytes are dropped.
fn write_replaced_or_deleted(
    data: &[u8],
    out: &mut Vec<u8>,
    delimiter: u8,
    replacement: Option<u8>,
) {
    let Some(first) = find_replace_target(data, delimiter) else {
        out.extend_from_slice(data);
        return;
    };

    if first > 0 {
        out.extend_from_slice(&data[..first]);
    }
    if let Some(byte) = replacement {
        out.push(byte);
    }

    let mut start = first + 1;
    while let Some(found_rel) = find_replace_target(&data[start..], delimiter) {
        let found = start + found_rel;
        if found > start {
            out.extend_from_slice(&data[start..found]);
        }
        if let Some(byte) = replacement {
            out.push(byte);
        }
        start = found + 1;
    }
    out.extend_from_slice(&data[start..]);
}

/// Collapse input quote modes into an optional single quote pair for output derivation.
///
/// `Quotes::Multi` uses only the first pair. An empty `Multi` is treated as no quotes.
fn input_quote_pair(quotes: &Quotes) -> Option<(u8, u8)> {
    match quotes {
        Quotes::None => None,
        Quotes::Single(open, close) => Some((*open, *close)),
        Quotes::Multi(pairs) => pairs.first().copied(),
    }
}

/// Derive output delimiter when `Spec.delimiter` is not explicitly provided.
const fn derive_delimiter(input: &input_file::Config, spec: &Spec) -> u8 {
    if let Some(delimiter) = spec.delimiter {
        return delimiter;
    }

    match input.column_config.delimiter {
        input::Delimiter::Char(delimiter) => delimiter,
        _ => b'\t',
    }
}

/// Derive output escape mode when `Spec.escape` is not explicitly provided.
fn derive_escape(input: &input_file::Config, spec: &Spec, output_delimiter: u8) -> Escape {
    if let Some(escape) = spec.escape {
        return escape;
    }

    match (input.column_config.backslash, input_quote_pair(&input.column_config.quotes)) {
        (BackslashMode::Off, None) => {
            let replacement = if output_delimiter == b' ' { b'.' } else { b' ' };
            Escape::Replace(replacement)
        }
        (BackslashMode::Off, Some((open, close))) => Escape::QuoteDoubled(open, close),
        (BackslashMode::On, None) => Escape::Backslash,
        (BackslashMode::On, Some((open, close))) => Escape::QuoteBackslash(open, close),
    }
}

/// Derive output header mode when `Spec.header` is not explicitly provided.
const fn derive_header(input: &input_file::Config, spec: &Spec) -> Header {
    if let Some(header) = spec.header {
        return header;
    }

    if let Some(saw_header) = input.saw_header {
        return match saw_header {
            input_file::SawHeader::Yes | input_file::SawHeader::Cdx => Header::Yes,
            input_file::SawHeader::No | input_file::SawHeader::Empty => Header::No,
        };
    }

    match input.header {
        input_file::Header::Yes => Header::Yes,
        input_file::Header::No => Header::No,
    }
}

impl Spec {
    /// Construct an empty spec that derives all output fields from input config.
    #[must_use]
    pub const fn new() -> Self {
        Self { delimiter: None, escape: None, header: None }
    }

    /// Construct a spec equivalent to [`Config::tsv`], with every field explicitly set.
    #[must_use]
    pub const fn tsv() -> Self {
        Self { delimiter: Some(b'\t'), escape: Some(Escape::Backslash), header: Some(Header::Yes) }
    }

    /// Construct a spec equivalent to [`Config::csv`], with every field explicitly set.
    #[must_use]
    pub const fn csv() -> Self {
        Self {
            delimiter: Some(b','),
            escape: Some(Escape::QuoteDoubled(b'"', b'"')),
            header: Some(Header::Yes),
        }
    }

    /// Construct an unconstrained spec (alias of [`Spec::new`]).
    #[must_use]
    pub const fn any() -> Self {
        Self::new()
    }

    /// Parse a command-line style output spec.
    ///
    /// Format:
    /// - `base,key=value,key=value`, where `base` is `csv` or `tsv`; or
    /// - `key=value,key=value` (base omitted, starts from [`Spec::any`]).
    ///
    /// If the first segment contains `=`, it is treated as an override (no base).
    /// If the first segment does not contain `=`, it must be `csv` or `tsv`.
    ///
    /// Supported keys:
    /// - `d` / `delimiter`
    /// - `e` / `escape`
    /// - `h` / `header`
    /// - `q` / `quotes` (for quote-based escapes)
    /// - `x` / `replace` (for `escape=replace`)
    pub fn from_spec(spec: &str) -> anyhow::Result<Self> {
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Ok(Self::any());
        }

        let mut parts = trimmed.split(',').map(str::trim);
        let first = parts.next().ok_or_else(|| anyhow::anyhow!("missing output spec"))?;

        let mut out;
        let mut escape_kind;
        let mut quote_pair;
        let mut replace_value;
        let mut seen_keys: HashSet<&'static str> = HashSet::new();
        let mut quote_overridden = false;
        let mut replace_overridden = false;

        if first.contains('=') {
            out = Self::any();
            escape_kind = None;
            quote_pair = None;
            replace_value = None;
        } else {
            let base_norm = normalize_token(first);
            out = match base_norm.as_str() {
                "csv" => Self::csv(),
                "tsv" => Self::tsv(),
                _ => {
                    return Err(anyhow::anyhow!(format!(
                        "unknown output spec base `{first}`; expected one of: csv, tsv"
                    )));
                }
            };

            let (kind, pair, replace) =
                escape_parts(out.escape.expect("Spec::csv and Spec::tsv always set escape"));
            escape_kind = Some(kind);
            quote_pair = Some(pair);
            replace_value = replace;
        }

        let mut apply_override = |part: &str| -> anyhow::Result<()> {
            if part.is_empty() {
                return Err(anyhow::anyhow!("empty override segment in output spec"));
            }

            let (key_raw, value_raw) = part.split_once('=').ok_or_else(|| {
                anyhow::anyhow!(format!("output spec override `{part}` must be in key=value form"))
            })?;

            let key_norm = normalize_token(key_raw);
            let key = canonical_output_key(&key_norm).ok_or_else(|| {
                anyhow::anyhow!(format!(
                    "unknown output spec key `{key_raw}`; expected one of: d|delimiter, e|escape, h|header, q|quotes, x|replace"
                ))
            })?;
            if !seen_keys.insert(key) {
                return Err(anyhow::anyhow!(format!("duplicate output spec key `{key_raw}`")));
            }

            match key {
                "delimiter" => {
                    out.delimiter = Some(parse_byte_token(value_raw, "delimiter")?);
                }
                "escape" => {
                    escape_kind = Some(parse_escape_kind(value_raw)?);
                }
                "header" => {
                    out.header = Some(parse_header_mode(value_raw)?);
                }
                "quotes" => {
                    quote_pair = Some(parse_output_quote_pair(value_raw)?);
                    quote_overridden = true;
                }
                "replace" => {
                    replace_value = Some(parse_byte_token(value_raw, "replace")?);
                    replace_overridden = true;
                }
                _ => unreachable!("all output keys are canonicalized above"),
            }

            Ok(())
        };

        if first.contains('=') {
            apply_override(first)?;
        }
        for part in parts {
            apply_override(part)?;
        }

        if quote_overridden && !escape_kind.is_some_and(escape_kind_uses_quotes) {
            return Err(anyhow::anyhow!(
                "quotes override requires a quote-based escape mode via base or `e=`",
            ));
        }

        if replace_overridden && !matches!(escape_kind, Some(EscapeKind::Replace)) {
            return Err(anyhow::anyhow!(
                "replace override requires `escape=replace` via base or `e=`",
            ));
        }

        if let Some(kind) = escape_kind {
            out.escape = Some(match kind {
                EscapeKind::QuoteDoubled => {
                    let (open, close) = quote_pair.unwrap_or((b'"', b'"'));
                    Escape::QuoteDoubled(open, close)
                }
                EscapeKind::AlwaysQuoteDoubled => {
                    let (open, close) = quote_pair.unwrap_or((b'"', b'"'));
                    Escape::AlwaysQuoteDoubled(open, close)
                }
                EscapeKind::QuoteBackslash => {
                    let (open, close) = quote_pair.unwrap_or((b'"', b'"'));
                    Escape::QuoteBackslash(open, close)
                }
                EscapeKind::AlwaysQuoteBackslash => {
                    let (open, close) = quote_pair.unwrap_or((b'"', b'"'));
                    Escape::AlwaysQuoteBackslash(open, close)
                }
                EscapeKind::Backslash => Escape::Backslash,
                EscapeKind::Error => Escape::Error,
                EscapeKind::Trust => Escape::Trust,
                EscapeKind::Delete => Escape::Delete,
                EscapeKind::Replace => {
                    let value = replace_value
                        .ok_or_else(|| anyhow::anyhow!("`escape=replace` requires `x=<byte>`"))?;
                    Escape::Replace(value)
                }
            });
        }

        Ok(out)
    }
}

impl Config {
    /// Construct a `Config` with the given delimiter and escaping behavior.
    ///
    /// # Examples
    /// ```rust
    /// use cdx::output::{Config, Escape};
    ///
    /// let config = Config::new(b',', Escape::QuoteDoubled(b'"', b'"'));
    /// assert_eq!(config.delimiter, b',');
    /// ```
    #[must_use]
    pub const fn new(delimiter: u8, escape: Escape) -> Self {
        Self { delimiter, escape, header: Header::Yes }
    }

    /// Construct a `Config` with an explicit header policy.
    ///
    /// # Examples
    /// ```rust
    /// use cdx::output::{Config, Escape, Header};
    ///
    /// let config = Config::with_header(b',', Escape::QuoteDoubled(b'"', b'"'), Header::Yes);
    /// assert_eq!(config.header, Header::Yes);
    /// ```
    #[must_use]
    pub const fn with_header(delimiter: u8, escape: Escape, header: Header) -> Self {
        Self { delimiter, escape, header }
    }

    /// Recommended default for TSV-like output.
    ///
    /// Uses tab as the delimiter and backslash escaping for delimiter/newline bytes.
    ///
    /// # Examples
    /// ```rust
    /// use cdx::output::{Config, Escape};
    ///
    /// let config = Config::tsv();
    /// assert_eq!(config.delimiter, b'\t');
    /// assert_eq!(config.escape, Escape::Backslash);
    /// ```
    #[must_use]
    pub const fn tsv() -> Self {
        Self { delimiter: b'\t', escape: Escape::Backslash, header: Header::Yes }
    }

    /// Recommended default for CSV output.
    ///
    /// Uses comma as the delimiter and RFC4180-style doubled quote escaping.
    ///
    /// # Examples
    /// ```rust
    /// use cdx::output::{Config, Escape};
    ///
    /// let config = Config::csv();
    /// assert_eq!(config.delimiter, b',');
    /// assert_eq!(config.escape, Escape::QuoteDoubled(b'"', b'"'));
    /// ```
    #[must_use]
    pub const fn csv() -> Self {
        Self { delimiter: b',', escape: Escape::QuoteDoubled(b'"', b'"'), header: Header::Yes }
    }

    /// Build output config by applying `spec` overrides to values derived from `input`.
    ///
    /// Derivation rules:
    /// - Delimiter: `Char(x)` maps to `x`; all other input delimiters map to tab.
    /// - Quotes: `Multi` uses only its first pair (empty `Multi` is treated as no quotes).
    /// - Escape:
    ///   - `Backslash::Off + Quotes::None` => `Replace(space)`; if output delimiter is space, `Replace('.')`.
    ///   - `Backslash::Off + Quotes::Single` => `QuoteDoubled`.
    ///   - `Backslash::On + Quotes::None` => `Backslash`.
    ///   - `Backslash::On + Quotes::Single` => `QuoteBackslash`.
    /// - Header:
    ///   - if `input.saw_header` is present, `Yes/Cdx => Yes`, `No => No`
    ///   - otherwise mirrors `input.header`.
    #[must_use]
    pub fn from_input_and_spec(input: &input_file::Config, spec: &Spec) -> Self {
        let delimiter = derive_delimiter(input, spec);
        let escape = derive_escape(input, spec, delimiter);
        let header = derive_header(input, spec);
        Self { delimiter, escape, header }
    }

    /// Build output config by applying `spec` overrides to values derived from `input.config()`.
    #[must_use]
    pub fn from_spec(input: &TextFile, spec: &Spec) -> Self {
        Self::from_input_and_spec(input.config(), spec)
    }

    /// Encode one column into `out` according to this config.
    pub fn write_column(&self, data: &[u8], out: &mut Vec<u8>) -> anyhow::Result<()> {
        match self.escape {
            Escape::Trust => {
                out.extend_from_slice(data);
                Ok(())
            }
            Escape::Error => {
                if has_delimiter_or_newline(data, self.delimiter) {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "column contains delimiter or newline",
                    )
                    .into());
                }
                out.extend_from_slice(data);
                Ok(())
            }
            Escape::Replace(replacement) => {
                write_replaced_or_deleted(data, out, self.delimiter, Some(replacement));
                Ok(())
            }
            Escape::Delete => {
                write_replaced_or_deleted(data, out, self.delimiter, None);
                Ok(())
            }
            Escape::Backslash => {
                write_backslash_escaped(data, out, self.delimiter);
                Ok(())
            }
            Escape::QuoteDoubled(open, close) => {
                let needs_quotes = has_delimiter_or_newline(data, self.delimiter)
                    || contains_quote_byte(data, open, close);
                if needs_quotes {
                    write_quoted_doubled(data, out, open, close);
                } else {
                    out.extend_from_slice(data);
                }
                Ok(())
            }
            Escape::AlwaysQuoteDoubled(open, close) => {
                write_quoted_doubled(data, out, open, close);
                Ok(())
            }
            Escape::QuoteBackslash(open, close) => {
                let needs_quotes = has_delimiter_or_newline(data, self.delimiter)
                    || contains_quote_or_backslash(data, open, close);
                if needs_quotes {
                    write_quoted_backslash(data, out, open, close);
                } else {
                    out.extend_from_slice(data);
                }
                Ok(())
            }
            Escape::AlwaysQuoteBackslash(open, close) => {
                write_quoted_backslash(data, out, open, close);
                Ok(())
            }
        }
    }
}

impl FromStr for Spec {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_spec(s)
    }
}

/// Normalized escape-kind selector used while parsing textual config specs.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EscapeKind {
    QuoteDoubled,
    AlwaysQuoteDoubled,
    QuoteBackslash,
    AlwaysQuoteBackslash,
    Backslash,
    Error,
    Trust,
    Replace,
    Delete,
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

/// Canonicalize one output-config override key.
fn canonical_output_key(normalized: &str) -> Option<&'static str> {
    match normalized {
        "d" | "delimiter" => Some("delimiter"),
        "e" | "escape" => Some("escape"),
        "h" | "header" => Some("header"),
        "q" | "quotes" => Some("quotes"),
        "x" | "replace" => Some("replace"),
        _ => None,
    }
}

/// Parse common single-byte tokens used by delimiter and replacement configuration.
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

/// Parse textual escape selector into a normalized escape-kind value.
fn parse_escape_kind(value: &str) -> anyhow::Result<EscapeKind> {
    match normalize_token(value).as_str() {
        "qd" | "quotedoubled" => Ok(EscapeKind::QuoteDoubled),
        "aqd" | "alwaysquotedoubled" => Ok(EscapeKind::AlwaysQuoteDoubled),
        "qb" | "quotebackslash" => Ok(EscapeKind::QuoteBackslash),
        "aqb" | "alwaysquotebackslash" => Ok(EscapeKind::AlwaysQuoteBackslash),
        "backslash" => Ok(EscapeKind::Backslash),
        "error" => Ok(EscapeKind::Error),
        "trust" => Ok(EscapeKind::Trust),
        "replace" => Ok(EscapeKind::Replace),
        "delete" => Ok(EscapeKind::Delete),
        _ => Err(anyhow::anyhow!(format!(
            "unknown escape value `{value}`; expected one of: qd|quote_doubled, aqd|always_quote_doubled, qb|quote_backslash, aqb|always_quote_backslash, backslash, error, trust, replace, delete"
        ))),
    }
}

/// Parse textual header selector into a normalized [`Header`] value.
fn parse_header_mode(value: &str) -> anyhow::Result<Header> {
    match normalize_token(value).as_str() {
        "y" | "yes" | "true" | "on" | "1" => Ok(Header::Yes),
        "n" | "no" | "false" | "off" | "0" => Ok(Header::No),
        _ => Err(anyhow::anyhow!(format!(
            "unknown header value `{value}`; expected one of: y|yes|true|on|1, n|no|false|off|0"
        ))),
    }
}

/// Parse quote-pair override for output config specs.
fn parse_output_quote_pair(value: &str) -> anyhow::Result<(u8, u8)> {
    match normalize_token(value).as_str() {
        "ang" => return Ok((b'<', b'>')),
        "brk" => return Ok((b'[', b']')),
        "dq" => return Ok((b'"', b'"')),
        "sq" => return Ok((b'\'', b'\'')),
        _ => {}
    }

    let trimmed = value.trim();
    let bytes = trimmed.as_bytes();
    match bytes {
        [single] if single.is_ascii() => Ok((*single, *single)),
        [open, close] if open.is_ascii() && close.is_ascii() => Ok((*open, *close)),
        _ => Err(anyhow::anyhow!(format!(
            "quotes value `{value}` must be one ASCII character, two ASCII characters, or one of `ang`, `brk`, `dq`, `sq`"
        ))),
    }
}

/// Return whether an escape kind has configurable quote bytes.
const fn escape_kind_uses_quotes(kind: EscapeKind) -> bool {
    matches!(
        kind,
        EscapeKind::QuoteDoubled
            | EscapeKind::AlwaysQuoteDoubled
            | EscapeKind::QuoteBackslash
            | EscapeKind::AlwaysQuoteBackslash
    )
}

/// Decompose an [`Escape`] value into parse-friendly components.
const fn escape_parts(escape: Escape) -> (EscapeKind, (u8, u8), Option<u8>) {
    match escape {
        Escape::QuoteDoubled(open, close) => (EscapeKind::QuoteDoubled, (open, close), None),
        Escape::AlwaysQuoteDoubled(open, close) => {
            (EscapeKind::AlwaysQuoteDoubled, (open, close), None)
        }
        Escape::QuoteBackslash(open, close) => (EscapeKind::QuoteBackslash, (open, close), None),
        Escape::AlwaysQuoteBackslash(open, close) => {
            (EscapeKind::AlwaysQuoteBackslash, (open, close), None)
        }
        Escape::Backslash => (EscapeKind::Backslash, (b'"', b'"'), None),
        Escape::Error => (EscapeKind::Error, (b'"', b'"'), None),
        Escape::Trust => (EscapeKind::Trust, (b'"', b'"'), None),
        Escape::Replace(byte) => (EscapeKind::Replace, (b'"', b'"'), Some(byte)),
        Escape::Delete => (EscapeKind::Delete, (b'"', b'"'), None),
    }
}

/// Backwards-compatible alias for older API users.
///
/// Prefer [`Config`].
pub type OutputConfig = Config;

impl LineWriter {
    /// Construct a new `LineWriter`.
    #[must_use]
    pub fn new(config: Config, eol: Vec<u8>, writer: Box<dyn MyWrite>) -> Self {
        Self::with_capacities(config, eol, writer, 0, 0)
    }

    /// Construct a new `LineWriter` with explicit reusable buffer capacities.
    ///
    /// `record_capacity` is the initial capacity for the whole-record buffer, and
    /// `column_capacity` is the initial capacity for the chunked-column buffer.
    #[must_use]
    pub fn with_capacities(
        config: Config,
        eol: Vec<u8>,
        writer: Box<dyn MyWrite>,
        record_capacity: usize,
        column_capacity: usize,
    ) -> Self {
        Self {
            config,
            eol,
            writer,
            record_buf: Vec::with_capacity(record_capacity),
            column_buf: Vec::with_capacity(column_capacity),
            column_open: false,
            wrote_any_column: false,
        }
    }

    /// Write the CDX header for this output format. This is optional but can be used by downstream tools to detect the output format.
    /// This must be called before writing any records, and will write " CDX" followed by the delimiter byte.
    pub fn write_cdx(&mut self) -> anyhow::Result<()> {
        let orig_len = self.record_buf.len();
        self.record_buf.extend_from_slice(b" CDX");
        self.record_buf.push(self.config.delimiter);
        let result = self.writer.write_all(&self.record_buf[orig_len..]);
        self.record_buf.truncate(orig_len);
        result?;
        Ok(())
    }

    /// Access the current output configuration.
    #[must_use]
    pub const fn config(&self) -> &Config {
        &self.config
    }

    /// Mutably access the current output configuration.
    pub const fn config_mut(&mut self) -> &mut Config {
        &mut self.config
    }

    /// Access the current record terminator bytes.
    #[must_use]
    pub fn eol(&self) -> &[u8] {
        &self.eol
    }

    /// Mutably access the record terminator bytes.
    pub const fn eol_mut(&mut self) -> &mut Vec<u8> {
        &mut self.eol
    }

    /// Mutably access the underlying writer.
    pub fn writer_mut(&mut self) -> &mut dyn io::Write {
        &mut self.writer
    }

    /// Return whether chunked-column mode is currently active.
    #[must_use]
    pub const fn is_column_open(&self) -> bool {
        self.column_open
    }

    /// Prepare to append one new column into the current in-memory record.
    ///
    /// This validates chunked-column state and writes a delimiter when needed.
    fn begin_column_write(&mut self) -> anyhow::Result<()> {
        if self.column_open {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot write column while chunked column is open",
            )
            .into());
        }

        if self.wrote_any_column {
            self.record_buf.push(self.config.delimiter);
        } else {
            self.wrote_any_column = true;
        }

        debug_assert!(!self.column_open);
        Ok(())
    }

    /// Write one full column into the current record.
    ///
    /// Error-state guarantee:
    /// on error, `record_buf` and column-separator state are restored to what
    /// they were before this call, so the caller may continue writing columns
    /// or retry.
    pub fn write_column(&mut self, data: &[u8]) -> anyhow::Result<()> {
        let original_len = self.record_buf.len();
        let original_wrote_any_column = self.wrote_any_column;
        self.begin_column_write()?;
        match self.config.write_column(data, &mut self.record_buf) {
            Ok(()) => Ok(()),
            Err(err) => {
                self.record_buf.truncate(original_len);
                self.wrote_any_column = original_wrote_any_column;
                Err(err)
            }
        }
    }

    /// Write many full columns into the current record.
    pub fn write_columns<I, B>(&mut self, columns: I) -> anyhow::Result<()>
    where
        I: IntoIterator<Item = B>,
        B: AsRef<[u8]>,
    {
        for column in columns {
            self.write_column(column.as_ref())?;
        }
        Ok(())
    }

    /// Write one column from multiple chunks.
    ///
    /// This is equivalent to:
    /// `begin_column`, repeated `write_column_chunk`, and `end_column`.
    pub fn write_column_chunks<I, B>(&mut self, chunks: I) -> anyhow::Result<()>
    where
        I: IntoIterator<Item = B>,
        B: AsRef<[u8]>,
    {
        self.begin_column()?;
        for chunk in chunks {
            self.write_column_chunk(chunk.as_ref())?;
        }
        self.end_column()
    }

    /// Write a complete record (columns plus record terminator).
    pub fn write_record<I, B>(&mut self, columns: I) -> anyhow::Result<()>
    where
        I: IntoIterator<Item = B>,
        B: AsRef<[u8]>,
    {
        self.write_columns(columns)?;
        self.finish_record()
    }

    /// Begin chunked-column mode for one column value.
    pub fn begin_column(&mut self) -> anyhow::Result<()> {
        if self.column_open {
            return Err(
                io::Error::new(io::ErrorKind::InvalidInput, "chunked column already open").into()
            );
        }
        self.column_buf.clear();
        self.column_open = true;
        Ok(())
    }

    /// Append bytes to the currently open chunked-column value.
    pub fn write_column_chunk(&mut self, data: &[u8]) -> anyhow::Result<()> {
        if !self.column_open {
            return Err(
                io::Error::new(io::ErrorKind::InvalidInput, "no open chunked column").into()
            );
        }
        self.column_buf.extend_from_slice(data);
        Ok(())
    }

    /// Finish the currently open chunked-column value and write it as one column.
    pub fn end_column(&mut self) -> anyhow::Result<()> {
        if !self.column_open {
            return Err(
                io::Error::new(io::ErrorKind::InvalidInput, "no open chunked column").into()
            );
        }

        self.column_open = false;
        let data = std::mem::take(&mut self.column_buf);
        let result = self.write_column(&data);
        self.column_buf = data;
        result
    }

    /// Discard the current in-progress record without writing anything.
    ///
    /// This also closes and clears any in-progress chunked column.
    pub fn abort_record(&mut self) {
        self.record_buf.clear();
        self.column_buf.clear();
        self.column_open = false;
        self.wrote_any_column = false;
    }

    /// Finish the current record by writing `self.eol` and resetting record state.
    ///
    /// Error-state guarantee:
    /// on write error, the encoded columns remain in `record_buf` and `wrote_any_column`
    /// is unchanged, so the caller may retry `finish_record`.
    pub fn finish_record(&mut self) -> anyhow::Result<()> {
        if self.column_open {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot finish record while chunked column is open",
            )
            .into());
        }
        debug_assert!(!self.column_open);

        let orig_len = self.record_buf.len();
        self.record_buf.extend_from_slice(&self.eol);
        let result = self.writer.write_all(&self.record_buf);
        if result.is_ok() {
            self.record_buf.clear();
            self.wrote_any_column = false;
        } else {
            self.record_buf.truncate(orig_len);
        }
        result?;
        Ok(())
    }

    /// Finish an empty record.
    ///
    /// This writes only the configured record terminator for the current line.
    pub fn finish_empty_record(&mut self) -> anyhow::Result<()> {
        self.finish_record()
    }

    /// Flush the underlying writer.
    pub fn flush(&mut self) -> anyhow::Result<()> {
        self.writer.flush()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    /// Encode a single column and return resulting bytes.
    fn output(config: OutputConfig, data: &[u8]) -> anyhow::Result<Vec<u8>> {
        let mut out = Vec::new();
        config.write_column(data, &mut out)?;
        Ok(out)
    }

    /// In-memory writer used for output-content assertions.
    #[derive(Debug, Clone, Default)]
    struct SharedVecWriter {
        data: Arc<Mutex<Vec<u8>>>,
    }

    impl SharedVecWriter {
        fn snapshot(&self) -> Vec<u8> {
            self.data.lock().unwrap().clone()
        }
    }

    impl io::Write for SharedVecWriter {
        fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
            self.data.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> Result<(), io::Error> {
            Ok(())
        }
    }

    /// In-memory writer that tracks number of write calls.
    #[derive(Debug, Clone, Default)]
    struct CountingCallsWriter {
        data: Arc<Mutex<Vec<u8>>>,
        calls: Arc<AtomicUsize>,
    }

    impl CountingCallsWriter {
        fn snapshot(&self) -> Vec<u8> {
            self.data.lock().unwrap().clone()
        }

        fn calls(&self) -> usize {
            self.calls.load(Ordering::Relaxed)
        }
    }

    impl io::Write for CountingCallsWriter {
        fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
            self.calls.fetch_add(1, Ordering::Relaxed);
            self.data.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> Result<(), io::Error> {
            Ok(())
        }
    }

    /// In-memory writer that fails exactly once, then succeeds.
    #[derive(Debug, Clone, Default)]
    struct FailFirstWriteThenSucceedWriter {
        data: Arc<Mutex<Vec<u8>>>,
        failed_once: Arc<AtomicBool>,
    }

    impl FailFirstWriteThenSucceedWriter {
        fn snapshot(&self) -> Vec<u8> {
            self.data.lock().unwrap().clone()
        }
    }

    impl io::Write for FailFirstWriteThenSucceedWriter {
        fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
            if !self.failed_once.swap(true, Ordering::Relaxed) {
                return Err(io::Error::other("forced write failure"));
            }
            self.data.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> Result<(), io::Error> {
            Ok(())
        }
    }

    #[test]
    fn write_column_trust_writes_raw() {
        let config = OutputConfig::with_header(b',', Escape::Trust, Header::No);
        assert_eq!(output(config, b"a,b\nc").unwrap(), b"a,b\nc");
    }

    #[test]
    fn write_column_error_rejects_delimiter_or_newline() {
        let config = OutputConfig::with_header(b',', Escape::Error, Header::No);
        assert!(output(config, b"a,b").is_err());
        assert!(output(config, b"a\nb").is_err());
        assert!(output(config, b"a\rb").is_err());
        assert_eq!(output(config, b"abc").unwrap(), b"abc");
    }

    #[test]
    fn write_column_replace_substitutes_delimiter_and_newlines() {
        let config = OutputConfig::with_header(b',', Escape::Replace(b'?'), Header::No);
        assert_eq!(output(config, b"a,b\nc\rd").unwrap(), b"a?b?c?d");
    }

    #[test]
    fn write_column_delete_removes_delimiter_and_newlines() {
        let config = OutputConfig::with_header(b',', Escape::Delete, Header::No);
        assert_eq!(output(config, b"a,b\nc\rd").unwrap(), b"abcd");
    }

    #[test]
    fn write_column_backslash_escapes_special_bytes() {
        let config = OutputConfig::with_header(b',', Escape::Backslash, Header::No);
        assert_eq!(output(config, b"a,b\nc\rd\\e").unwrap(), b"a\\,b\\nc\\rd\\\\e");
    }

    #[test]
    fn write_column_quote_doubled_quotes_when_needed() {
        let config = OutputConfig::with_header(b',', Escape::QuoteDoubled(b'"', b'"'), Header::No);
        assert_eq!(output(config, b"a,b").unwrap(), b"\"a,b\"");
        assert_eq!(output(config, b"ab").unwrap(), b"ab");
        assert_eq!(output(config, b"a\"b").unwrap(), b"\"a\"\"b\"");
    }

    #[test]
    fn write_column_always_quote_doubled_always_quotes() {
        let config =
            OutputConfig::with_header(b',', Escape::AlwaysQuoteDoubled(b'"', b'"'), Header::No);
        assert_eq!(output(config, b"ab").unwrap(), b"\"ab\"");
        assert_eq!(output(config, b"a\"b").unwrap(), b"\"a\"\"b\"");
    }

    #[test]
    fn write_column_quote_doubled_asymmetric_only_doubles_close_quote() {
        let config = OutputConfig::with_header(b',', Escape::QuoteDoubled(b'<', b'>'), Header::No);
        assert_eq!(output(config, b"a<b").unwrap(), b"a<b");
        assert_eq!(output(config, b"a>b<c").unwrap(), b"<a>>b<c>");
    }

    #[test]
    fn write_column_always_quote_doubled_asymmetric_only_doubles_close_quote() {
        let config =
            OutputConfig::with_header(b',', Escape::AlwaysQuoteDoubled(b'<', b'>'), Header::No);
        assert_eq!(output(config, b"a<b").unwrap(), b"<a<b>");
        assert_eq!(output(config, b"a>b<c").unwrap(), b"<a>>b<c>");
    }

    #[test]
    fn write_column_quote_backslash_quotes_when_needed() {
        let config =
            OutputConfig::with_header(b',', Escape::QuoteBackslash(b'"', b'"'), Header::No);
        assert_eq!(output(config, b"a,b").unwrap(), b"\"a,b\"");
        assert_eq!(output(config, b"ab").unwrap(), b"ab");
        assert_eq!(output(config, b"a\"b\\c").unwrap(), b"\"a\\\"b\\\\c\"");
    }

    #[test]
    fn write_column_quote_backslash_asymmetric_escapes_close_and_backslash_only() {
        let config =
            OutputConfig::with_header(b',', Escape::QuoteBackslash(b'<', b'>'), Header::No);
        assert_eq!(output(config, b"a<b").unwrap(), b"a<b");
        assert_eq!(output(config, b"a>b\\c").unwrap(), b"<a\\>b\\\\c>");
    }

    #[test]
    fn write_column_always_quote_backslash_always_quotes() {
        let config =
            OutputConfig::with_header(b',', Escape::AlwaysQuoteBackslash(b'"', b'"'), Header::No);
        assert_eq!(output(config, b"ab").unwrap(), b"\"ab\"");
        assert_eq!(output(config, b"a\"b\\c").unwrap(), b"\"a\\\"b\\\\c\"");
    }

    #[test]
    fn output_config_safe_constructors() {
        assert_eq!(
            OutputConfig::tsv(),
            OutputConfig { delimiter: b'\t', escape: Escape::Backslash, header: Header::Yes }
        );
        assert_eq!(
            OutputConfig::csv(),
            OutputConfig {
                delimiter: b',',
                escape: Escape::QuoteDoubled(b'"', b'"'),
                header: Header::Yes,
            }
        );
    }

    /// Verifies output spec parser supports short aliases and quote-pair aliases.
    #[test]
    fn output_spec_from_spec_parses_aliases() {
        let parsed = Spec::from_spec("csv,d=t,e=aqb,q=ang").unwrap();
        assert_eq!(
            parsed,
            Spec {
                delimiter: Some(b'\t'),
                escape: Some(Escape::AlwaysQuoteBackslash(b'<', b'>')),
                header: Some(Header::Yes),
            }
        );
    }

    /// Verifies output spec parser supports one- and two-byte quote values.
    #[test]
    fn output_spec_from_spec_quote_values() {
        let one = Spec::from_spec("csv,e=qd,q='").unwrap();
        assert_eq!(one.escape, Some(Escape::QuoteDoubled(b'\'', b'\'')));
        assert_eq!(one.header, Some(Header::Yes));

        let two = Spec::from_spec("csv,e=qb,q=[]").unwrap();
        assert_eq!(two.escape, Some(Escape::QuoteBackslash(b'[', b']')));
        assert_eq!(two.header, Some(Header::Yes));
    }

    /// Verifies output spec parser supports header overrides.
    #[test]
    fn output_spec_from_spec_header_values() {
        let yes = Spec::from_spec("csv,h=yes").unwrap();
        assert_eq!(yes.header, Some(Header::Yes));

        let no = Spec::from_spec("csv,header=off").unwrap();
        assert_eq!(no.header, Some(Header::No));
    }

    /// Verifies output spec parser requires replace byte when `escape=replace`.
    #[test]
    fn output_spec_from_spec_replace_requires_value() {
        let err = Spec::from_spec("csv,e=replace").unwrap_err();
        assert!(err.to_string().contains("requires"));

        let parsed = Spec::from_spec("csv,e=replace,x=s").unwrap();
        assert_eq!(parsed.escape, Some(Escape::Replace(b' ')));
    }

    /// Verifies output spec parser rejects quote overrides for non-quote escapes.
    #[test]
    fn output_spec_from_spec_rejects_quotes_for_non_quote_escape() {
        let err = Spec::from_spec("csv,e=delete,q=brk").unwrap_err();
        assert!(err.to_string().contains("quote-based"));
    }

    /// Verifies `FromStr` delegates to output spec parsing.
    #[test]
    fn output_spec_from_str_works() {
        let parsed: Spec = "tsv,e=trust,d=s".parse().unwrap();
        assert_eq!(parsed.delimiter, Some(b' '));
        assert_eq!(parsed.escape, Some(Escape::Trust));
    }

    /// Verifies unknown output spec base errors list expected base names.
    #[test]
    fn output_spec_from_spec_unknown_base_lists_expected_values() {
        let err = Spec::from_spec("nope").unwrap_err();
        let text = err.to_string();
        assert!(text.contains("expected one of"));
        assert!(text.contains("csv"));
        assert!(text.contains("tsv"));
    }

    /// Verifies spec parser allows omitted base when first segment is `key=value`.
    #[test]
    fn output_spec_from_spec_omitted_base_allows_key_value_start() {
        let parsed = Spec::from_spec("d=t,e=backslash").unwrap();
        assert_eq!(parsed.delimiter, Some(b'\t'));
        assert_eq!(parsed.escape, Some(Escape::Backslash));
        assert_eq!(parsed.header, None);
    }

    /// Verifies empty output spec parses as unconstrained (`Spec::any`).
    #[test]
    fn output_spec_from_spec_empty_is_any() {
        assert_eq!(Spec::from_spec("").unwrap(), Spec::any());
        assert_eq!(Spec::from_spec("   ").unwrap(), Spec::any());
    }

    /// Verifies `q=` is rejected without a quote-based base/escape selection.
    #[test]
    fn output_spec_from_spec_quotes_without_escape_is_error() {
        let err = Spec::from_spec("q=dq").unwrap_err();
        assert!(err.to_string().contains("quote-based"));
    }

    /// Verifies `x=` is rejected without `escape=replace` via base or override.
    #[test]
    fn output_spec_from_spec_replace_without_escape_is_error() {
        let err = Spec::from_spec("x=s").unwrap_err();
        assert!(err.to_string().contains("escape=replace"));
    }

    /// Verifies empty spec leaves all output fields unresolved for input-driven derivation.
    #[test]
    fn output_spec_new_is_all_none() {
        assert_eq!(Spec::new(), Spec { delimiter: None, escape: None, header: None });
    }

    /// Verifies `Spec::any` is an alias of `Spec::new`.
    #[test]
    fn output_spec_any_matches_new() {
        assert_eq!(Spec::any(), Spec::new());
    }

    /// Verifies `Spec::csv` and `Spec::tsv` mirror the corresponding output config defaults.
    #[test]
    fn output_spec_csv_tsv_match_config_defaults() {
        assert_eq!(
            Spec::csv(),
            Spec {
                delimiter: Some(Config::csv().delimiter),
                escape: Some(Config::csv().escape),
                header: Some(Config::csv().header),
            }
        );
        assert_eq!(
            Spec::tsv(),
            Spec {
                delimiter: Some(Config::tsv().delimiter),
                escape: Some(Config::tsv().escape),
                header: Some(Config::tsv().header),
            }
        );
    }

    /// Verifies input-driven derivation for standard CSV options.
    #[test]
    fn output_config_from_input_and_spec_csv_defaults() {
        let input = input_file::Config::csv();
        let output = Config::from_input_and_spec(&input, &Spec::new());

        assert_eq!(output.delimiter, b',');
        assert_eq!(output.escape, Escape::QuoteDoubled(b'"', b'"'));
        assert_eq!(output.header, Header::Yes);
    }

    /// Verifies non-char delimiters fall back to tab and off+none maps to replacement escaping.
    #[test]
    fn output_config_from_input_and_spec_non_char_delimiter_fallback() {
        let input = input_file::Config::whole();
        let output = Config::from_input_and_spec(&input, &Spec::new());

        assert_eq!(output.delimiter, b'\t');
        assert_eq!(output.escape, Escape::Replace(b' '));
        assert_eq!(output.header, Header::No); // whole-file input has no header
    }

    /// Verifies off+none uses dot replacement when the output delimiter is space.
    #[test]
    fn output_config_from_input_and_spec_replace_dot_for_space_delimiter() {
        let input = input_file::Config::from_delim(input::Delimiter::Char(b','));
        let spec = Spec { delimiter: Some(b' '), escape: None, header: None };
        let output = Config::from_input_and_spec(&input, &spec);
        assert_eq!(output.escape, Escape::Replace(b'.'));
    }

    /// Verifies `Quotes::Multi` uses its first quote pair for output-escape derivation.
    #[test]
    fn output_config_from_input_and_spec_multi_quotes_uses_first_pair() {
        let mut input = input_file::Config::from_delim(input::Delimiter::Char(b','));
        input.column_config.backslash = BackslashMode::On;
        input.column_config.quotes = Quotes::Multi(vec![(b'<', b'>'), (b'[', b']')]);

        let output = Config::from_input_and_spec(&input, &Spec::new());
        assert_eq!(output.escape, Escape::QuoteBackslash(b'<', b'>'));
    }

    /// Verifies observed header state takes precedence over configured input header.
    #[test]
    fn output_config_from_input_and_spec_header_precedence() {
        let mut input = input_file::Config::csv();
        input.header = input_file::Header::No;
        input.saw_header = Some(input_file::SawHeader::Cdx);

        let output = Config::from_input_and_spec(&input, &Spec::new());
        assert_eq!(output.header, Header::Yes);

        input.saw_header = Some(input_file::SawHeader::No);
        let output = Config::from_input_and_spec(&input, &Spec::new());
        assert_eq!(output.header, Header::No);
    }

    /// Verifies explicit spec values override all input-derived output values.
    #[test]
    fn output_config_from_input_and_spec_overrides() {
        let input = input_file::Config::csv();
        let spec =
            Spec { delimiter: Some(b'|'), escape: Some(Escape::Delete), header: Some(Header::No) };

        let output = Config::from_input_and_spec(&input, &spec);
        assert_eq!(output, Config { delimiter: b'|', escape: Escape::Delete, header: Header::No });
    }

    #[test]
    fn line_writer_write_record_writes_columns_and_eol() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b',', Escape::QuoteDoubled(b'"', b'"'), Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_record([&b"a,b"[..], &b"c"[..]]).unwrap();
        line_writer.write_record([&b"x"[..], &b"y"[..]]).unwrap();

        assert_eq!(sink_view.snapshot(), b"\"a,b\",c\nx,y\n");
    }

    #[test]
    fn line_writer_with_capacities_uses_requested_capacity() {
        let sink = SharedVecWriter::default();
        let config = OutputConfig::with_header(b',', Escape::Trust, Header::No);
        let line_writer =
            LineWriter::with_capacities(config, b"\n".to_vec(), Box::new(sink), 64, 32);

        assert!(line_writer.record_buf.capacity() >= 64);
        assert!(line_writer.column_buf.capacity() >= 32);
    }

    #[test]
    fn line_writer_is_column_open_tracks_state() {
        let sink = SharedVecWriter::default();
        let config = OutputConfig::new(b',', Escape::Trust);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        assert!(!line_writer.is_column_open());
        line_writer.begin_column().unwrap();
        assert!(line_writer.is_column_open());
        line_writer.write_column_chunk(b"a").unwrap();
        line_writer.end_column().unwrap();
        assert!(!line_writer.is_column_open());
    }

    #[test]
    fn line_writer_abort_record_discards_pending_data() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::new(b',', Escape::Trust);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"left").unwrap();
        line_writer.begin_column().unwrap();
        line_writer.write_column_chunk(b"right").unwrap();
        line_writer.abort_record();

        assert!(!line_writer.is_column_open());
        line_writer.write_column(b"kept").unwrap();
        line_writer.finish_record().unwrap();
        assert_eq!(sink_view.snapshot(), b"kept\n");
    }

    #[test]
    fn line_writer_write_columns_then_finish_record() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b'\t', Escape::Trust, Header::No);
        let mut line_writer = LineWriter::new(config, b"\r\n".to_vec(), Box::new(sink));

        line_writer.write_columns([&b"aa"[..], &b"bb"[..], &b"cc"[..]]).unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"aa\tbb\tcc\r\n");
    }

    #[test]
    fn line_writer_write_column_writes_tab_delimiter_and_lf_eol() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b'\t', Escape::Trust, Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"foo").unwrap();
        line_writer.write_column(b"bar").unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"foo\tbar\n");
    }

    #[test]
    fn line_writer_chunked_column_mode() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config =
            OutputConfig::with_header(b',', Escape::QuoteBackslash(b'"', b'"'), Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"left").unwrap();
        line_writer.begin_column().unwrap();
        line_writer.write_column_chunk(b"a\"").unwrap();
        line_writer.write_column_chunk(b"b\\c").unwrap();
        line_writer.end_column().unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"left,\"a\\\"b\\\\c\"\n");
    }

    #[test]
    fn line_writer_write_column_chunks_convenience_api() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config =
            OutputConfig::with_header(b',', Escape::QuoteBackslash(b'"', b'"'), Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"left").unwrap();
        line_writer.write_column_chunks([&b"a\""[..], &b"b\\c"[..]]).unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"left,\"a\\\"b\\\\c\"\n");
    }

    #[test]
    fn line_writer_chunked_column_chunked_and_bulk_api() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config =
            OutputConfig::with_header(b',', Escape::QuoteBackslash(b'"', b'"'), Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"left").unwrap();
        line_writer.begin_column().unwrap();
        line_writer.write_column_chunk(b"a\"").unwrap();
        line_writer.write_column_chunk(b"b\\c").unwrap();
        line_writer.end_column().unwrap();
        line_writer.write_column_chunks([&b"x"[..], &b"y"[..]]).unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"left,\"a\\\"b\\\\c\",xy\n");
    }

    #[test]
    fn line_writer_finish_record_uses_single_write_call() {
        let sink = CountingCallsWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b'\t', Escape::Trust, Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"foo").unwrap();
        line_writer.write_column(b"bar").unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"foo\tbar\n");
        assert_eq!(sink_view.calls(), 1);
    }

    #[test]
    fn line_writer_write_column_error_rolls_back_state() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b',', Escape::Error, Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"left").unwrap();
        assert!(line_writer.write_column(b"bad,value").is_err());
        line_writer.write_column(b"right").unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"left,right\n");
    }

    #[test]
    fn line_writer_finish_record_error_keeps_record_retryable() {
        let sink = FailFirstWriteThenSucceedWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::with_header(b'\t', Escape::Trust, Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        line_writer.write_column(b"foo").unwrap();
        assert!(line_writer.finish_record().is_err());
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"foo\n");
    }

    #[test]
    fn line_writer_finish_empty_record_writes_only_eol() {
        let sink = SharedVecWriter::default();
        let sink_view = sink.clone();
        let config = OutputConfig::new(b',', Escape::Trust);
        let mut line_writer = LineWriter::new(config, b"\r\n".to_vec(), Box::new(sink));

        line_writer.finish_empty_record().unwrap();
        line_writer.write_column(b"a").unwrap();
        line_writer.finish_record().unwrap();

        assert_eq!(sink_view.snapshot(), b"\r\na\r\n");
    }

    #[test]
    fn line_writer_chunked_state_errors() {
        let sink = SharedVecWriter::default();
        let config = OutputConfig::with_header(b',', Escape::Trust, Header::No);
        let mut line_writer = LineWriter::new(config, b"\n".to_vec(), Box::new(sink));

        assert!(line_writer.write_column_chunk(b"x").is_err());
        assert!(line_writer.end_column().is_err());

        line_writer.begin_column().unwrap();
        assert!(line_writer.begin_column().is_err());
        assert!(line_writer.write_column(b"x").is_err());
        assert!(line_writer.finish_record().is_err());
        line_writer.end_column().unwrap();
        line_writer.finish_record().unwrap();
    }
}
