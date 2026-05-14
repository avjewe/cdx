//! text file reading.

use crate::prelude::*;
use crate::*;
use input::Delimiter;
use input::normalize_token;
use std::collections::HashSet;

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

/// Complete parser configuration
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Config {
    /// Column config.
    pub column_config: read_line::Config,
    /// Whether input is expected to include a header record.
    pub header: Header,
    /// How CDX-style headers are handled.
    pub cdx: CdxMode,
    /// What header state has been observed so far.
    pub saw_header: Option<SawHeader>,
}

/// Canonicalize one input-config override key.
pub(crate) fn canonical_input_key(normalized: &str) -> Result<&'static str> {
    match normalized {
        "d" | "delimiter" => Ok("delimiter"),
        "q" | "quotes" => Ok("quotes"),
        "b" | "backslash" => Ok("backslash"),
        "u" | "unterminated" => Ok("unterminated"),
        "h" | "header" => Ok("header"),
        "c" | "cdx" => Ok("cdx"),
        _ => anyhow::bail!(format!(
            "unknown input config key `{normalized}`; expected one of: d|delimiter, q|quotes, b|backslash, u|unterminated, h|header, x|cdx"
        )),
    }
}

/// Parse header-mode override value for input config specs.
fn parse_header_mode(value: &str) -> Result<Header> {
    match normalize_token(value).as_str() {
        "y" | "yes" | "true" | "on" | "1" => Ok(Header::Yes),
        "n" | "no" | "false" | "off" | "0" => Ok(Header::No),
        _ => Err(anyhow::anyhow!(format!(
            "unknown header value `{value}`; expected one of: y|yes|true|on|1, n|no|false|off|0"
        ))),
    }
}

/// Parse CDX-mode override value for input config specs.
fn parse_cdx_mode(value: &str) -> Result<CdxMode> {
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

impl Config {
    /// new Config with the given column config and expecting a header.
    #[must_use]
    pub const fn from_inner(c: read_line::Config) -> Self {
        Self { column_config: c, header: Header::Yes, cdx: CdxMode::Optional, saw_header: None }
    }

    /// new Config with the given column config and not expecting a header.
    #[must_use]
    pub const fn from_inner_no_header(c: read_line::Config) -> Self {
        Self { column_config: c, header: Header::No, cdx: CdxMode::Optional, saw_header: None }
    }

    /// new Config as tsv but with the given delimiter.
    #[must_use]
    pub const fn from_delim(delimiter: Delimiter) -> Self {
        Self::from_inner(read_line::Config::new(delimiter))
    }

    /// new Config with the inner column config denoted by the string, e.g. "tsv".
    pub fn from_base(c: &str) -> Result<Self> {
        Ok(Self::from_inner(read_line::Config::from_base(c)?))
    }
    /// csv file with header
    #[must_use]
    pub const fn csv() -> Self {
        Self::from_inner(read_line::Config::csv())
    }
    /// csv file without header
    #[must_use]
    pub const fn csv_n() -> Self {
        Self::from_inner_no_header(read_line::Config::csv())
    }
    /// tsv file with header
    #[must_use]
    pub const fn tsv() -> Self {
        Self::from_inner(read_line::Config::tsv())
    }
    /// tsv file without header
    #[must_use]
    pub const fn tsv_n() -> Self {
        Self::from_inner_no_header(read_line::Config::tsv())
    }
    /// clf (log) file with header
    #[must_use]
    pub fn clf() -> Self {
        Self::from_inner(read_line::Config::clf())
    }
    /// clf (log) file without header
    #[must_use]
    pub fn clf_n() -> Self {
        Self::from_inner_no_header(read_line::Config::clf())
    }
    /// file where every line is a single column, with no header
    #[must_use]
    pub const fn whole() -> Self {
        Self::from_inner_no_header(read_line::Config::whole())
    }

    pub(crate) fn add_spec(&mut self, key: &str, value: &str) -> Result<()> {
        match key {
            "delimiter" => {
                self.header = parse_header_mode(value)?;
                Ok(())
            }
            "quotes" => {
                self.cdx = parse_cdx_mode(value)?;
                Ok(())
            }
            _ => Err(anyhow!(
                "Impossible. Unrecognized key `{key}` should have been rejected by caller."
            )),
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
            let key = canonical_input_key(&key_norm)?;
            if !seen_keys.insert(key) {
                return Err(anyhow!(format!("duplicate input config key `{key_raw}`")));
            }
            if !config.column_config.add_spec(key, value_raw)? {
                config.add_spec(key, value_raw)?;
            }
        }

        Ok(config)
    }
}

/// A line of text input, including the original line and the parsed column values.
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct TextLine {
    /// The whole input line, without newline, bytes untouched
    pub line: Vec<u8>,
    /// column values, decoded and unquoted
    pub values: input::Columns,
    /// The EOL observed at the end of the line.
    pub eol: read_line::ReadResult,
}

impl std::ops::Index<usize> for TextLine {
    type Output = [u8];
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
    }
}

impl TextLine {
    /// Write the original line to the given writer, including the EOL.
    pub fn write(&self, w: &mut impl Write) -> Result<()> {
        w.write_all(&self.line)?;
        w.write_all(self.eol.as_bytes())?;
        Ok(())
    }
    /// Get one column. Return an empty column if index is too big.
    #[must_use]
    pub fn get(&self, index: usize) -> &[u8] {
        if index >= self.values.columns.len() { &[] } else { &self.values.columns[index] }
    }
    /// Clear the line and columns.
    pub fn clear(&mut self) {
        self.line.clear();
        self.values.columns.clear();
        self.eol = read_line::ReadResult::Lf;
    }
    /// Get a reference to the columns.
    #[must_use]
    pub const fn columns(&self) -> &RVec<Vec<u8>> {
        &self.values.columns
    }
    /// Get a reference to the original line.
    #[must_use]
    pub fn line(&self) -> &[u8] {
        &self.line
    }
    /// Split the line into columns according to the given config.
    pub fn split(&mut self, options: &input::Config) {
        self.values.read(&self.line, options);
    }
    /// Split the line into columns according to the given delimiter.
    pub fn split_plain(&mut self, delim: u8) {
        self.values.read_plain(&self.line, delim);
    }
}

/// A text file being read, including the reader, config, column names, and current line values.
#[derive(Debug)]
pub struct TextFile {
    /// what to read
    reader: util::Infile,
    /// how to interpret
    options: Config,
    /// The names of the columns. c1, c1... are auto generated if needed
    column_names: Vec<String>,
    /// The column values
    column_values: TextLine,
    /// Have we hit EOF?
    done: bool,
    /// Do we bother splitting into columns?
    do_split: bool,
    /// Where are we in the file? (line number, byte offset, etc.)
    loc: util::FileLocData,
}

/// A `TextFile` along with the previous line, for operations that need to compare adjacent lines.
#[derive(Debug)]
pub struct TextFilePrev(
    /// The current text file.
    pub TextFile,
    /// The previous line's column values, for comparison with the current line.
    pub TextLine,
);

impl TextFilePrev {
    /// Create a new `TextFilePrev` with the given reader and options.
    pub fn new(file_name: &str, options: Config) -> Result<Self> {
        Ok(Self(TextFile::new(file_name, options)?, TextLine::default()))
    }
    /// Read the first line of the file and initialize the previous line to be empty.
    pub fn read_first_line(&mut self) -> anyhow::Result<()> {
        self.0.read_first_line()
    }
    /// Get the next line of the file, updating the previous line and current line values.
    pub fn get_line(&mut self) -> Result<bool> {
        std::mem::swap(&mut self.1, &mut self.0.column_values);
        self.0.get_line()
    }
}
impl TextFile {
    /// Create a new `TextFile` with the given reader and options.
    pub fn new(file_name: &str, options: Config) -> Result<Self> {
        let mut me = Self {
            reader: util::get_reader(file_name)?,
            options,
            column_names: Vec::new(),
            column_values: TextLine::default(),
            done: false,
            do_split: true,
            loc: util::FileLocData::default(),
        };
        me.read_first_line()?;
        Ok(me)
    }
    /// Get the column names.
    #[must_use]
    pub const fn is_done(&self) -> bool {
        self.done
    }
    /// Get the column names.
    #[must_use]
    pub fn names(&self) -> &[String] {
        &self.column_names
    }
    /// Get the column values.
    #[must_use]
    pub const fn values(&self) -> &TextLine {
        &self.column_values
    }
    /// open file for reading
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.reader = util::get_reader(name)?;
        self.read_first_line()
    }

    fn copy_column_names(&mut self) -> Result<()> {
        self.column_names.clear();
        for v in self.column_values.values.as_ref() {
            self.column_names.push(String::from_utf8(v.clone())?);
        }
        // FIXME - validate column names
        Ok(())
    }

    fn read_line(&mut self) -> Result<()> {
        self.column_values.eol = read_line::read(
            &mut self.column_values.line,
            &mut self.reader.0,
            &self.options.column_config,
            &mut self.loc,
        )?;
        Ok(())
    }
    fn split(&mut self) {
        // self.column_values.values.read(&self.column_values.line, &self.options.column_config);
        self.column_values.split(&self.options.column_config);
    }
    /// Read the next line of the file, returning true if EOF is reached.
    pub fn get_line(&mut self) -> Result<bool> {
        self.read_line()?;
        if self.column_values.eol == read_line::ReadResult::Eof {
            self.done = true;
            return Ok(true);
        }
        if self.do_split {
            self.split();
        }
        Ok(false)
    }

    /// Generate default column names (`c1`, `c2`, ...) matching current values.
    fn generate_column_names(count: usize) -> Vec<String> {
        let mut names = Vec::with_capacity(count);
        for index in 0..count {
            names.push(format!("c{}", index + 1));
        }
        names
    }

    /// Read column names and the first data record according to header and CDX settings.
    pub fn read_first_line(&mut self) -> anyhow::Result<()> {
        if self.options.saw_header.is_some() {
            return Err(anyhow::anyhow!(
                "read_first_line cannot be called after header state is set",
            ));
        }

        self.read_line()?;
        if self.column_values.eol == read_line::ReadResult::Eof {
            self.options.saw_header = Some(SawHeader::Empty);
            self.done = true;
            return Ok(());
        }

        let has_cdx = self.column_values.line.starts_with(b" CDX");
        match (self.options.cdx, has_cdx) {
            (CdxMode::Forbidden, true) => {
                return Err(anyhow::anyhow!("CDX header is forbidden"));
            }
            (CdxMode::Required, false) => {
                return Err(anyhow::anyhow!("CDX header is required"));
            }
            (CdxMode::Optional | CdxMode::Required, true) => {
                let delimiter = *self
                    .column_values
                    .line
                    .get(4)
                    .ok_or_else(|| anyhow::anyhow!("CDX header is missing delimiter byte"))?;
                self.options.column_config.delimiter = Delimiter::Char(delimiter);
                self.column_values
                    .values
                    .read(&self.column_values.line[5..], &self.options.column_config);
                self.options.saw_header = Some(SawHeader::Cdx);
                self.copy_column_names()?;
                self.get_line()?;
            }
            (CdxMode::Optional | CdxMode::Forbidden | CdxMode::Ignore, false)
            | (CdxMode::Ignore, true) => match self.options.header {
                Header::Yes => {
                    self.column_values
                        .values
                        .read(&self.column_values.line, &self.options.column_config);
                    self.options.saw_header = Some(SawHeader::Yes);
                    self.copy_column_names()?;
                    self.get_line()?;
                }
                Header::No => {
                    self.column_values
                        .values
                        .read(&self.column_values.line, &self.options.column_config);
                    self.column_names =
                        Self::generate_column_names(self.column_values.values.columns.len());
                    self.options.saw_header = Some(SawHeader::No);
                    eprintln!("AAA");
                    if self.do_split {
                        eprintln!("BBB");
                        self.split();
                    }
                }
            },
        }

        eprintln!("CCC");
        Ok(())
    }
}
