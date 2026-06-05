//! stuff

use crate::prelude::*;
use crate::*;
pub use input::Delimiter;
use std::io;
use util::FileLocData;

/// Whether backslash escape handling is enabled while parsing.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum BackslashMode {
    /// Backslash has no special meaning.
    #[default]
    Off,
    /// Backslash causes the following byte to be ignored for parser-state changes.
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

/// Complete parser configuration used by [`read`].
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Config {
    /// Column delimiter mode. Only use for column parsing, not line parsing.
    pub delimiter: Delimiter,
    /// Quote handling mode.
    pub quotes: Quotes,
    /// Backslash escape handling mode.
    pub backslash: BackslashMode,
    /// Unterminated quote behavior.
    pub unterminated_quote: UnterminatedQuoteMode,
}

impl Config {
    /// Get the byte that represents this delimiter, if applicable.
    #[must_use]
    pub const fn delim(&self) -> u8 {
        self.delimiter.delim()
    }
}

/// The outcome of a call to [`read`].
#[must_use]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum ReadResult {
    /// A record ending in `\n` was read.
    #[default]
    Lf,
    /// A record ending in `\r` was read.
    Cr,
    /// A record ending in `\r\n` was read.
    CrLf,
    /// A record was read, but EOF was reached before a terminator.
    NoTerminator,
    /// EOF was reached before any record bytes were read.
    Eof,
}

impl ReadResult {
    /// Returns the EOL as a string slice.
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Lf => "\n",
            Self::Cr => "\r",
            Self::CrLf => "\r\n",
            Self::NoTerminator => "",
            Self::Eof => "",
        }
    }
    /// Returns the EOL as a byte slice.
    #[must_use]
    pub const fn as_bytes(&self) -> &'static [u8] {
        match self {
            Self::Lf => b"\n",
            Self::Cr => b"\r",
            Self::CrLf => b"\r\n",
            Self::NoTerminator => b"",
            Self::Eof => b"",
        }
    }
}

/// Reads a line of input from `reader` into `line`, applying the specified `options`.
///
/// The line is cleared before reading, and the resulting line does not include
/// the record-ending character(s).
/// Returns the record terminator on success, or an error if an I/O error occurs
/// or if the input is invalid according to the options.
/// A line can end with \r, \n, or \r\n, and bytes inside the record are preserved unchanged.
/// `location.bytes` is increased by every byte consumed from `reader`, including
/// stripped record terminators. `location.line` is increased after successful reads
/// except [`ReadResult::Eof`]. Errors include the file name and line number; on
/// error, `location.line` is not increased, but `location.bytes` may already reflect
/// bytes consumed before the error was detected.
///
/// If Eof is returned the line buffer is not modified.
pub fn read(
    line: &mut Vec<u8>,
    reader: &mut dyn BufRead,
    options: &Config,
    location: &mut FileLocData,
) -> anyhow::Result<ReadResult> {
    let available = fill_buf(reader, location)?;
    if available.is_empty() {
        return Ok(ReadResult::Eof);
    }
    line.clear();

    let result = if options.quotes == Quotes::None && options.backslash == BackslashMode::Off {
        read_unquoted(line, reader, location)
    } else {
        read_quoted(line, reader, options, location)
    }?;

    if result != ReadResult::Eof {
        location.line += 1;
    }

    Ok(result)
}

fn read_unquoted(
    line: &mut Vec<u8>,
    reader: &mut dyn BufRead,
    location: &mut FileLocData,
) -> anyhow::Result<ReadResult> {
    loop {
        let available = fill_buf(reader, location)?;
        if available.is_empty() {
            return Ok(if line.is_empty() { ReadResult::Eof } else { ReadResult::NoTerminator });
        }

        if let Some(index) = memchr::memchr2(b'\n', b'\r', available) {
            line.extend_from_slice(&available[..index]);
            if available[index] == b'\r' {
                let consume_len =
                    if available.get(index + 1) == Some(&b'\n') { index + 2 } else { index + 1 };
                let cr_ends_buffer = consume_len == available.len() && available[index] == b'\r';
                consume(reader, location, consume_len);
                if cr_ends_buffer && consume_len == index + 1 && consume_split_lf(reader, location)?
                {
                    return Ok(ReadResult::CrLf);
                }
                return Ok(if consume_len == index + 2 {
                    ReadResult::CrLf
                } else {
                    ReadResult::Cr
                });
            }
            consume(reader, location, index + 1);
            return Ok(ReadResult::Lf);
        }

        let len = available.len();
        line.extend_from_slice(available);
        consume(reader, location, len);
    }
}

fn read_quoted(
    line: &mut Vec<u8>,
    reader: &mut dyn BufRead,
    options: &Config,
    location: &mut FileLocData,
) -> anyhow::Result<ReadResult> {
    let mut close_quote = None;
    let mut escaped = false;

    'read: loop {
        let available = fill_buf(reader, location)?;
        if available.is_empty() {
            return Ok(if line.is_empty() { ReadResult::Eof } else { ReadResult::NoTerminator });
        }

        let mut consumed = 0;
        while consumed < available.len() {
            let byte = available[consumed];

            if byte == b'\n' || byte == b'\r' {
                escaped = false;
                line.extend_from_slice(&available[..consumed]);
                let terminator_in_buffer_len =
                    if byte == b'\r' && available.get(consumed + 1) == Some(&b'\n') {
                        2
                    } else {
                        1
                    };

                if close_quote.is_none()
                    || options.unterminated_quote == UnterminatedQuoteMode::EndAtEol
                {
                    let cr_ends_buffer = byte == b'\r' && consumed + 1 == available.len();
                    consume(reader, location, consumed + terminator_in_buffer_len);
                    if cr_ends_buffer && consume_split_lf(reader, location)? {
                        return Ok(ReadResult::CrLf);
                    }
                    return Ok(if terminator_in_buffer_len == 2 {
                        ReadResult::CrLf
                    } else if byte == b'\r' {
                        ReadResult::Cr
                    } else {
                        ReadResult::Lf
                    });
                }

                if options.unterminated_quote == UnterminatedQuoteMode::Error {
                    let cr_ends_buffer = byte == b'\r' && consumed + 1 == available.len();
                    consume(reader, location, consumed + terminator_in_buffer_len);
                    if cr_ends_buffer {
                        let _ = consume_split_lf(reader, location)?;
                    }
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "{}: end of line reached inside quoted field",
                            location_display(location)
                        ),
                    )
                    .into());
                }

                line.extend_from_slice(&available[consumed..consumed + terminator_in_buffer_len]);
                let cr_ends_buffer = byte == b'\r' && consumed + 1 == available.len();
                consume(reader, location, consumed + terminator_in_buffer_len);
                let _ = cr_ends_buffer && append_split_lf(line, reader, location)?;
                continue 'read;
            }

            if options.backslash == BackslashMode::On && escaped {
                escaped = false;
                consumed += 1;
                continue;
            }

            if options.backslash == BackslashMode::On && byte == b'\\' {
                escaped = true;
                consumed += 1;
                continue;
            }

            if let Some(close) = close_quote {
                if byte == close {
                    close_quote = None;
                }
            } else if let Some(close) = quote_close(byte, &options.quotes) {
                close_quote = Some(close);
            }

            consumed += 1;
        }

        if consumed == available.len() {
            line.extend_from_slice(available);
            consume(reader, location, consumed);
        }
    }
}

fn quote_close(open: u8, quotes: &Quotes) -> Option<u8> {
    match quotes {
        Quotes::None => None,
        Quotes::Single(start, end) => (*start == open).then_some(*end),
        Quotes::Multi(pairs) => {
            pairs.iter().find_map(|(start, end)| (*start == open).then_some(*end))
        }
    }
}

fn fill_buf<'a>(reader: &'a mut dyn BufRead, location: &FileLocData) -> anyhow::Result<&'a [u8]> {
    reader.fill_buf().map_err(|err| {
        io::Error::new(err.kind(), format!("{}: {err}", location_display(location))).into()
    })
}

fn location_display(location: &FileLocData) -> String {
    format!("{}:{}", location.name, location.line + 1)
}

pub(crate) fn consume(reader: &mut dyn BufRead, location: &mut FileLocData, bytes: usize) {
    reader.consume(bytes);
    location.bytes += bytes;
}

pub(crate) fn consume_split_lf(
    reader: &mut dyn BufRead,
    location: &mut FileLocData,
) -> anyhow::Result<bool> {
    if fill_buf(reader, location)?.first() == Some(&b'\n') {
        consume(reader, location, 1);
        return Ok(true);
    }
    Ok(false)
}

fn append_split_lf(
    line: &mut Vec<u8>,
    reader: &mut dyn BufRead,
    location: &mut FileLocData,
) -> anyhow::Result<bool> {
    if fill_buf(reader, location)?.first() == Some(&b'\n') {
        line.push(b'\n');
        consume(reader, location, 1);
        return Ok(true);
    }
    Ok(false)
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, Cursor};

    use super::*;

    struct ErrorReader;

    impl Read for ErrorReader {
        fn read(&mut self, _buffer: &mut [u8]) -> io::Result<usize> {
            Err(io::Error::other("forced read failure"))
        }
    }

    fn read_bytes(input: &[u8], options: &Config) -> anyhow::Result<(Vec<u8>, ReadResult)> {
        let mut reader = Cursor::new(input);
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");
        let result = read(&mut line, &mut reader, options, &mut location)?;
        Ok((line, result))
    }

    #[test]
    fn unquoted_strips_line_terminators() -> anyhow::Result<()> {
        let options = Config::default();

        assert_eq!(read_bytes(b"abc\nnext", &options)?, (b"abc".to_vec(), ReadResult::Lf));
        assert_eq!(read_bytes(b"abc\rnext", &options)?, (b"abc".to_vec(), ReadResult::Cr));
        assert_eq!(read_bytes(b"abc\r\nnext", &options)?, (b"abc".to_vec(), ReadResult::CrLf));
        assert_eq!(read_bytes(b"abc", &options)?, (b"abc".to_vec(), ReadResult::NoTerminator));
        assert_eq!(read_bytes(b"", &options)?, (Vec::new(), ReadResult::Eof));

        Ok(())
    }

    #[test]
    fn continued_quote_preserves_inner_line_ending() -> anyhow::Result<()> {
        let options = Config { quotes: Quotes::Single(b'"', b'"'), ..Config::default() };

        assert_eq!(
            read_bytes(b"\"a\r\nb\"\r\nnext", &options)?,
            (b"\"a\r\nb\"".to_vec(), ReadResult::CrLf)
        );

        Ok(())
    }

    #[test]
    fn unterminated_quote_can_end_at_eol() -> anyhow::Result<()> {
        let options = Config {
            quotes: Quotes::Single(b'"', b'"'),
            unterminated_quote: UnterminatedQuoteMode::EndAtEol,
            ..Config::default()
        };

        assert_eq!(read_bytes(b"\"abc\nnext", &options)?, (b"\"abc".to_vec(), ReadResult::Lf));

        Ok(())
    }

    #[test]
    fn unterminated_quote_can_error() {
        let options = Config {
            quotes: Quotes::Single(b'"', b'"'),
            unterminated_quote: UnterminatedQuoteMode::Error,
            ..Config::default()
        };

        assert!(read_bytes(b"\"abc\nnext", &options).is_err());
    }

    #[test]
    fn backslash_preserves_bytes_and_escapes_quote_state() -> anyhow::Result<()> {
        let options = Config {
            quotes: Quotes::Single(b'"', b'"'),
            backslash: BackslashMode::On,
            ..Config::default()
        };

        assert_eq!(
            read_bytes(br#""a\"b":tail"#, &options)?,
            (br#""a\"b":tail"#.to_vec(), ReadResult::NoTerminator)
        );

        Ok(())
    }

    #[test]
    fn location_counts_consumed_bytes_and_successful_records() -> anyhow::Result<()> {
        let options = Config::default();
        let mut reader = Cursor::new(b"abc\r\nnext".as_slice());
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::CrLf);
        assert_eq!(line, b"abc");
        assert_eq!(location.line, 1);
        assert_eq!(location.bytes, 5);

        assert_eq!(
            read(&mut line, &mut reader, &options, &mut location)?,
            ReadResult::NoTerminator
        );
        assert_eq!(line, b"next");
        assert_eq!(location.line, 2);
        assert_eq!(location.bytes, 9);

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::Eof);
        assert_eq!(line, b"next");
        assert_eq!(location.line, 2);
        assert_eq!(location.bytes, 9);

        Ok(())
    }

    #[test]
    fn location_counts_consumed_bytes_for_continued_quotes() -> anyhow::Result<()> {
        let options = Config { quotes: Quotes::Single(b'"', b'"'), ..Config::default() };
        let mut reader = Cursor::new(b"\"a\r\nb\"\r\nnext".as_slice());
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::CrLf);
        assert_eq!(line, b"\"a\r\nb\"");
        assert_eq!(location.line, 1);
        assert_eq!(location.bytes, 8);

        Ok(())
    }

    #[test]
    fn location_counts_split_crlf_for_unquoted_records() -> anyhow::Result<()> {
        let options = Config::default();
        let cursor = Cursor::new(b"abc\r\nnext".as_slice());
        let mut reader = BufReader::with_capacity(4, cursor);
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::CrLf);
        assert_eq!(line, b"abc");
        assert_eq!(location.line, 1);
        assert_eq!(location.bytes, 5);

        Ok(())
    }

    #[test]
    fn location_counts_split_crlf_inside_continued_quotes() -> anyhow::Result<()> {
        let options = Config { quotes: Quotes::Single(b'"', b'"'), ..Config::default() };
        let cursor = Cursor::new(b"\"a\r\nb\"\nnext".as_slice());
        let mut reader = BufReader::with_capacity(3, cursor);
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::Lf);
        assert_eq!(line, b"\"a\r\nb\"");
        assert_eq!(location.line, 1);
        assert_eq!(location.bytes, 7);

        Ok(())
    }

    #[test]
    fn error_messages_include_file_name_and_line_number() -> anyhow::Result<()> {
        let options = Config {
            quotes: Quotes::Single(b'"', b'"'),
            unterminated_quote: UnterminatedQuoteMode::Error,
            ..Config::default()
        };
        let mut reader = Cursor::new(b"ok\n\"abc\nnext".as_slice());
        let mut line = Vec::new();
        let mut location = FileLocData::new("input.txt");

        assert_eq!(read(&mut line, &mut reader, &options, &mut location)?, ReadResult::Lf);
        let error = read(&mut line, &mut reader, &options, &mut location)
            .expect_err("unterminated quote should error");

        assert!(error.to_string().contains("input.txt:2:"), "{error:#}");
        assert_eq!(location.line, 1);
        assert_eq!(location.bytes, 8);

        Ok(())
    }

    #[test]
    fn io_error_messages_include_file_name_and_line_number() {
        let options = Config::default();
        let mut reader = BufReader::new(ErrorReader);
        let mut line = Vec::new();
        let mut location = FileLocData::new("broken.txt");

        let error =
            read(&mut line, &mut reader, &options, &mut location).expect_err("reader should fail");

        assert!(error.to_string().contains("broken.txt:1:"), "{error:#}");
        assert!(error.to_string().contains("forced read failure"), "{error:#}");
        assert_eq!(location.line, 0);
        assert_eq!(location.bytes, 0);
    }
}
