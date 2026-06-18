//! Abstractions for reading input from the console using Rustyline.

use crate::prelude::*;

use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::io;

/// Read stdin with Rustyline, allowing for line editing and history.
pub fn new_reader(prompt: &str) -> Result<Box<dyn Read>> {
    let editor = DefaultEditor::new().expect("Failed to create Rustyline editor");
    Ok(Box::new(RustyLineReader::new(editor, prompt)))
}

/// An adapter that wraps a Rustyline editor and implements `std::io::Read`.
struct RustyLineReader {
    editor: DefaultEditor,
    prompt: String,
    buffer: Vec<u8>,
}

impl RustyLineReader {
    fn new(editor: DefaultEditor, prompt: &str) -> Self {
        Self { editor, prompt: prompt.to_string(), buffer: Vec::new() }
    }
}

impl Read for RustyLineReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // If our internal buffer is empty, we need to block and fetch the next line
        if self.buffer.is_empty() {
            match self.editor.readline(&self.prompt) {
                Ok(line) => {
                    // Add the line to history (optional)
                    let _ = self.editor.add_history_entry(line.as_str()).unwrap();

                    // Convert the line to bytes and append a newline character
                    // so it behaves like standard console input
                    self.buffer = format!("{line}\n").into_bytes();
                }
                Err(ReadlineError::Eof) => {
                    // EOF (Ctrl-D) means we return 0 bytes read, signaling the end of the stream
                    return Ok(0);
                }
                Err(ReadlineError::Interrupted) => {
                    // Handle Ctrl-C as an Interrupted IO error
                    return Err(io::Error::new(io::ErrorKind::Interrupted, "Ctrl-C"));
                }
                Err(err) => {
                    // Wrap any other rustyline errors into an IO error
                    return Err(io::Error::other(err));
                }
            }
        }

        // Consume bytes from our internal buffer into the provided output buffer
        let amt = std::cmp::min(buf.len(), self.buffer.len());
        buf[..amt].copy_from_slice(&self.buffer[..amt]);

        // Remove the consumed bytes from our internal buffer
        self.buffer.drain(..amt);

        Ok(amt)
    }
}
