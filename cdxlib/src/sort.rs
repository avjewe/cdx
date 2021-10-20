//! Tools for sorting text files

use crate::comp::{Comparator, Item};
use crate::{err, get_reader, Error, Infile, Result, TextLine};
use std::io::{self, Write};

/// convert u8 slice to string
pub fn u2s(v: &[u8]) -> String {
    String::from_utf8_lossy(v).to_string()
}

/// Read a text file, but not line-by-line
#[derive(Debug)]
pub struct BlockReader {
    file: Infile,
    /// file header
    pub header: TextLine,
    /// column delimiter
    pub delim: u8,
    /// eof has been reached
    pub is_done: bool,
    /// file was zero bytes
    pub is_empty: bool,
}

impl BlockReader {
    /// new BlockReader
    pub fn new() -> Self {
        Self {
            file: Infile::new(io::BufReader::new(Box::new(io::empty()))),
            header: TextLine::new(),
            delim: b'\t',
            is_done: true,
            is_empty: true,
        }
    }
    /// open the file
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        Ok(())
        /*
                read_header(
                    &mut self.file,
                    &mut self.header,
                    &mut self.line,
                    &mut self.delim,
                    &mut self.is_done,
                    &mut self.is_empty,
                )
        */
    }
}

impl Default for BlockReader {
    fn default() -> Self {
        Self::new()
    }
}

/// Large block of text and pointers to lines therein
#[derive(Debug)]
pub struct Sorter {
    ptrs: Vec<Item>,
    data: Vec<u8>,
    //    used : u32,
}

impl Sorter {
    /// new Sorter
    pub fn new() -> Self {
        Self {
            ptrs: Vec::new(),
            data: Vec::new(),
            //	    used : 0
        }
    }
    /// Add another file's worth of data to the stream
    pub fn add(
        &mut self,
        _r: &mut BlockReader,
        cmp: &Comparator,
        _w: &mut dyn Write,
    ) -> Result<()> {
        self.ptrs
            .sort_by(|a, b| cmp.comp.comp_items(cmp, &self.data, a, b));
        Ok(())
    }
    /// All files have been added, write final results
    pub fn finalize(&mut self, cmp: &Comparator, _w: &mut dyn Write) -> Result<()> {
        self.ptrs
            .sort_by(|a, b| cmp.comp.comp_items(cmp, &self.data, a, b));
        Ok(())
    }
}

impl Default for Sorter {
    fn default() -> Self {
        Self::new()
    }
}

/// Sort all the files together, into w
/// will have to use a temp directory somehow
pub fn sort(files: &[String], cmp: &mut Comparator, w: &mut dyn Write) -> Result<()> // maybe return some useful stats?
{
    let mut s = Sorter::new();
    let mut header: Vec<u8> = Vec::new();
    let mut first_file = true;

    for f in files {
        let mut b = BlockReader::new();
        b.open(f)?;
        if first_file {
            first_file = false;
            header = b.header.line.clone();
            w.write_all(&header)?;
        } else if b.header.line != header {
            return err!(
                "Header Mismatch : '{}' vs '{}'",
                &u2s(&header),
                &u2s(&b.header.line)
            );
        }
        s.add(&mut b, cmp, w)?;
    }
    s.finalize(cmp, w)
}
