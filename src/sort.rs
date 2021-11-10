//! Tools for sorting text files

use crate::comp::{Comparator, Item};
use crate::{err, get_reader, Error, Infile, InfileContext, Result, TextLine};
use std::io::{self, Read, Write};

/*
/// convert u8 slice to string
pub fn u2s(v: &[u8]) -> String {
    String::from_utf8_lossy(v).to_string()
}
 */

/// Read a text file, but not line-by-line
#[derive(Debug)]
pub struct BlockReader {
    file: Infile,
    cont: InfileContext,
    first: TextLine,
}

impl BlockReader {
    /// new BlockReader
    pub fn new() -> Self {
        Self {
            file: Infile::new(io::BufReader::new(Box::new(io::empty()))),
            cont: InfileContext::new(),
            first: TextLine::new(),
        }
    }
    /// open the file
    pub fn open(&mut self, name: &str) -> Result<()> {
        self.file = get_reader(name)?;
        self.cont.read_header(&mut *self.file, &mut self.first)?;
        Ok(())
    }
    /// read from file, append to 'data' to a maximum of 'max' total size
    /// return bytes read.
    pub fn read(&mut self, data: &mut Vec<u8>, offset: usize) -> Result<usize> {
        let mut nsize = offset + 16 * 1024;
        if nsize > data.capacity() {
            nsize = data.capacity();
        }
        if data.len() < nsize {
            data.resize(nsize, 0);
        }
        if offset >= data.len() {
            return Ok(0);
        }
        if self.first.line.is_empty() {
            let sz = self.file.read(&mut data[offset..])?;
            Ok(sz)
        } else {
            debug_assert!(offset == 0);
            let len = self.first.line.len();
            data[0..len].copy_from_slice(&self.first.line[0..len]);
            self.first.line.clear();
            Ok(len)
        }
    }
}

impl Default for BlockReader {
    fn default() -> Self {
        Self::new()
    }
}

/// Large block of text and pointers to lines therein
#[derive(Debug)]
pub struct Sorter<'a> {
    ptrs: Vec<Item>,
    data: Vec<u8>,
    cmp: &'a Comparator,

    /// bytes in data that are referenced by ptrs
    /// needed because I can't just read into a Vec, of I don't want the whole file
    data_used: usize,
    /// bytes in data from input files, not referenced by ptrs
    data_unused: usize,
}

const MAX_DATA: usize = 0x0ffffff00;

impl<'a> Sorter<'a> {
    /// new Sorter
    pub fn new(cmp: &'a Comparator, max_alloc: usize) -> Self {
        let mut data_size = max_alloc / 2;
        if data_size > MAX_DATA {
            data_size = MAX_DATA;
        }
        let ptr_size = max_alloc / 2 / std::mem::size_of::<Item>();
        Self {
            ptrs: Vec::with_capacity(ptr_size),
            data: Vec::with_capacity(data_size),
            cmp,
            data_used: 0,
            data_unused: 0,
        }
    }
    /// Add another file's worth of data to the stream
    /// possibly writing temporary files
    pub fn add(&mut self, r: &mut BlockReader) -> Result<()> {
        loop {
            let nbytes = r.read(&mut self.data, self.data_used)?;
            if nbytes == 0 {
                break;
            }
            self.data_used += nbytes;
            if self.data_used >= self.data.capacity() {
                break;
            }
        }
        self.calc();
        Ok(())
    }
    /// Populate 'ptrs' from 'data'
    fn calc(&mut self) {
        self.ptrs.clear();
        let mut item = Item::new();
        let mut off: usize = 0;
        for iter in self.data.iter().enumerate() {
            if iter.1 == &b'\n' {
                item.offset = off as u32;
                item.size_plus = (iter.0 - off + 1) as u32;
                off = iter.0 + 1;
                self.cmp
                    .comp
                    .fill_cache_item(self.cmp, &mut item, &self.data);
                self.ptrs.push(item);
            }
        }
        self.data_unused = self.data.len() - off;
    }
    /// All files have been added, write final results
    pub fn finalize(&mut self, w: &mut dyn Write) -> Result<()> {
        self.ptrs
            .sort_by(|a, b| self.cmp.comp.comp_items(self.cmp, &self.data, a, b));
        for &x in &self.ptrs {
            w.write_all(x.get(&self.data))?;
        }
        Ok(())
    }
}

/// Sort all the files together, into w
/// will have to use a temp directory somehow
pub fn sort(files: &[String], cmp: &mut Comparator, w: &mut dyn Write) -> Result<()> // maybe return some useful stats?
{
    let mut s = Sorter::new(cmp, 1000000000);
    let mut header: String = String::new();
    let mut first_file = true;

    for f in files {
        let mut b = BlockReader::new();
        b.open(f)?;
        if first_file {
            first_file = false;
            header = b.cont.header.line.clone();
            w.write_all(header.as_bytes())?;
        } else if b.cont.header.line != header {
            return err!(
                "Header Mismatch : '{}' vs '{}'",
                &header,
                &b.cont.header.line
            );
        }
        s.add(&mut b)?;
    }
    s.finalize(w)
}
