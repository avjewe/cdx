//! Tools for sorting text files

/*
incremental calc
N-way merge
add HEADER_MODE and total_size as parameters
fancier sort
 */

use crate::comp::{Comparator, Item};
use crate::{copy, err, get_reader, get_writer, is_cdx, Error, HeaderChecker, Reader, Result};
use std::cmp::Ordering;
use std::io::{BufRead, Read, Write};
use std::mem;
use tempdir::TempDir;

/// merge all the files into w, using tmp
pub fn merge_t(
    in_files: &[String],
    cmp: &mut Comparator,
    w: impl Write,
    unique: bool,
    tmp: &TempDir,
) -> Result<()> {
    if in_files.is_empty() {
        return Ok(());
    }
    if in_files.len() == 1 {
        let r = get_reader(&in_files[0])?;
        return copy(r.0, w);
    }

    let mut files = in_files.to_owned();
    let mut n = 0;
    loop {
        if files.len() == 2 {
            return merge_2(&files[0], &files[1], cmp, w, unique);
        }
        let mut tmp_file = tmp.path().to_owned();
        tmp_file.push(format!("merge_{}.txt", n));
        n += 1;
        let tmp_name = tmp_file.to_str().unwrap();
        let new_w = get_writer(tmp_name)?;
        merge_2(&files[0], &files[1], cmp, new_w, unique)?;
        files.remove(0);
        files.remove(0);
        files.push(tmp_name.to_string());
    }
}

/// merge all the files into w
pub fn merge(files: &[String], cmp: &mut Comparator, w: impl Write, unique: bool) -> Result<()> {
    let tmp = TempDir::new("merge")?;
    merge_t(files, cmp, w, unique, &tmp)
}

/// given two file names, merge them into output
pub fn merge_2(
    left: &str,
    right: &str,
    cmp: &mut Comparator,
    mut w: impl Write,
    unique: bool,
) -> Result<()> {
    let mut left_file = Reader::new();
    let mut right_file = Reader::new();
    left_file.open(left)?;
    right_file.open(right)?;
    left_file.do_split = false;
    right_file.do_split = false;
    cmp.lookup(&left_file.names())?;

    // FIXME -- Check Header
    if left_file.cont.has_header {
        w.write_all(left_file.header().line.as_bytes())?;
    }

    if unique {
        let mut prev: Vec<u8> = Vec::new();
        while !left_file.is_done() && !right_file.is_done() {
            let ord = cmp.comp_lines(&left_file.curr().line, &right_file.curr().line);
            if ord == Ordering::Less {
                left_file.write(&mut w)?;
                mem::swap(&mut prev, &mut left_file.line.line);
                left_file.getline()?;
            } else if ord == Ordering::Greater {
                right_file.write(&mut w)?;
                mem::swap(&mut prev, &mut left_file.line.line);
                right_file.getline()?;
            } else {
                left_file.write(&mut w)?;
                mem::swap(&mut prev, &mut left_file.line.line);
                left_file.getline()?;
                right_file.getline()?;
            }
            while !left_file.is_done() && cmp.equal_lines(&left_file.curr().line, &prev) {
                left_file.getline()?;
            }
            while !right_file.is_done() && cmp.equal_lines(&right_file.curr().line, &prev) {
                right_file.getline()?;
            }
        }
    } else {
        while !left_file.is_done() && !right_file.is_done() {
            let ord = cmp.comp_lines(&left_file.curr().line, &right_file.curr().line);
            // if Equal, write both lines
            if ord != Ordering::Less {
                right_file.write(&mut w)?;
                right_file.getline()?;
            }
            if ord != Ordering::Greater {
                left_file.write(&mut w)?;
                left_file.getline()?;
            }
        }
    }
    while !left_file.is_done() {
        left_file.write(&mut w)?;
        left_file.getline()?;
    }
    while !right_file.is_done() {
        right_file.write(&mut w)?;
        right_file.getline()?;
    }
    Ok(())
}

/// Large block of text and pointers to lines therein
#[derive(Debug)]
pub struct Sorter {
    ptrs: Vec<Item>,
    cmp: Comparator,
    tmp: TempDir,
    tmp_files: Vec<String>,
    unique: bool,
    checker: HeaderChecker,

    // raw data. never resized smaller, so use data_used for real size
    data: Vec<u8>,

    // bytes of real data in Vec
    data_used: usize,

    // bytes of data referenced by ptrs
    // a.k.a. offset of first byte not referenced by ptrs
    // assert(data_calc <= data_used)
    data_calc: usize,

    // number of btes beyond data_calc, known to be free of newlines
    // to avoid N^2 craziness with long lines
    // assert(data_calc+data_nonl <= data_used)
    data_nonl: usize,
}

const MAX_DATA: usize = 0x0ffffff00;

impl Sorter {
    /// new Sorter
    pub fn new(cmp: Comparator, max_alloc: usize, unique: bool) -> Self {
        let mut data_size = max_alloc / 2;
        if data_size > MAX_DATA {
            data_size = MAX_DATA;
        }
        let ptr_size = max_alloc / 2 / std::mem::size_of::<Item>();
        Self {
            ptrs: Vec::with_capacity(ptr_size),
            data: Vec::with_capacity(data_size),
            cmp,
            tmp: TempDir::new("sort").unwrap(), // FIXME - new should return Result
            tmp_files: Vec::new(),
            unique,
            checker: HeaderChecker::new(),
            data_used: 0,
            data_calc: 0,
            data_nonl: 0,
        }
    }

    fn check(&self) -> bool {
        debug_assert!(self.data_used <= self.data.len());
        debug_assert!(self.data_calc <= self.data_used);
        debug_assert!((self.data_calc + self.data_nonl) <= self.data_used);
        true
    }

    // number of bytes available to write
    fn avail(&self) -> usize {
        self.data.len() - self.data_used
    }

    // try to make N bytes available, return amount actually available
    fn prepare(&mut self, n: usize) -> usize {
        let mut nsize = self.data_used + n;
        if nsize > self.data.capacity() {
            nsize = self.data.capacity();
        }
        if self.data.len() < nsize {
            self.data.resize(nsize, 0);
        }
        let avail = self.avail();
        if avail < n {
            avail
        } else {
            n
        }
    }

    /// add some more data to be sorted.
    /// must be integer number of lines.
    pub fn add_data(&mut self, in_data: &[u8]) -> Result<()> {
        let sz = self.prepare(in_data.len());
        if sz != in_data.len() {
            eprintln!("Failed to prepare {}, only got {}", in_data.len(), sz);
            return err!("Badness");
        }
        self.data[self.data_used..self.data_used + in_data.len()].copy_from_slice(in_data);
        self.data_used += in_data.len();
        // FIXME - add newline
        Ok(())
    }
    /// Add another file's worth of data to the stream
    /// possibly writing temporary files
    pub fn add(&mut self, mut r: impl Read) -> Result<()> {
        loop {
            debug_assert!(self.check());
            const SIZE: usize = 16 * 1024;
            let sz = self.prepare(SIZE);
            debug_assert!(sz > 0);
            let nbytes = r.read(&mut self.data[self.data_used..self.data_used + sz])?;
            if nbytes == 0 {
                if self.data_used > 0 && self.data[self.data_used - 1] != b'\n' {
                    self.data[self.data_used] = b'\n';
                    self.data_used += 1;
                }
                return Ok(());
            }
            self.data_used += nbytes;
            // calc new stuff
            if self.data_used >= self.data.capacity() {
                self.calc();
                self.do_sort();
                self.write_tmp()?;
            }
        }
    }
    /// Populate 'ptrs' from 'data'
    fn calc(&mut self) {
        self.ptrs.clear();
        let mut item = Item::new();
        let mut off: usize = 0;
        for iter in self.data[0..self.data_used].iter().enumerate() {
            if iter.1 == &b'\n' {
                item.offset = off as u32;
                item.size_plus = (iter.0 - off + 1) as u32;
                off = iter.0 + 1;
                self.cmp
                    .comp
                    .fill_cache_item(&self.cmp, &mut item, &self.data);
                self.ptrs.push(item);
            }
        }
        self.data_calc = off;
    }

    /// write ptrs to tmp file
    fn write_tmp(&mut self) -> Result<()> {
        let mut tmp_file = self.tmp.path().to_owned();
        tmp_file.push(format!("sort_{}.txt", self.tmp_files.len()));
        let tmp_name = tmp_file.to_str().unwrap();
        let mut new_w = get_writer(tmp_name)?;
        for &x in &self.ptrs {
            new_w.write_all(x.get(&self.data))?;
        }
        self.tmp_files.push(tmp_name.to_string());
        self.ptrs.clear();
        let nsize = self.data.len() - self.data_calc;
        for i in 0..nsize {
            self.data[i] = self.data[self.data_calc + i];
        }
        self.data_used = nsize;
        self.data_calc = 0;
        Ok(())
    }

    /// sort and unique self.ptrs
    fn do_sort(&mut self) {
        self.ptrs
            .sort_by(|a, b| self.cmp.comp.comp_items(&self.cmp, &self.data, a, b));
        if self.unique {
            self.ptrs
                .dedup_by(|a, b| self.cmp.comp.equal_items(&self.cmp, &self.data, a, b));
        }
    }
    /// All files have been added, write final results
    pub fn finalize(&mut self, mut w: impl Write) -> Result<()> {
        self.calc();
        self.do_sort();
        if self.tmp_files.is_empty() {
            for &x in &self.ptrs {
                w.write_all(x.get(&self.data))?;
            }
        } else {
            self.write_tmp()?;
            merge_t(&self.tmp_files, &mut self.cmp, w, self.unique, &self.tmp)?;
        }
        Ok(())
    }

    #[allow(dead_code)]
    fn no_del(self) {
        eprintln!(
            "Not deleting {}",
            self.tmp.into_path().into_os_string().to_string_lossy()
        );
    }
    /// add another file to be sorted
    pub fn add_file<W: Write>(&mut self, fname: &str, w: &mut W) -> Result<()> {
        let mut f = get_reader(fname)?;
        let mut first_line = Vec::new();
        let n = f.read_until(b'\n', &mut first_line)?;
        if n == 0 {
            return Ok(());
        }
        if self.checker.check(&first_line, fname)? {
            if is_cdx(&first_line) {
                w.write_all(&first_line)?;
            } else {
                self.add_data(&first_line)?;
            }
        }
        self.add(&mut *f)
    }
}

/// Sort all the files together, into w
pub fn sort<W: Write>(files: &[String], cmp: Comparator, w: &mut W, unique: bool) -> Result<()> // maybe return some useful stats?
{
    let mut s = Sorter::new(cmp, 100000000, unique);
    for fname in files {
        s.add_file(fname, w)?;
    }
    s.finalize(w)?;
    //    s.no_del();
    Ok(())
}
