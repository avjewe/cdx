//! Tools for sorting text files

/*
incremental calc for better locality
N-way merge
add HEADER_MODE and total_size as parameters
fancier sort
 */

use crate::comp::Item;
use crate::prelude::*;
use crate::util::{copy, get_reader, is_cdx, make_header, HeaderChecker};
use binary_heap_plus::BinaryHeap;
use std::cell::RefCell;
//use std::mem;
use std::rc::Rc;
use tempfile::TempDir;

struct MergeContext<'a> {
    open: Vec<Reader>,
    cmp: &'a mut LineCompList,
}
impl MergeContext<'_> {
    fn compare(&mut self, a: usize, b: usize) -> Ordering {
        self.cmp
            .comp_cols(self.open[a].curr_line(), self.open[b].curr_line())
            .reverse()
    }
    fn equal(&mut self, a: &TextLine, b: usize) -> bool {
        self.cmp.equal_cols(a, self.open[b].curr_line())
    }
}

/// sort configuration
#[derive(Copy, Clone, Debug, Default)]
pub struct SortConfig {
    /// use a different sort algorithm
    pub alt_sort: bool,
    /// use a different merge algorithm
    pub alt_merge: bool,
}

impl SortConfig {
    /// merge all the files into w, using tmp
    pub fn merge_t(
        &self,
        in_files: &[String],
        cmp: &mut LineCompList,
        w: impl Write,
        unique: bool,
        tmp: &TempDir,
    ) -> Result<()> {
        eprintln!("Merging");
        if self.alt_merge {
            self.merge_t1(in_files, cmp, w, unique, tmp)
        } else {
            self.merge_t2(in_files, cmp, w, unique, tmp)
        }
    }

    /// merge all the files into w, using tmp
    pub fn merge_t2(
        &self,
        in_files: &[String],
        cmp: &mut LineCompList,
        mut w: impl Write,
        unique: bool,
        _tmp: &TempDir,
    ) -> Result<()> {
        if in_files.is_empty() {
            return Ok(());
        }
        if in_files.len() == 1 && !unique {
            let r = get_reader(&in_files[0])?;
            return copy(r.0, w);
        }
        let mc = Rc::new(RefCell::new(MergeContext {
            open: Vec::with_capacity(in_files.len()),
            cmp,
        }));
        let mut heap = BinaryHeap::new_by(|a: &usize, b: &usize| mc.borrow_mut().compare(*a, *b));
        {
            let mut mcm = mc.borrow_mut();
            for x in in_files {
                mcm.open.push(Reader::new_open2(x)?);
            }
            if !mcm.cmp.need_split() {
                for x in &mut mcm.open {
                    x.do_split(false);
                }
            }
            // FIXME -- Check Header
            if mcm.open[0].has_header() {
                w.write_all(mcm.open[0].header().line.as_bytes())?;
            }
        }
        for i in 0..in_files.len() {
            if !mc.borrow().open[i].is_done() {
                heap.push(i);
            }
        }
        if unique {
            if heap.is_empty() {
                return Ok(());
            }
            let first = heap.pop().unwrap();
            let mut prev = mc.borrow().open[first].curr_line().clone();
            if !mc.borrow_mut().open[first].getline()? {
                heap.push(first);
            }
            w.write_all(prev.line())?;

            while !heap.is_empty() {
                if let Some(x) = heap.pop() {
                    let eq = mc.borrow_mut().equal(&prev, x);
                    if !eq {
                        let mcm = mc.borrow();
                        w.write_all(mcm.open[x].curr_line().line())?;
                        prev.assign(mcm.open[x].curr_line());
                    }
                    if !mc.borrow_mut().open[x].getline()? {
                        heap.push(x);
                    }
                }
            }
        } else {
            while !heap.is_empty() {
                if let Some(x) = heap.pop() {
                    w.write_all(mc.borrow_mut().open[x].curr_line().line())?;
                    if !mc.borrow_mut().open[x].getline()? {
                        heap.push(x);
                    }
                }
            }
        }
        Ok(())
    }

    /// merge all the files into w, using tmp
    pub fn merge_t1(
        &self,
        in_files: &[String],
        cmp: &mut LineCompList,
        mut w: impl Write,
        unique: bool,
        _tmp: &TempDir,
    ) -> Result<()> {
        if in_files.is_empty() {
            return Ok(());
        }
        if in_files.len() == 1 && !unique {
            let r = get_reader(&in_files[0])?;
            return copy(r.0, w);
        }
        let mut open_files: Vec<Reader> = Vec::with_capacity(in_files.len());
        for x in in_files {
            open_files.push(Reader::new_open2(x)?);
        }
        if !cmp.need_split() {
            for x in &mut open_files {
                x.do_split(false);
            }
        }
        // FIXME -- Check Header
        if open_files[0].has_header() {
            w.write_all(open_files[0].header().line.as_bytes())?;
        }

        let nums: Vec<usize> = (0..open_files.len()).collect();
        let mut mm = MergeTreeItem::new_tree(&nums);
        if unique {
            let x = mm.next(cmp, &mut open_files)?;
            if x.is_none() {
                return Ok(());
            }
            let x = x.unwrap();
            w.write_all(open_files[x].curr_line().line())?;
            let mut prev = open_files[x].curr_line().clone();
            loop {
                let x = mm.next(cmp, &mut open_files)?;
                if x.is_none() {
                    break;
                }
                let x = x.unwrap();
                if !cmp.equal_cols(&prev, open_files[x].curr_line()) {
                    w.write_all(open_files[x].curr_line().line())?;
                }
                prev.assign(open_files[x].curr_line());
            }
        } else {
            loop {
                let x = mm.next(cmp, &mut open_files)?;
                if x.is_none() {
                    break;
                }
                let x = x.unwrap();
                w.write_all(open_files[x].curr_line().line())?;
            }
        }
        Ok(())
    }

    /// merge all the files into w
    pub fn merge(
        &self,
        files: &[String],
        cmp: &mut LineCompList,
        w: impl Write,
        unique: bool,
    ) -> Result<()> {
        let tmp = TempDir::new()?;
        if self.alt_merge {
            self.merge_t1(files, cmp, w, unique, &tmp)
        } else {
            self.merge_t2(files, cmp, w, unique, &tmp)
        }
    }
    /*
        /// given two file names, merge them into output
        pub fn merge_2(
            &self,
            left: &str,
            right: &str,
            cmp: &mut LineCompList,
            mut w: impl Write,
            unique: bool,
        ) -> Result<()> {
            let mut left_file = Reader::new2();
            let mut right_file = Reader::new2();
            left_file.open(left)?;
            right_file.open(right)?;
            left_file.do_split(false);
            right_file.do_split(false);
            cmp.lookup(&left_file.names())?;

            // FIXME -- Check Header
            if left_file.has_header() {
                w.write_all(left_file.header().line.as_bytes())?;
            }

            if unique {
                let mut prev: Vec<u8> = Vec::new();
                while !left_file.is_done() && !right_file.is_done() {
                    let ord = cmp.comp_lines(left_file.curr().line(), right_file.curr().line());
                    if ord == Ordering::Less {
                        left_file.write(&mut w)?;
                        mem::swap(&mut prev, left_file.curr_mut().raw());
                        left_file.getline()?;
                    } else if ord == Ordering::Greater {
                        right_file.write(&mut w)?;
                        mem::swap(&mut prev, left_file.curr_mut().raw());
                        right_file.getline()?;
                    } else {
                        left_file.write(&mut w)?;
                        mem::swap(&mut prev, left_file.curr_mut().raw());
                        left_file.getline()?;
                        right_file.getline()?;
                    }
                    while !left_file.is_done() && cmp.equal_lines(left_file.curr().line(), &prev) {
                        left_file.getline()?;
                    }
                    while !right_file.is_done() && cmp.equal_lines(right_file.curr().line(), &prev) {
                        right_file.getline()?;
                    }
                }
            } else {
                while !left_file.is_done() && !right_file.is_done() {
                    let ord = cmp.comp_lines(left_file.curr().line(), right_file.curr().line());
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
    */
    /// Sort all the files together, into w
    pub fn sort<W: Write>(
        &self,
        files: &[String],
        cmp: LineCompList,
        w: &mut W,
        unique: bool,
    ) -> Result<()> // maybe return some useful stats?
    {
        let mut s = Sorter::new(cmp, 500_000_000, unique);
        for fname in files {
            s.add_file(fname, w)?;
        }
        s.finalize(w)?;
        //    s.no_del();
        Ok(())
    }
}

/// Large block of text and pointers to lines therein
#[allow(missing_debug_implementations)]
pub struct Sorter {
    config: SortConfig,
    ptrs: Vec<Item>,
    cmp: LineCompList,
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

const MAX_DATA: usize = 0x0000_ffff_ff00;

impl Sorter {
    /// new Sorter
    #[must_use] pub fn new(cmp: LineCompList, max_alloc: usize, unique: bool) -> Self {
        let mut data_size = max_alloc / 2;
        if data_size > MAX_DATA {
            data_size = MAX_DATA;
        }
        let ptr_size = max_alloc / 2 / std::mem::size_of::<Item>();
        Self {
            config: SortConfig::default(),
            ptrs: Vec::with_capacity(ptr_size),
            data: Vec::with_capacity(data_size),
            cmp,
            tmp: TempDir::new().unwrap(), // FIXME - new should return Result
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
                self.cmp.fill_cache_line(&mut item, &self.data);
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
        if self.config.alt_sort {
            do_sort_lines(&self.data, &mut self.ptrs, &mut self.cmp);
        } else {
            self.ptrs
                .sort_by(|a, b| self.cmp.comp_items(&self.data, a, b));
        }
        if self.unique {
            self.ptrs
                .dedup_by(|a, b| self.cmp.equal_items(&self.data, a, b));
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
            self.config
                .merge_t(&self.tmp_files, &mut self.cmp, w, self.unique, &self.tmp)?;
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
            let s = make_header(&first_line);
            self.cmp.lookup(&s.vec())?;
            if is_cdx(&first_line) {
                w.write_all(&first_line)?;
            } else {
                self.add_data(&first_line)?;
            }
        }
        self.add(&mut *f)
    }
}

type NodeType = Box<MergeTreeItem>;
struct NodeData {
    left: NodeType,
    right: NodeType,
    left_data: Option<usize>,
    right_data: Option<usize>,
    //    done : bool, Optimization?
}
impl NodeData {
    fn new(left: NodeType, right: NodeType) -> Self {
        Self {
            left,
            right,
            left_data: None,
            right_data: None,
        }
    }
    fn left_cols<'a>(&self, files: &'a [Reader]) -> &'a TextLine {
        files[self.left_data.unwrap()].curr_line()
    }
    fn right_cols<'a>(&self, files: &'a [Reader]) -> &'a TextLine {
        files[self.right_data.unwrap()].curr_line()
    }
}
struct LeafData {
    file_num: usize,
    first: bool,
}

enum MergeTreeItem {
    Leaf(LeafData),
    Node(NodeData),
}

impl MergeTreeItem {
    fn new_tree(nums: &[usize]) -> Self {
        if nums.is_empty() {
            panic!("Can't make a MergeTreeItem from zero files")
        } else if nums.len() == 1 {
            Self::new_leaf(nums[0])
        } else {
            let mid = nums.len() / 2;
            Self::new_node(
                Box::new(Self::new_tree(&nums[..mid])),
                Box::new(Self::new_tree(&nums[mid..])),
            )
        }
    }
    fn new_node(left: NodeType, right: NodeType) -> Self {
        Self::Node(NodeData::new(left, right))
    }
    const fn new_leaf(r: usize) -> Self {
        Self::Leaf(LeafData {
            file_num: r,
            first: true,
        })
    }
    fn next(&mut self, cmp: &mut LineCompList, files: &mut [Reader]) -> Result<Option<usize>> {
        match self {
            Self::Leaf(r) => {
                if files[r.file_num].is_done() {
                    Ok(None)
                } else {
                    if r.first {
                        r.first = false;
                    } else if files[r.file_num].getline()? {
                        return Ok(None);
                    }
                    Ok(Some(r.file_num))
                }
            }
            Self::Node(n) => {
                if n.left_data.is_none() {
                    n.left_data = n.left.next(cmp, files)?;
                }
                if n.right_data.is_none() {
                    n.right_data = n.right.next(cmp, files)?;
                }
                if n.left_data.is_none() && n.right_data.is_none() {
                    Ok(None)
                } else if n.left_data.is_none() {
                    let tmp = n.right_data;
                    n.right_data = None;
                    Ok(tmp)
                } else if n.right_data.is_none() {
                    let tmp = n.left_data;
                    n.left_data = None;
                    Ok(tmp)
                } else {
                    let c = cmp.comp_cols(n.left_cols(files), n.right_cols(files));
                    if c == Ordering::Greater {
                        let tmp = n.right_data;
                        n.right_data = None;
                        Ok(tmp)
                    } else {
                        let tmp = n.left_data;
                        n.left_data = None;
                        Ok(tmp)
                    }
                }
            }
        }
    }
}

#[allow(dead_code)]
fn merge_lines(
    data: &[u8],
    dst: &mut [Item],
    mut low: &[Item],
    mut hi_start: usize,
    hi_end: usize,
    cmp: &mut LineCompList,
) {
    let mut dst_pos = 0;
    loop {
        if cmp.comp_items(data, &low[0], &dst[hi_start]) != Ordering::Greater {
            dst[dst_pos] = low[0];
            dst_pos += 1;
            low = &low[1..];
            if low.is_empty() {
                /* HI - NHI equalled T - (NLO + NHI) when this function
                began.  Therefore HI must equal T now, and there is no
                need to copy from HI to T.  */
                break;
            }
        } else {
            dst[dst_pos] = dst[hi_start];
            dst_pos += 1;
            hi_start += 1;
            if hi_start == hi_end {
                while !low.is_empty() {
                    dst[dst_pos] = low[0];
                    dst_pos += 1;
                    low = &low[1..];
                }
                break;
            }
        }
    }
}

#[allow(dead_code)]
fn sort_lines(data: &[u8], items: &mut [Item], temp: &mut [Item], cmp: &mut LineCompList) {
    if items.len() == 2 {
        if cmp.comp_items(data, &items[0], &items[1]) == Ordering::Greater {
            items.swap(0, 1);
        }
    } else {
        let low = items.len() / 2;
        sort_lines(data, &mut items[low..], temp, cmp);
        if low == 1 {
            temp[0] = items[0];
        } else {
            sort_lines_temp(data, &mut items[..low], temp, cmp);
        }
        merge_lines(data, items, &temp[..low], low, items.len(), cmp);
    }
}

// Like sort_lines but output into temp, rather than sorting in place
#[allow(dead_code)]
fn sort_lines_temp(data: &[u8], items: &mut [Item], temp: &mut [Item], cmp: &mut LineCompList) {
    if items.len() == 2 {
        if cmp.comp_items(data, &items[0], &items[1]) == Ordering::Greater {
            temp[0] = items[1];
            temp[1] = items[0];
        } else {
            temp[0] = items[0];
            temp[1] = items[1];
        }
    } else {
        let low = items.len() / 2;
        let items_len = items.len();
        sort_lines_temp(data, &mut items[low..], &mut temp[low..items_len], cmp);
        if low > 1 {
            sort_lines(data, &mut items[..low], temp, cmp);
        }
        merge_lines(data, temp, &items[..low], low, items_len, cmp);
    }
}

#[allow(dead_code)]
fn do_sort_lines(data: &[u8], items: &mut [Item], cmp: &mut LineCompList) {
    if items.len() > 1 {
        let nsize = items.len() / 2 + 1;
        let mut temp = Vec::with_capacity(nsize);
        temp.resize(nsize, Item::default());
        sort_lines(data, items, &mut temp, cmp);
    }
}
