//! binary search in text files

use crate::prelude::*;
use crate::util::is_cdx;
use memmap2;
use std::cmp::Ordering;
use std::fs;

// lower - first that is not less
// upper - fist that is greater

/// memory map a file, extract CDX header
#[derive(Debug)]
pub struct MemMap {
    map: memmap2::Mmap,
    header_len: usize,
    header: StringLine,
    delim: u8,
}

impl MemMap {
    /// create new read-only memory map to be searched
    pub fn new(fname: &str) -> Result<Self> {
        // needs delim as input if no CDX header
        let file = fs::File::open(fname)?;
        let map = unsafe { memmap2::Mmap::map(&file)? };
        let mut delim = b'\t';
        let mut header_len = find_end(&map[..], 0);
        let mut header = StringLine::new();
        if is_cdx(&map[..]) {
            delim = map[4];
            header.line = std::str::from_utf8(&map[0..header_len])?.to_string();
        } else {
            let num_cols = map[0..header_len].split(|ch| *ch == delim).count();
            header_len = 0;
            header.fake(num_cols, delim);
        }
        header.split(delim);
        header.parts.remove(0);
        Ok(Self {
            map,
            header_len,
            header,
            delim,
        })
    }
    /// get underlying memmap data
    #[must_use]
    pub fn get(&self) -> &[u8] {
        &self.map[..]
    }
    /// get column delimiter
    #[must_use]
    pub const fn get_delim(&self) -> u8 {
        self.delim
    }
    /// does the file have a CDX header?
    #[must_use]
    pub const fn has_header(&self) -> bool {
        self.header_len > 0
    }
    /// full text of header line, including newline
    #[must_use]
    pub const fn header(&self) -> &[u8] {
        self.header.line.as_bytes()
    }
    /// column names, suitable for lookup
    #[must_use]
    pub fn names(&self) -> Vec<&str> {
        self.header.vec()
    }
}

/// return the position after the next newline after start
/// that is, starting in the middle of a line, find the start of the next line
#[must_use]
pub const fn find_end(data: &[u8], mut start: usize) -> usize {
    while (start < data.len()) && (data[start] != b'\n') {
        start += 1;
    }
    if start < data.len() {
        start += 1; // start is one past newline
    }
    start
}

/// return the position after the first newline before start
/// that is, starting in the middle of a line, find the start of it
#[must_use]
pub const fn find_start(data: &[u8], mut start: usize) -> usize {
    while (start > 0) && (data[start] != b'\n') {
        start -= 1;
    }
    if data[start] == b'\n' {
        start += 1;
    }
    start
}

/// 'start' should be the beginning of a line
/// return the beginning of the previous line
#[must_use]
pub const fn find_prev(data: &[u8], start: usize) -> usize {
    if start <= 1 {
        return 0;
    }
    if data[start - 1] != b'\n' {
        return find_start(data, start);
    }
    find_start(data, start - 2)
}

/// return first line that matches (with newline) or empty slice if none
/// `LineCompList` holds the value against which we are comparing
pub fn lower_bound<'a>(data: &'a [u8], comp: &mut LineCompList) -> &'a [u8] {
    let (start, stop) = lower_bound_n(data, comp);
    &data[start..stop]
}

/// return first line that matches (with newline) or empty slice if none
/// `LineCompList` holds the value against which we are comparing
pub fn equal_range<'a>(data: &'a [u8], comp: &mut LineCompList) -> &'a [u8] {
    let (start, stop) = equal_range_n(data, comp);
    &data[start..stop]
}

/// return first line that matches (with newline) or empty slice if none
/// `LineCompList` holds the value against which we are comparing
pub fn equal_range_n(data: &[u8], comp: &mut LineCompList) -> (usize, usize) {
    let (start1, stop1) = lower_bound_n(data, comp);
    if start1 == stop1 {
        (start1, stop1)
    } else {
        (start1, upper_bound_n(data, comp))
    }
}

/// return first line that matches (with newline) or empty slice if none
/// `LineCompList` holds the value against which we are comparing
pub fn lower_bound_n(data: &[u8], comp: &mut LineCompList) -> (usize, usize) {
    let mut trapped = false; // to exit a possible infinite loop
    let mut begin: usize = 0; // start of range under consideration
    let mut end: usize = data.len(); // end of range under consideration
    while begin < end {
        // find start and stop of a line in the middle
        let mut start = if trapped {
            begin
        } else {
            find_end(data, (end + begin - 1) / 2)
        };

        // if we hit end of buffer, back up from middle instead
        let stop = if start == end {
            let new_stop = start;
            start = find_start(data, (end + begin - 1) / 2);
            new_stop
        } else {
            find_end(data, start)
        };
        // data[start..stop] is one whole line, roughly in the middle of data[begin..end]
        match comp.comp_self_line(&data[start..stop]) {
            Ordering::Equal => {
                if start == begin {
                    return (start, stop);
                }
                trapped = stop == end;
                end = stop;
            }
            Ordering::Less => {
                end = start;
            }
            Ordering::Greater => {
                begin = stop;
            }
        };
    }
    (begin, end)
}

/// return start of first line that is greater than comp
pub fn upper_bound_n(data: &[u8], comp: &mut LineCompList) -> usize {
    let mut begin: usize = 0; // start of range under consideration
    let mut end: usize = data.len(); // end of range under consideration
    while begin < end {
        // find start and stop of a line in the middle
        let mut start = find_end(data, (end + begin - 1) / 2);

        // if we hit end of buffer, back up from middle instead
        let stop = if start == end {
            let new_stop = start;
            start = find_start(data, (end + begin - 1) / 2);
            new_stop
        } else {
            find_end(data, start)
        };
        // data[start..stop] is one whole line, roughly in the middle of data
        if comp.comp_self_line(&data[start..stop]) == Ordering::Less {
            end = start;
        } else {
            begin = stop;
        }
    }
    debug_assert!(begin == end);
    begin
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lower() {
        let mut comp = LineCompList::new();
        comp.push(crate::comp::CompMaker::make_line_comp("1").unwrap());
        comp.set(b"bbb", b',').unwrap();
        assert_eq!(lower_bound(b"aaa\nbbb\nccc\n", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"bbb\nccc\n", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nbbb\n", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nccc\n", &mut comp), b"");
        assert_eq!(lower_bound(b"", &mut comp), b"");
        assert_eq!(lower_bound(b"aaa\nbbb\nccc", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"bbb\nccc", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nbbb", &mut comp), b"bbb");
        assert_eq!(lower_bound(b"aaa\nccc", &mut comp), b"");

        assert_eq!(
            lower_bound(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &mut comp),
            b"bbb\t1\n"
        );
        assert_eq!(
            lower_bound(b"bbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &mut comp),
            b"bbb\t1\n"
        );
        assert_eq!(
            lower_bound(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4", &mut comp),
            b"bbb\t1\n"
        );
    }
    #[test]
    fn test_lower2() {
        let mut comp = LineCompList::new();
        comp.push(crate::comp::CompMaker::make_line_comp("1").unwrap());
        comp.set(b"bbb", b',').unwrap();
        assert_eq!(
            equal_range(b"aaa\nbbb\nbbb\nbbb\nccc\n", &mut comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(
            equal_range(b"bbb\nbbb\nbbb\nccc\n", &mut comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(
            equal_range(b"aaa\nbbb\nbbb\nbbb\n", &mut comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(equal_range(b"", &mut comp), b"");
        assert_eq!(equal_range(b"aaa\n", &mut comp), b"");
        assert_eq!(equal_range(b"ccc\n", &mut comp), b"");
        assert_eq!(equal_range(b"aaa\nccc\n", &mut comp), b"");

        assert_eq!(upper_bound_n(b"aaa\nccc\n", &mut comp), 4);
        assert_eq!(
            lower_bound_n(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &mut comp),
            (4, 10)
        );
        assert_eq!(
            upper_bound_n(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &mut comp),
            28
        );

        assert_eq!(lower_bound(b"\n\naaa\nbbb\nccc", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"\n\nbbb\nccc", &mut comp), b"bbb\n");
        assert_eq!(lower_bound(b"\n\naaa\nbbb", &mut comp), b"bbb");
        assert_eq!(lower_bound(b"\n\naaa\nccc", &mut comp), b"");
        assert_eq!(lower_bound(b"\n\n", &mut comp), b"");
        assert_eq!(lower_bound(b"\n", &mut comp), b"");
    }
}
