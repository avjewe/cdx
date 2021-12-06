//! binary search in text files

use crate::comp::CompareList;
use crate::{is_cdx, Result, StringLine};
use memmap;
use std::cmp::Ordering;
use std::fs;

// lower - first that is not less
// upper - fist that is greater

/// binary search in text files
#[derive(Debug)]
pub struct MemMap {
    map: memmap::Mmap,
    header_len: usize,
    header: StringLine,
    delim: u8,
}

impl MemMap {
    /// create new read-only memory map to be searched
    pub fn new(fname: &str) -> Result<Self> {
        // needs delim as input if no CDX header
        let file = fs::File::open(fname)?;
        let map = unsafe { memmap::Mmap::map(&file)? };
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
    /// get underlying map
    pub fn get(&self) -> &[u8] {
        &self.map[..]
    }
    /// get underlying map
    pub const fn get_delim(&self) -> u8 {
        self.delim
    }
    /// does the file have a CDX header?
    pub const fn has_header(&self) -> bool {
        self.header_len > 0
    }
    /// full text of header line, including newline
    pub fn header(&self) -> &[u8] {
        self.header.line.as_bytes()
    }
    /// column names, suitable for lookup
    pub fn names(&self) -> Vec<&str> {
        self.header.vec()
    }
}

/// return the position after the next newline after start
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
/// that is, starting in the middle of a line, find the start
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
/// CompareList holds the value against which we are comparing
pub fn lower_bound<'a>(data: &'a [u8], comp: &CompareList) -> &'a [u8] {
    let (start, stop) = lower_bound_n(data, comp);
    &data[start..stop]
}

/// return first line that matches (with newline) or empty slice if none
/// CompareList holds the value against which we are comparing
pub fn equal_range<'a>(data: &'a [u8], comp: &CompareList) -> &'a [u8] {
    let (start, stop) = equal_range_n(data, comp);
    &data[start..stop]
}

/// return first line that matches (with newline) or empty slice if none
/// CompareList holds the value against which we are comparing
pub fn equal_range_n(data: &[u8], comp: &CompareList) -> (usize, usize) {
    let (start1, stop1) = lower_bound_n(data, comp);
    if start1 == stop1 {
        (start1, stop1)
    } else {
        (start1, upper_bound_n(data, comp))
    }
}

/// return first line that matches (with newline) or empty slice if none
/// CompareList holds the value against which we are comparing
pub fn lower_bound_n(data: &[u8], comp: &CompareList) -> (usize, usize) {
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
pub fn upper_bound_n(data: &[u8], comp: &CompareList) -> usize {
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
        let mut comp = crate::comp::CompareList::new();
        comp.push(crate::comp::make_comp("1").unwrap());
        comp.set("bbb", ',').unwrap();
        assert_eq!(lower_bound(b"aaa\nbbb\nccc\n", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"bbb\nccc\n", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nbbb\n", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nccc\n", &comp), b"");
        assert_eq!(lower_bound(b"", &comp), b"");
        assert_eq!(lower_bound(b"aaa\nbbb\nccc", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"bbb\nccc", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"aaa\nbbb", &comp), b"bbb");
        assert_eq!(lower_bound(b"aaa\nccc", &comp), b"");

        assert_eq!(
            lower_bound(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &comp),
            b"bbb\t1\n"
        );
        assert_eq!(
            lower_bound(b"bbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &comp),
            b"bbb\t1\n"
        );
        assert_eq!(
            lower_bound(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4", &comp),
            b"bbb\t1\n"
        );

        assert_eq!(
            equal_range(b"aaa\nbbb\nbbb\nbbb\nccc\n", &comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(
            equal_range(b"bbb\nbbb\nbbb\nccc\n", &comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(
            equal_range(b"aaa\nbbb\nbbb\nbbb\n", &comp),
            b"bbb\nbbb\nbbb\n"
        );
        assert_eq!(equal_range(b"", &comp), b"");
        assert_eq!(equal_range(b"aaa\n", &comp), b"");
        assert_eq!(equal_range(b"ccc\n", &comp), b"");
        assert_eq!(equal_range(b"aaa\nccc\n", &comp), b"");

        assert_eq!(upper_bound_n(b"aaa\nccc\n", &comp), 4);
        assert_eq!(
            lower_bound_n(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &comp),
            (4, 10)
        );
        assert_eq!(
            upper_bound_n(b"aaa\nbbb\t1\nbbb\t2\nbbb\t3\nbbb\t4\nccc\n", &comp),
            28
        );

        assert_eq!(lower_bound(b"\n\naaa\nbbb\nccc", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"\n\nbbb\nccc", &comp), b"bbb\n");
        assert_eq!(lower_bound(b"\n\naaa\nbbb", &comp), b"bbb");
        assert_eq!(lower_bound(b"\n\naaa\nccc", &comp), b"");
        assert_eq!(lower_bound(b"\n\n", &comp), b"");
        assert_eq!(lower_bound(b"\n", &comp), b"");
    }
}
