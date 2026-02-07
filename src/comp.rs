//! Tools for comparing lines and fields
//!
//!  For plain buffers, there is a trait [Compare], which is usually wrapped in a struct [Comp]
//!  which is usually aggregated into a [`CompList`]
//!
//!  For whole lines, there is a trait [`LineCompare`], which is usually wrapped in a struct [`LineComp`]
//!  which is usually aggregated into a [`LineCompList`]
//!
//! ```
//! use cdx::comp::LineCompList;
//! use cdx::prelude::*;
//! let mut comp = LineCompList::new();
//! comp.add("price,float")?;
//! comp.add("quant,num")?;
//! let input = "<< CDX\tname\tquant\tprice\naaa\t42\t1.23\nbbb\t12\t2.34\nccc\t12\t2.34\n";
//! let mut reader = Reader::new_with(2, &TextFileMode::default());
//! reader.open(input);
//! comp.lookup(&reader.names())?;
//! reader.getline()?;
//! reader.getline()?;
//! assert_eq!(comp.comp_cols(reader.curr_line(), reader.prev_line(1)), std::cmp::Ordering::Equal);
//! assert_eq!(comp.comp_cols(reader.prev_line(2), reader.prev_line(1)), std::cmp::Ordering::Less);
//! # Ok::<(), cdx::util::Error>(())
//! ```

use crate::column::get_col;
use crate::num::{Junk, JunkType, JunkVal, fcmp, ulp_to_ulong};
use crate::prelude::*;
use crate::util::prerr_n;
use std::sync::Mutex;

/// method of comparing two slices
pub trait Compare {
    /// Compare two slices, usually column values
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering;
    /// Compare two slices for equality
    fn equal(&self, left: &[u8], right: &[u8]) -> bool;
    /// set cache for this value
    fn fill_cache(&self, item: &mut Item, value: &[u8]);
    /// set my value
    fn set(&mut self, value: &[u8]);
    /// Compare self to slice
    fn comp_self(&self, right: &[u8]) -> Ordering;
    /// Compare self to slice for equality
    fn equal_self(&self, right: &[u8]) -> bool;
}

/// method of comparing two lines
pub trait LineCompare {
    /// compare lines
    fn comp_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering;
    /// which columns are in use for this file
    fn used_cols(&self, v: &mut Vec<usize>, file_num: usize);
    /// compare lines
    fn equal_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool;
    /// compare lines
    fn comp_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> Ordering;
    /// compare lines
    fn equal_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> bool;
    /// resolve named columns; illegal to call any of the others with a file that has not been looked up
    fn lookup(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()>;
    /// initialize columns in `TextLine`?
    fn need_split(&self) -> bool {
        true
    }
    /// set cache for this value
    fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine);
    /// set cache for this value
    fn fill_cache_line(&mut self, item: &mut Item, value: &[u8], delim: u8);
    /// set my value
    fn set(&mut self, value: &[u8]);
    /// Compare self to line
    fn comp_self_cols(&mut self, right: &TextLine) -> Ordering;
    /// Compare self to line for equality
    fn equal_self_cols(&mut self, right: &TextLine) -> bool;
    /// Compare self to line
    fn comp_self_line(&mut self, right: &[u8], delim: u8) -> Ordering;
    /// Compare self to line for equality
    fn equal_self_line(&mut self, right: &[u8], delim: u8) -> bool;
}

/// Settings for one Compare object
#[allow(missing_debug_implementations)]
pub struct Comp {
    /// junk allowed, responsibiltiy of inner Compare
    pub junk: Junk,
    /// optional arg to comparison, responsibiltiy of inner Compare
    pub pattern: String,

    /// type of comparison
    pub ctype: String,
    /// reverse comparison?
    pub reverse: bool,
    /// the thing that compares
    pub comp: Box<dyn Compare>,
}

impl fmt::Debug for Comp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Comp")
    }
}

impl Default for Comp {
    fn default() -> Self {
        Self {
            junk: Junk::default(),
            pattern: String::default(),
            ctype: String::default(),
            reverse: false,
            comp: Box::new(ComparePlain::new()),
        }
    }
}

impl Comp {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// new
    #[must_use]
    pub fn with_line_comp(c: &LineComp) -> Self {
        Self {
            junk: c.junk,
            pattern: c.pattern.clone(),
            ctype: c.ctype.clone(),
            reverse: c.reverse,
            comp: Box::new(ComparePlain::new()),
        }
    }
    /// Compare two slices, usually column values
    #[must_use]
    pub fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        self.comp.comp(left, right)
    }
    /// Compare two slices for equality
    #[must_use]
    pub fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        self.comp.equal(left, right)
    }
    /// set cache for this value
    pub fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        self.comp.fill_cache(item, value);
    }
    /// set my value
    pub fn set(&mut self, value: &[u8]) {
        self.comp.set(value);
    }
    /// Compare self to slice
    #[must_use]
    pub fn comp_self(&self, right: &[u8]) -> Ordering {
        self.comp.comp_self(right)
    }
    /// Compare self to slice for equality
    #[must_use]
    pub fn equal_self(&self, right: &[u8]) -> bool {
        self.comp.equal_self(right)
    }
}

/// Settings for one Compare object
#[allow(missing_debug_implementations)]
pub struct LineComp {
    /// junk allowed, responsibiltiy of inner `LineCompare`
    pub junk: Junk,
    /// optional arg to comparison, responsibiltiy of inner `LineCompare`
    pub pattern: String,
    /// period delimited list of columns, responsibiltiy of inner `LineCompare`
    pub cols: String,

    /// type of comparison
    pub ctype: String,
    /// reverse comparison?
    pub reverse: bool,
    /// column delimiter
    pub delim: u8,
    /// the thing that compares
    pub comp: Box<dyn LineCompare>,
}

impl fmt::Debug for LineComp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LineComp")
    }
}

impl Default for LineComp {
    fn default() -> Self {
        Self {
            junk: Junk::default(),
            pattern: String::default(),
            cols: String::default(),

            ctype: String::new(),
            reverse: false,
            delim: b'\t',
            comp: Box::new(LineCompWhole::new(Comp::default())),
        }
    }
}

impl LineComp {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// which columns are in use for this file
    pub fn used_cols(&self, v: &mut Vec<usize>, file_num: usize) {
        self.comp.used_cols(v, file_num);
    }
    /// Compare Items
    pub fn comp_items(&mut self, base: &[u8], left: &Item, right: &Item) -> Ordering {
        let x = if left.cache < right.cache {
            Ordering::Less
        } else if left.cache > right.cache {
            Ordering::Greater
        } else if left.complete() && right.complete() {
            Ordering::Equal
        } else {
            self.comp_lines(left.get(base), right.get(base))
        };
        self.reverse(x)
    }
    /// Compare Items
    pub fn equal_items(&mut self, base: &[u8], left: &Item, right: &Item) -> bool {
        if left.complete() && right.complete() {
            left.cache == right.cache
        } else {
            self.equal_lines(left.get(base), right.get(base))
        }
    }
    /// reverse the ordering if self.reverse is set
    #[must_use]
    pub const fn reverse(&self, x: Ordering) -> Ordering {
        if self.reverse { x.reverse() } else { x }
    }
    /// compare [`TextLine`]s from a single file
    pub fn comp_cols(&mut self, left: &TextLine, right: &TextLine) -> Ordering {
        let x = self.comp.comp_cols(left, right, 0, 0);
        self.reverse(x)
    }
    /// compare [`TextLine`]s from multiple files
    pub fn comp_cols_n(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        let x = self.comp.comp_cols(left, right, left_file, right_file);
        self.reverse(x)
    }
    /// compare [`TextLine`]s from a single file
    pub fn equal_cols(&mut self, left: &TextLine, right: &TextLine) -> bool {
        self.comp.equal_cols(left, right, 0, 0)
    }
    /// compare [`TextLine`]s from multiple files
    pub fn equal_cols_n(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal_cols(left, right, left_file, right_file)
    }
    /// compare line from a single file
    pub fn comp_lines(&mut self, left: &[u8], right: &[u8]) -> Ordering {
        let x = self.comp.comp_lines(left, right, self.delim, 0, 0);
        self.reverse(x)
    }
    /// compare lines from multiple files
    pub fn comp_lines_n(
        &mut self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        let x = self.comp.comp_lines(left, right, self.delim, left_file, right_file);
        self.reverse(x)
    }
    /// compare line from a single file
    pub fn equal_lines(&mut self, left: &[u8], right: &[u8]) -> bool {
        self.comp.equal_lines(left, right, self.delim, 0, 0)
    }
    /// compare lines from multiple files
    pub fn equal_lines_n(
        &mut self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal_lines(left, right, self.delim, left_file, right_file)
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.comp.lookup(fieldnames, 0)
    }
    /// resolve named columns for the given file
    pub fn lookup_n(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()> {
        self.comp.lookup(fieldnames, file_num)
    }
    /// do [`TextLine`]s need their columns to be initialized
    #[must_use]
    pub fn need_split(&self) -> bool {
        self.comp.need_split()
    }
    /// set cache of [Item] from [`TextLine`]
    pub fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache_cols(item, value);
    }
    /// set cache of [Item] from line
    pub fn fill_cache_line(&mut self, item: &mut Item, value: &[u8]) {
        self.comp.fill_cache_line(item, value, self.delim);
    }
    /// set value for later comparison
    pub fn set(&mut self, value: &[u8]) {
        self.comp.set(value);
    }
    /// compare self to this line
    pub fn comp_self_cols(&mut self, right: &TextLine) -> Ordering {
        let x = self.comp.comp_self_cols(right);
        self.reverse(x)
    }
    /// compare self to this line
    pub fn equal_self_cols(&mut self, right: &TextLine) -> bool {
        self.comp.equal_self_cols(right)
    }
    /// compare self to this line
    pub fn comp_self_line(&mut self, right: &[u8]) -> Ordering {
        let x = self.comp.comp_self_line(right, self.delim);
        self.reverse(x)
    }
    /// compare self to this line
    pub fn equal_self_line(&mut self, right: &[u8]) -> bool {
        self.comp.equal_self_line(right, self.delim)
    }
}

/// compare the whole line with the [Compare]
struct LineCompWhole {
    comp: Comp,
}

impl LineCompWhole {
    const fn new(comp: Comp) -> Self {
        Self { comp }
    }
}

impl LineCompare for LineCompWhole {
    fn used_cols(&self, _v: &mut Vec<usize>, _file_num: usize) {}
    /// compare lines
    fn comp_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        _left_file: usize,
        _right_file: usize,
    ) -> Ordering {
        self.comp.comp(left.line(), right.line())
    }
    /// compare lines
    fn equal_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        _left_file: usize,
        _right_file: usize,
    ) -> bool {
        self.comp.equal(left.line(), right.line())
    }
    /// compare lines
    fn comp_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        _delim: u8,
        _left_file: usize,
        _right_file: usize,
    ) -> Ordering {
        self.comp.comp(left, right)
    }
    /// compare lines
    fn equal_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        _delim: u8,
        _left_file: usize,
        _right_file: usize,
    ) -> bool {
        self.comp.equal(left, right)
    }
    /// resolve named columns; illegal to call any of the others with a file that has not been looked up
    fn lookup(&mut self, _fieldnames: &[&str], _file_num: usize) -> Result<()> {
        Ok(())
    }
    fn need_split(&self) -> bool {
        false
    }

    fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache(item, value.line());
    }

    fn fill_cache_line(&mut self, item: &mut Item, value: &[u8], _delim: u8) {
        self.comp.fill_cache(item, value);
    }

    fn set(&mut self, value: &[u8]) {
        self.comp.set(value);
    }

    fn comp_self_cols(&mut self, right: &TextLine) -> Ordering {
        self.comp.comp_self(right.line())
    }

    fn equal_self_cols(&mut self, right: &TextLine) -> bool {
        self.comp.equal_self(right.line())
    }

    fn comp_self_line(&mut self, right: &[u8], _delim: u8) -> Ordering {
        self.comp.comp_self(right)
    }

    fn equal_self_line(&mut self, right: &[u8], _delim: u8) -> bool {
        self.comp.equal_self(right)
    }
}

/// compare the whole line with the [Compare]
struct LineCompCol {
    cols: Vec<NamedCol>,
    comp: Comp,
}

impl LineCompCol {
    /// cols must not be empty, use `LineCompWhole` for that
    fn new(comp: Comp, spec: &str) -> Result<Self> {
        let mut cols = Vec::new();
        for x in spec.split('.') {
            cols.push(NamedCol::new_from(x)?);
        }
        Ok(Self { cols, comp })
    }
}

impl LineCompare for LineCompCol {
    fn used_cols(&self, v: &mut Vec<usize>, file_num: usize) {
        v.push(self.cols[file_num].num);
    }
    /// compare lines
    fn comp_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        self.comp.comp(left.get(self.cols[left_file].num), right.get(self.cols[right_file].num))
    }
    /// compare lines
    fn equal_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal(left.get(self.cols[left_file].num), right.get(self.cols[right_file].num))
    }
    /// compare lines
    fn comp_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        self.comp.comp(
            get_col(left, self.cols[left_file].num, delim),
            get_col(right, self.cols[right_file].num, delim),
        )
    }
    /// compare lines
    fn equal_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal(
            get_col(left, self.cols[left_file].num, delim),
            get_col(right, self.cols[right_file].num, delim),
        )
    }
    /// resolve named columns; illegal to call any of the others with a file that has not been looked up
    fn lookup(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()> {
        while self.cols.len() < (file_num + 1) {
            self.cols.push(self.cols[self.cols.len() - 1].clone());
        }
        self.cols[file_num].lookup(fieldnames)
    }
    fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache(item, value.get(self.cols[0].num));
    }

    fn fill_cache_line(&mut self, item: &mut Item, value: &[u8], delim: u8) {
        self.comp.fill_cache(item, get_col(item.get(value), self.cols[0].num, delim));
    }

    fn set(&mut self, value: &[u8]) {
        self.comp.set(value);
    }

    fn comp_self_cols(&mut self, right: &TextLine) -> Ordering {
        self.comp.comp_self(right.get(self.cols[0].num))
    }

    fn equal_self_cols(&mut self, right: &TextLine) -> bool {
        self.comp.equal_self(right.get(self.cols[0].num))
    }

    fn comp_self_line(&mut self, right: &[u8], delim: u8) -> Ordering {
        self.comp.comp_self(get_col(right, self.cols[0].num, delim))
    }

    fn equal_self_line(&mut self, right: &[u8], delim: u8) -> bool {
        self.comp.equal_self(get_col(right, self.cols[0].num, delim))
    }
}

/*
    /// Compare lines
    fn comp_items(&self, base: &[u8], left: &Item, right: &Item) -> Ordering {
        if left.cache < right.cache {
            Ordering::Less
        } else if left.cache > right.cache {
            Ordering::Greater
        } else if left.complete() && right.complete() {
            Ordering::Equal
        } else {
            self.comp_lines(left.get(base), right.get(base))
        }
    }
    /// Compare lines
    fn equal_items(&self, base: &[u8], left: &Item, right: &Item) -> bool {
        if left.cache != right.cache {
            return false;
        }
        if left.complete() && right.complete() {
            return true;
        }
        self.equal_lines(left.get(base), right.get(base))
    }
}
*/

/// make fixed length array from slice
fn make_array(val: &[u8]) -> [u8; 8] {
    let mut res = [0; 8];
    match val.len() {
        0 => {}
        1 => res[..1].copy_from_slice(val),
        2 => res[..2].copy_from_slice(val),
        3 => res[..3].copy_from_slice(val),
        4 => res[..4].copy_from_slice(val),
        5 => res[..5].copy_from_slice(val),
        6 => res[..6].copy_from_slice(val),
        7 => res[..7].copy_from_slice(val),
        _ => res[..8].copy_from_slice(&val[..8]),
        //	_ => val[..8].try_into().unwrap(),
    }
    res
}
/*
fn make_array(val: &[u8]) -> [u8; 8] {
    let mut result = [0; 8];
    let len = std::cmp::min(val.len(), 8);
    result[..len].copy_from_slice(&val[..len]);
    result
}
*/

fn skip_comma_zero(mut a: &[u8]) -> &[u8] {
    while !a.is_empty() && (a[0] == b'0' || a[0] == b',') {
        a = &a[1..];
    }
    a
}

fn skip_comma(mut a: &[u8]) -> &[u8] {
    while !a.is_empty() && a[0] == b',' {
        a = &a[1..];
    }
    a
}

fn frac_cmp(mut a: &[u8], mut b: &[u8]) -> Ordering {
    debug_assert!(!a.is_empty());
    debug_assert!(!b.is_empty());
    debug_assert!(a[0] == b'.');
    debug_assert!(b[0] == b'.');
    a = &a[1..];
    b = &b[1..];
    while !a.is_empty() && !b.is_empty() && (a[0] == b[0]) && a[0].is_ascii_digit() {
        a = &a[1..];
        b = &b[1..];
    }
    if a.is_empty() {
        if b.is_empty() || !b[0].is_ascii_digit() { Ordering::Equal } else { Ordering::Less }
    } else if b.is_empty() {
        if a[0].is_ascii_digit() { Ordering::Greater } else { Ordering::Equal }
    } else if a[0].is_ascii_digit() {
        if b[0].is_ascii_digit() { a[0].cmp(&b[0]) } else { Ordering::Greater }
    } else if b[0].is_ascii_digit() {
        Ordering::Less
    } else {
        Ordering::Equal
    }
}

fn num_cmp_unsigned(mut a: &[u8], mut b: &[u8]) -> Ordering {
    a = skip_comma_zero(a);
    b = skip_comma_zero(b);
    while !a.is_empty() && !b.is_empty() && (a[0] == b[0]) && a[0].is_ascii_digit() {
        a = &a[1..];
        b = &b[1..];
        a = skip_comma(a);
        b = skip_comma(b);
    }
    if a.is_empty() && b.is_empty() {
        return Ordering::Equal;
    }
    if a.is_empty() {
        if b[0].is_ascii_digit() {
            return Ordering::Less;
        }
        return Ordering::Equal;
    }
    if b.is_empty() {
        if a[0].is_ascii_digit() {
            return Ordering::Greater;
        }
        return Ordering::Equal;
    }
    if a[0] == b'.' && b[0] == b'.' {
        return frac_cmp(a, b);
    }
    let tmp = a[0].cmp(&b[0]);

    let mut log_a = 0;
    while !a.is_empty() && a[0].is_ascii_digit() {
        log_a += 1;
        a = &a[1..];
        a = skip_comma(a);
    }

    let mut log_b = 0;
    while !b.is_empty() && b[0].is_ascii_digit() {
        log_b += 1;
        b = &b[1..];
        b = skip_comma(b);
    }

    if log_a != log_b {
        return log_a.cmp(&log_b);
    }
    if log_a == 0 {
        return Ordering::Equal;
    }
    tmp
}

fn num_cmp_signed(mut a: &[u8], mut b: &[u8]) -> Ordering {
    a = a.trimw_start();
    b = b.trimw_start();

    let left_minus = if !a.is_empty() && a[0] == b'-' {
        a = &a[1..];
        true
    } else {
        false
    };

    let right_minus = if !b.is_empty() && b[0] == b'-' {
        b = &b[1..];
        true
    } else {
        false
    };

    if left_minus {
        if right_minus {
            return num_cmp_unsigned(b, a);
        }
        return Ordering::Less;
    }
    if right_minus {
        return Ordering::Greater;
    }
    num_cmp_unsigned(a, b)
}

fn ip_cmp(mut a: &[u8], mut b: &[u8]) -> Ordering {
    a = a.trimw_start();
    b = b.trimw_start();

    loop {
        let mut a2: usize = 0;
        while a.len() > a2 && a[a2].is_ascii_digit() {
            a2 += 1;
        }
        let mut b2: usize = 0;
        while b.len() > b2 && b[b2].is_ascii_digit() {
            b2 += 1;
        }
        if a2 != b2 {
            return a2.cmp(&b2);
        }
        for x in 0..a2 {
            if a[x] != b[x] {
                return a[x].cmp(&b[x]);
            }
        }
        let a_dot = a.len() > a2 && a[a2] == b'.';
        let b_dot = b.len() > b2 && b[b2] == b'.';
        if !a_dot && !b_dot {
            return Ordering::Equal;
        }
        if !a_dot {
            return Ordering::Less;
        }
        if !b_dot {
            return Ordering::Greater;
        }
        a = &a[a2 + 1..];
        b = &b[b2 + 1..];
    }
}

/// One line in a buffer of text, suitable for sorting
#[derive(Debug, Clone, Copy)]
pub struct Item {
    /// offset from beginning of buffer
    pub offset: u32,
    /// length of line, including newline, with high bit reserved
    pub size_plus: u32,
    /// Compare caches, and then if necessary compare actual data
    pub cache: u64,
}
const _: () = assert!(size_of::<Item>() == 16);

/*
    Reverse, // compare right to left
    LowerUtf, // unicode lowercase
    NormUtf, // unicode normalized
    LowerNormUtf, // unicode lowercase normalized
    Human,  // 2.3K or 4.5G
    Fuzzy(u32 n), // integer compare, but equal if within n
    Date(String fmt), // formatted date compare
    Url, //. Compare backwards from first slash, then forwards from first slash
    Prefix, // columns are equal if either is a prefix of the other
    Enum (JAN,FEB,MAR,...)
*/

type MakerBox = Box<dyn Fn(&Comp) -> Result<Box<dyn Compare>> + Send>;
/// A named constructor for a [Compare], used by [`CompMaker`]
struct CompMakerItem {
    /// matched against `Comparator::ctype`
    tag: &'static str,
    /// what this matcher does
    help: &'static str,
    /// Create a dyn Match from a pattern
    maker: MakerBox,
}

type LineMakerBox = Box<dyn Fn(&LineComp) -> Result<Box<dyn LineCompare>> + Send>;
/// A named constructor for a [`LineCompare`], used by [`CompMaker`]
struct LineCompMakerItem {
    /// matched against `Comparator::ctype`
    tag: &'static str,
    /// what this matcher does
    help: &'static str,
    /// Create a dyn Match from a pattern
    maker: LineMakerBox,
}

struct CompMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

static COMP_MAKER: Mutex<Vec<CompMakerItem>> = Mutex::new(Vec::new());
static LINE_MAKER: Mutex<Vec<LineCompMakerItem>> = Mutex::new(Vec::new());
static COMP_ALIAS: Mutex<Vec<CompMakerAlias>> = Mutex::new(Vec::new());
const MODIFIERS: &[&str] = &["rev", "strict", "trail", "low"];

/// Makes a [Compare or `LineComp`]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
pub struct CompMaker {}

impl CompMaker {
    /// add standard match makers
    fn init() -> Result<()> {
        if !COMP_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("numeric", "num")?;
        Self::do_add_alias("length", "len")?;
        Self::do_add_alias("plain", "")?;
        Self::do_push_line("expr", "Sort by value of expr", |p| {
            Ok(Box::new(LineCompExpr::new(&p.pattern)?))
        })?;
        Self::do_push("length", "Sort by length of string", |_p| Ok(Box::new(CompareLen::new())))?;
        Self::do_push("random", "Sort randomly", |_p| Ok(Box::new(CompareRandom::new())))?;
        Self::do_push("ip", "Sort as IP address or 1.2.3 section numbers", |_p| {
            Ok(Box::new(CompareIP::new()))
        })?;
        Self::do_push("plain", "Sort the plain bytes", |_p| Ok(Box::new(ComparePlain::new())))?;
        Self::do_push("lower", "Sort as the ascii lowercase of the string", |_p| {
            Ok(Box::new(CompareLower::new()))
        })?;
        Self::do_push("float", "Convert to floating point, and sort the result.", |_p| {
            Ok(Box::new(Comparef64::new()))
        })?;
        Self::do_push("numeric", "Convert NNN.nnn of arbitrary length.", |_p| {
            Ok(Box::new(CompareNumeric::new()))
        })?;
        Self::do_push("equal", "Everything always compares equal.", |_p| {
            Ok(Box::new(CompareEqual {}))
        })?;
        Ok(())
    }
    /// Add a new Compare. If a Compare already exists by that name, replace it.
    pub fn push<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&Comp) -> Result<Box<dyn Compare>> + Send + 'static,
    {
        Self::init()?;
        Self::do_push(tag, help, maker)
    }
    /// Add a new `LineCompare`. If a Compare already exists by that name, replace it.
    pub fn push_line<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&LineComp) -> Result<Box<dyn LineCompare>> + Send + 'static,
    {
        Self::init()?;
        Self::do_push_line(tag, help, maker)
    }
    /// Add a new alias. If an alias already exists by that name, replace it.
    pub fn add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        Self::init()?;
        Self::do_add_alias(old_name, new_name)
    }
    /// Return name, replaced by its alias, if any.
    fn resolve_alias(name: &str) -> &str {
        // let mut mm = COMP_ALIAS.lock().unwrap();
        for x in COMP_ALIAS.lock().unwrap().iter_mut() {
            if x.new_name == name {
                return x.old_name;
            }
        }
        name
    }
    fn do_add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        if MODIFIERS.contains(&new_name) {
            return err!(
                "You can't add an alias named {new_name} because that is reserved for a modifier"
            );
        }
        let m = CompMakerAlias { old_name, new_name };
        let mut mm = COMP_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == m.new_name {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        drop(mm);
        Ok(())
    }
    fn do_push<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&Comp) -> Result<Box<dyn Compare>> + Send + 'static,
    {
        if MODIFIERS.contains(&tag) {
            return err!(
                "You can't add a matcher named {tag} because that is reserved for a modifier"
            );
        }
        let m = CompMakerItem { tag, help, maker: Box::new(maker) };
        let mut mm = COMP_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        drop(mm);
        Ok(())
    }
    fn do_push_line<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&LineComp) -> Result<Box<dyn LineCompare>> + Send + 'static,
    {
        if MODIFIERS.contains(&tag) {
            return err!(
                "You can't add a matcher named {tag} because that is reserved for a modifier"
            );
        }
        let m = LineCompMakerItem { tag, help, maker: Box::new(maker) };
        let mut mm = LINE_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        drop(mm);
        Ok(())
    }
    /// Print all available Matchers to stdout.
    pub fn help() {
        println!("Modifers :");
        println!("rev     reverse to ordering");
        println!("strict  compare as junk if not exactly right");
        println!("trail   compare as junk if no leading goodness");
        println!("low     junk should compare low, rather than high");
        println!("Methods :");
        Self::init().unwrap();
        let mut results = Vec::new();
        for x in &*COMP_MAKER.lock().unwrap() {
            results.push(format!("{:12}{}", x.tag, x.help));
        }
        for x in &*LINE_MAKER.lock().unwrap() {
            results.push(format!("{:12}{}", x.tag, x.help));
        }
        results.sort();
        for x in results {
            println!("{x}");
        }
        println!("See also https://avjewe.github.io/cdxdoc/Comparator.html.");
    }
    /// create Box<dyn Compare> from spec
    pub fn make_comp_box(spec: &str) -> Result<Box<dyn Compare>> {
        Ok(Self::make_comp(spec)?.comp)
    }
    /// create Box<dyn LineCompare> from spec
    pub fn make_line_comp_box(spec: &str) -> Result<Box<dyn LineCompare>> {
        Ok(Self::make_line_comp(spec)?.comp)
    }
    /// create Comp from spec
    pub fn make_comp(spec: &str) -> Result<Comp> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::make_comp_parts(a, b)
        } else {
            Self::make_comp_parts(spec, "")
        }
    }
    /// create Comp from method and pattern
    pub fn make_comp_parts(method: &str, pattern: &str) -> Result<Comp> {
        let mut comp = Comp::new();
        comp.pattern = pattern.to_string();
        if !method.is_empty() {
            for x in method.split('.') {
                if x.eq_ignore_ascii_case("rev") {
                    comp.reverse = true;
                } else if x.eq_ignore_ascii_case("strict") {
                    comp.junk.junk_type = JunkType::None;
                } else if x.eq_ignore_ascii_case("trail") {
                    comp.junk.junk_type = JunkType::Trailing;
                } else if x.eq_ignore_ascii_case("low") {
                    comp.junk.junk_val = JunkVal::Min;
                } else {
                    comp.ctype = x.to_string();
                }
            }
        }
        Self::remake_comp(&mut comp)?;
        Ok(comp)
    }
    /// create `LineComp` from spec
    pub fn make_line_comp(spec: &str) -> Result<LineComp> {
        if let Some((a, b)) = spec.split_once(',') {
            if let Some((c, d)) = b.split_once(',') {
                Self::make_line_comp_parts(a, c, d)
            } else {
                Self::make_line_comp_parts(a, b, "")
            }
        } else {
            Self::make_line_comp_parts(spec, "", "")
        }
    }
    /// create `LineComp` from columns, method and pattern
    pub fn make_line_comp_parts(cols: &str, method: &str, pattern: &str) -> Result<LineComp> {
        let mut comp = LineComp::new();
        comp.pattern = pattern.to_string();
        comp.cols = cols.to_string();
        if !method.is_empty() {
            for x in method.split('.') {
                if x.eq_ignore_ascii_case("rev") {
                    comp.reverse = true;
                } else if x.eq_ignore_ascii_case("strict") {
                    comp.junk.junk_type = JunkType::None;
                } else if x.eq_ignore_ascii_case("trail") {
                    comp.junk.junk_type = JunkType::Trailing;
                } else if x.eq_ignore_ascii_case("low") {
                    comp.junk.junk_val = JunkVal::Min;
                } else {
                    comp.ctype = x.to_string();
                }
            }
        }
        Self::remake_line_comp(&mut comp)?;
        Ok(comp)
    }
    /// reset the Compare inside the Comp
    pub fn remake_comp(comp: &mut Comp) -> Result<()> {
        Self::init()?;
        let ctype = Self::resolve_alias(&comp.ctype);
        for x in &*COMP_MAKER.lock().unwrap() {
            if ctype.eq_ignore_ascii_case(x.tag) {
                comp.comp = (x.maker)(comp)?;
                return Ok(());
            }
        }
        err!("No such compare type : '{}'", comp.ctype)
    }
    /// reset the `LineCompare` inside the `LineComp`
    pub fn remake_line_comp(comp: &mut LineComp) -> Result<()> {
        Self::init()?;
        let ctype = Self::resolve_alias(&comp.ctype);
        for x in &*LINE_MAKER.lock().unwrap() {
            if ctype.eq_ignore_ascii_case(x.tag) {
                comp.comp = (x.maker)(comp)?;
                return Ok(());
            }
        }
        let mut new_comp = Comp::with_line_comp(comp);
        for x in &*COMP_MAKER.lock().unwrap() {
            if ctype.eq_ignore_ascii_case(x.tag) {
                new_comp.comp = (x.maker)(&new_comp)?;
                if comp.cols.is_empty() {
                    comp.comp = Box::new(LineCompWhole::new(new_comp));
                } else {
                    comp.comp = Box::new(LineCompCol::new(new_comp, &comp.cols)?);
                }
                return Ok(());
            }
        }
        err!("No such compare type : '{}'", comp.ctype)
    }
}

#[derive(Default, Debug)]
/// Ordered list of [Comp]
pub struct CompList {
    c: Vec<Comp>,
}
#[derive(Default, Debug)]
/// Ordered list of [`LineComp`]
pub struct LineCompList {
    c: Vec<LineComp>,
    value: Vec<u8>,
}

impl CompList {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// any [Comp]s in the list?
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.c.is_empty()
    }
    /// add
    pub fn push(&mut self, x: Comp) {
        self.c.push(x);
    }
    /// add
    pub fn add(&mut self, x: &str) -> Result<()> {
        self.c.push(CompMaker::make_comp(x)?);
        Ok(())
    }
    /// Compare two slices, usually column values
    #[must_use]
    pub fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        for x in &self.c {
            let ret = x.comp(left, right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// Compare two slices for equality
    #[must_use]
    pub fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        for x in &self.c {
            if !x.comp.equal(left, right) {
                return false;
            }
        }
        true
    }
    /// set cache for this value
    pub fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        if !self.c.is_empty() {
            self.c[0].fill_cache(item, value);
        }
    }
    /// set my value
    pub fn set(&mut self, value: &[u8], delim: u8) -> Result<()> {
        if self.c.len() == 1 {
            self.c[0].set(value);
        } else {
            let values: Vec<&[u8]> = value.split(|ch| *ch == delim).collect();
            if values.len() != self.c.len() {
                return err!(
                    "Tried to use a {} part value for a {} part Comparison",
                    values.len(),
                    self.c.len()
                );
            }
            for (n, x) in self.c.iter_mut().enumerate() {
                x.set(values[n]);
            }
        }
        Ok(())
    }
    /// Compare self to slice
    #[must_use]
    pub fn comp_self(&self, right: &[u8]) -> Ordering {
        for x in &self.c {
            let ret = x.comp_self(right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// Compare self to slice for equality
    #[must_use]
    pub fn equal_self(&self, right: &[u8]) -> bool {
        for x in &self.c {
            if !x.equal_self(right) {
                return false;
            }
        }
        true
    }
}

impl LineCompList {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// any [`LineComp`]s in the list?
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.c.is_empty()
    }
    /// which columns used as part of key?
    #[must_use]
    pub fn used_cols(&self, file_num: usize) -> Vec<usize> {
        let mut v = Vec::new();
        for x in &self.c {
            x.used_cols(&mut v, file_num);
        }
        v
    }
    /// add
    pub fn add(&mut self, x: &str) -> Result<()> {
        self.c.push(CompMaker::make_line_comp(x)?);
        Ok(())
    }
    /// add
    pub fn push(&mut self, x: LineComp) {
        self.c.push(x);
    }
    /// compare [Item]s
    pub fn comp_items(&mut self, base: &[u8], left: &Item, right: &Item) -> Ordering {
        if self.c.is_empty() {
            return Ordering::Equal;
        }
        let ret = self.c[0].comp_items(base, left, right);
        if ret != Ordering::Equal {
            return ret;
        }
        for x in self.c.iter_mut().skip(1) {
            let ret = x.comp_lines(left.get(base), right.get(base));
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare [Item]s
    pub fn equal_items(&mut self, base: &[u8], left: &Item, right: &Item) -> bool {
        if self.c.is_empty() {
            return true;
        }
        if !self.c[0].equal_items(base, left, right) {
            return false;
        }
        for x in self.c.iter_mut().skip(1) {
            if !x.equal_lines(left.get(base), right.get(base)) {
                return false;
            }
        }
        true
    }
    /// compare [`TextLine`]s in the same file
    pub fn comp_cols(&mut self, left: &TextLine, right: &TextLine) -> Ordering {
        self.comp_cols_n(left, right, 0, 0)
    }
    /// compare [`TextLine`]s in different files
    pub fn comp_cols_n(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        for x in &mut self.c {
            let ret = x.comp_cols_n(left, right, left_file, right_file);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare [`TextLine`]s in the same file
    pub fn equal_cols(&mut self, left: &TextLine, right: &TextLine) -> bool {
        self.equal_cols_n(left, right, 0, 0)
    }
    /// compare [`TextLine`]s in different files
    pub fn equal_cols_n(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        for x in &mut self.c {
            if !x.equal_cols_n(left, right, left_file, right_file) {
                return false;
            }
        }
        true
    }
    /// compare liness from the same
    pub fn comp_lines(&mut self, left: &[u8], right: &[u8]) -> Ordering {
        self.comp_lines_n(left, right, 0, 0)
    }
    /// compare lines from different files
    pub fn comp_lines_n(
        &mut self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        for x in &mut self.c {
            let ret = x.comp_lines_n(left, right, left_file, right_file);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare lines from the same file
    pub fn equal_lines(&mut self, left: &[u8], right: &[u8]) -> bool {
        self.equal_lines_n(left, right, 0, 0)
    }
    /// compare lines from different files
    pub fn equal_lines_n(
        &mut self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> bool {
        for x in &mut self.c {
            if !x.equal_lines_n(left, right, left_file, right_file) {
                return false;
            }
        }
        true
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.lookup_n(fieldnames, 0)
    }
    /// resolve named columns in the given file
    pub fn lookup_n(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()> {
        for x in &mut self.c {
            x.lookup_n(fieldnames, file_num)?;
        }
        Ok(())
    }
    /// do [`TextLine`]s need their columns initialized
    #[must_use]
    pub fn need_split(&self) -> bool {
        for x in &self.c {
            if x.need_split() {
                return true;
            }
        }
        false
    }
    /// fill [Item]'s cache
    pub fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine) {
        if !self.c.is_empty() {
            self.c[0].fill_cache_cols(item, value);
        }
    }
    /// fill [Item]'s cache
    pub fn fill_cache_line(&mut self, item: &mut Item, value: &[u8]) {
        if !self.c.is_empty() {
            self.c[0].fill_cache_line(item, value);
        }
    }
    /// get the value previously set
    #[must_use]
    pub fn get_value(&self) -> &[u8] {
        &self.value
    }
    /// set value fo later comparison
    pub fn set(&mut self, value: &[u8], delim: u8) -> Result<()> {
        value.clone_into(&mut self.value);
        if self.c.len() == 1 {
            self.c[0].set(value);
        } else {
            let values: Vec<&[u8]> = value.split(|ch| *ch == delim).collect();
            if values.len() != self.c.len() {
                return err!(
                    "Tried to use a {} part value for a {} part Comparison",
                    values.len(),
                    self.c.len()
                );
            }
            for (n, x) in self.c.iter_mut().enumerate() {
                x.set(values[n]);
            }
        }
        Ok(())
    }
    /// compare my value to this line
    pub fn comp_self_cols(&mut self, right: &TextLine) -> Ordering {
        for x in &mut self.c {
            let ret = x.comp_self_cols(right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare my value to this line
    pub fn equal_self_cols(&mut self, right: &TextLine) -> bool {
        for x in &mut self.c {
            if !x.equal_self_cols(right) {
                return false;
            }
        }
        true
    }
    /// compare my value to this line
    pub fn comp_self_line(&mut self, right: &[u8]) -> Ordering {
        for x in &mut self.c {
            let ret = x.comp_self_line(right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare my value to this line
    pub fn equal_self_line(&mut self, right: &[u8]) -> bool {
        for x in &mut self.c {
            if !x.equal_self_line(right) {
                return false;
            }
        }
        true
    }
}

#[derive(Default, Debug)]
struct CompareRandom {}
impl CompareRandom {
    fn new() -> Self {
        Self::default()
    }
    fn ord() -> Ordering {
        if fastrand::bool() { Ordering::Less } else { Ordering::Greater }
    }
}
impl Compare for CompareRandom {
    fn comp(&self, _left: &[u8], _right: &[u8]) -> Ordering {
        Self::ord()
    }
    fn equal(&self, _left: &[u8], _right: &[u8]) -> bool {
        fastrand::bool()
    }
    fn fill_cache(&self, _item: &mut Item, _value: &[u8]) {}
    fn set(&mut self, _value: &[u8]) {}
    fn comp_self(&self, _right: &[u8]) -> Ordering {
        Self::ord()
    }
    fn equal_self(&self, _right: &[u8]) -> bool {
        fastrand::bool()
    }
}

/// IP Address Comparison
#[derive(Default, Debug)]
struct CompareIP {
    value: Vec<u8>,
}

/// Default comparison
#[derive(Default, Debug)]
struct ComparePlain {
    value: Vec<u8>,
}

/// Default comparison
#[derive(Default, Debug)]
struct CompareLower {
    value: Vec<u8>,
}

/// Compare by length of string
#[derive(Default, Debug)]
struct CompareLen {
    value: u32,
}

/// always equal comparison
#[derive(Default, Debug)]
struct CompareEqual {}

/// f64 comparison
#[derive(Default, Debug)]
struct Comparef64 {
    value: f64,
}

impl Comparef64 {
    const fn new() -> Self {
        Self { value: 0.0 }
    }
}

/// nnn.nnn comparison
#[derive(Default, Debug)]
struct CompareNumeric {
    value: Vec<u8>,
}

impl CompareNumeric {
    const fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl ComparePlain {
    const fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl CompareLower {
    const fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl CompareIP {
    const fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl CompareLen {
    const fn new() -> Self {
        Self { value: 0 }
    }
}

impl Compare for ComparePlain {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        left.cmp(right)
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        left == right
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        item.cache = u64::from_be_bytes(make_array(value));
        item.assign_complete(value.len() <= 8);
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.to_vec();
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        self.value[..].cmp(right)
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value == right
    }
}

impl Compare for CompareLower {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        left.cmp_insens(right)
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        left.equal_insens(right)
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        let mut aa = make_array(value);
        for x in &mut aa {
            x.make_ascii_lowercase();
        }
        item.cache = u64::from_be_bytes(aa);
        item.assign_complete(value.len() <= 8);
    }
    fn set(&mut self, value: &[u8]) {
        value.assign_lower(&mut self.value);
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        self.value.cmp_insens_quick(right)
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value.equal_insens_quick(right)
    }
}

impl Compare for CompareIP {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        ip_cmp(left, right)
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        ip_cmp(left, right) == Ordering::Equal
    }
    fn fill_cache(&self, item: &mut Item, _value: &[u8]) {
        item.cache = 0; // FIXME
        item.clear_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.to_vec();
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        ip_cmp(&self.value, right)
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        ip_cmp(&self.value, right) == Ordering::Equal
    }
}

impl Compare for CompareNumeric {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        num_cmp_signed(left, right)
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        num_cmp_signed(left, right) == Ordering::Equal
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        item.cache = ulp_to_ulong(value.to_f64_lossy());
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.to_vec();
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        num_cmp_signed(&self.value, right)
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        num_cmp_signed(&self.value, right) == Ordering::Equal
    }
}

impl Compare for CompareEqual {
    fn comp(&self, _left: &[u8], _right: &[u8]) -> Ordering {
        Ordering::Equal
    }
    fn equal(&self, _left: &[u8], _right: &[u8]) -> bool {
        true
    }
    fn fill_cache(&self, item: &mut Item, _value: &[u8]) {
        item.cache = 0;
        item.set_complete();
    }
    fn set(&mut self, _value: &[u8]) {}
    fn comp_self(&self, _right: &[u8]) -> Ordering {
        Ordering::Equal
    }
    fn equal_self(&self, _right: &[u8]) -> bool {
        true
    }
}

impl Compare for Comparef64 {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        fcmp(left.to_f64_lossy(), right.to_f64_lossy())
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        left.to_f64_lossy() == right.to_f64_lossy()
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        item.cache = ulp_to_ulong(value.to_f64_lossy());
        item.set_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.to_f64_lossy();
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        fcmp(self.value, right.to_f64_lossy())
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value == right.to_f64_lossy()
    }
}

impl Compare for CompareLen {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        left.len().cmp(&right.len())
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        left.len() == right.len()
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        item.cache = value.len() as u64;
        item.set_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.len() as u32;
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        self.value.cmp(&(right.len() as u32))
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value == right.len() as u32
    }
}

impl Item {
    /// new item
    #[must_use]
    pub const fn new() -> Self {
        Self { offset: 0, size_plus: 0, cache: 0 }
    }
    /// return line as slice
    #[must_use]
    pub fn get<'a>(&self, base: &'a [u8]) -> &'a [u8] {
        &base[self.begin()..self.end()]
    }
    /// size in bytes of line
    #[must_use]
    pub const fn size(&self) -> u32 {
        self.size_plus & 0x7fff_ffff
    }
    /// test complete bit
    #[must_use]
    pub const fn complete(&self) -> bool {
        (self.size_plus & 0x8000_0000) != 0
    }
    /// set complete bit
    pub const fn set_complete(&mut self) {
        self.size_plus |= 0x8000_0000;
    }
    /// clear complete bit
    pub const fn clear_complete(&mut self) {
        self.size_plus &= 0x7fff_ffff;
    }
    /// conditionally set complete bit
    pub const fn assign_complete(&mut self, tag: bool) {
        if tag {
            self.set_complete();
        } else {
            self.clear_complete();
        }
    }
    /// offset of begin
    #[must_use]
    pub const fn begin(&self) -> usize {
        self.offset as usize
    }
    /// offset of end
    #[must_use]
    pub const fn end(&self) -> usize {
        self.offset as usize + self.size() as usize
    }
    /// Test an Item for equality to myself
    #[must_use]
    pub fn equal(&self, other: &Self, base: &[u8]) -> bool {
        if self.cache != other.cache {
            return false;
        }
        if self.complete() && other.complete() {
            return true;
        }
        self.get(base) == other.get(base)
    }
    /// Compare Item to myself with standard ordering
    #[must_use]
    pub fn compare(&self, other: &Self, base: &[u8]) -> Ordering {
        if self.cache < other.cache {
            return Ordering::Less;
        }
        if self.cache > other.cache {
            return Ordering::Greater;
        }
        if (self.cache == other.cache) && self.complete() && other.complete() {
            return Ordering::Equal;
        }
        self.get(base).cmp(other.get(base))
    }
    /// Compare an Item to myself, with custom ordering
    pub fn compare2(&self, other: &Self, base: &[u8], cmp: &dyn Compare) -> Ordering {
        if self.cache < other.cache {
            return Ordering::Less;
        }
        if self.cache > other.cache {
            return Ordering::Greater;
        }
        if (self.cache == other.cache) && self.complete() && other.complete() {
            return Ordering::Equal;
        }
        cmp.comp(self.get(base), other.get(base))
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
struct LineCompExpr {
    exprs: Vec<Expr>,
    value: f64,
}

impl LineCompExpr {
    fn new(expr: &str) -> Result<Self> {
        Ok(Self { exprs: vec![Expr::new(expr)?], value: 0.0 })
    }
}

impl LineCompare for LineCompExpr {
    fn comp_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        let left_val = self.exprs[left_file].eval(left);
        let right_val = self.exprs[right_file].eval(right);
        fcmp(left_val, right_val)
    }
    fn used_cols(&self, v: &mut Vec<usize>, file_num: usize) {
        self.exprs[file_num].used_cols(v);
    }
    fn equal_cols(
        &mut self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        let left_val = self.exprs[left_file].eval(left);
        let right_val = self.exprs[right_file].eval(right);
        left_val == right_val
    }
    fn comp_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        let left_val = self.exprs[left_file].eval_line(left, delim);
        let right_val = self.exprs[right_file].eval_line(right, delim);
        fcmp(left_val, right_val)
    }
    fn equal_lines(
        &mut self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        let left_val = self.exprs[left_file].eval_line(left, delim);
        let right_val = self.exprs[right_file].eval_line(right, delim);
        left_val == right_val
    }
    fn lookup(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()> {
        while self.exprs.len() < (file_num + 1) {
            self.exprs.push(Expr::new(self.exprs[0].expr())?);
        }
        self.exprs[file_num].lookup(fieldnames)
    }

    fn fill_cache_cols(&mut self, item: &mut Item, value: &TextLine) {
        item.cache = ulp_to_ulong(self.exprs[0].eval(value));
        item.set_complete();
    }
    fn fill_cache_line(&mut self, item: &mut Item, value: &[u8], delim: u8) {
        let val = self.exprs[0].eval_line(item.get(value), delim);
        item.cache = ulp_to_ulong(val);
        item.set_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = value.to_f64_lossy();
    }
    fn comp_self_cols(&mut self, right: &TextLine) -> Ordering {
        let value = self.exprs[0].eval(right);
        fcmp(self.value, value)
    }
    fn equal_self_cols(&mut self, right: &TextLine) -> bool {
        let value = self.exprs[0].eval(right);
        self.value == value
    }
    fn comp_self_line(&mut self, right: &[u8], delim: u8) -> Ordering {
        let value = self.exprs[0].eval_line(right, delim);
        fcmp(self.value, value)
    }
    fn equal_self_line(&mut self, right: &[u8], delim: u8) -> bool {
        let value = self.exprs[0].eval_line(right, delim);
        self.value == value
    }
}

/// return true if ordering is bad.
/// print appropriate message to stderr
pub fn comp_check(f: &Reader, cmp: &mut LineCompList, unique: bool) -> bool {
    let c = cmp.comp_cols(f.prev_line(1), f.curr_line());
    let bad = match c {
        Ordering::Less => false,
        Ordering::Equal => unique,
        Ordering::Greater => true,
    };
    if c == Ordering::Equal && unique {
        eprintln!("Lines are equal when they should be unique.");
    } else if bad {
        eprintln!("Lines are out of order");
    }
    if bad {
        eprint!("{} : ", f.line_number() - 1);
        prerr_n(&[f.prev_line(1).line()]);
        eprint!("{} : ", f.line_number());
        prerr_n(&[f.curr_line().line()]);
    }
    bad
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_less(x: &[u8], y: &[u8]) {
        assert_eq!(ip_cmp(x, y), Ordering::Less);
        assert_eq!(ip_cmp(y, x), Ordering::Greater);
    }

    #[test]
    fn test_ip_cmp() {
        assert_eq!(ip_cmp(b"", b""), Ordering::Equal);
        assert_eq!(ip_cmp(b"1", b"1"), Ordering::Equal);
        assert_eq!(ip_cmp(b"1.2", b"1.2"), Ordering::Equal);
        assert_eq!(ip_cmp(b"1.2.3.4", b"1.2.3.4"), Ordering::Equal);

        test_less(b"", b"1");
        test_less(b"1.100", b"100.1");

        assert_eq!(ip_cmp(b"X", b"Y"), Ordering::Equal);
        assert_eq!(ip_cmp(b"1.2.3X", b"1.2.3Y"), Ordering::Equal);
        assert_eq!(ip_cmp(b"1.2.3.X", b"1.2.3.Y"), Ordering::Equal);
        test_less(b"1.2.3X", b"1.2.3.Y");
    }
    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn num_cmp() {
        assert_eq!(num_cmp_signed(b"1", b"1"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"  1", b"1"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"1", b"  1"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"1,2", b"12"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"      1,2,3", b"123"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"123", b"   1,2,3"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b".123", b".123"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"123", b"124"), Ordering::Less);
        assert_eq!(num_cmp_signed(b"123", b"1234"), Ordering::Less);
        assert_eq!(num_cmp_signed(b"123", b"-124"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"123", b"-1234"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"999", b"1111"), Ordering::Less);
        assert_eq!(num_cmp_signed(b".999", b".1111"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"1234", b"123X"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"5.123", b"5.123"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"5.123", b"5.1234"), Ordering::Less);
        assert_eq!(num_cmp_signed(b"5.123", b"5.123X"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"5.1234", b"5.123"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"5.1234", b"5.1234"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"5.1234", b"5.123X"), Ordering::Greater);
        assert_eq!(num_cmp_signed(b"5.123X", b"5.123"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"5.123X", b"5.1234"), Ordering::Less);
        assert_eq!(num_cmp_signed(b"5.123X", b"5.123X"), Ordering::Equal);
        assert_eq!(num_cmp_signed(b"5.1234", b"5.1235"), Ordering::Less);
        assert_eq!(num_cmp_signed(b"5.1235", b"5.1234"), Ordering::Greater);
    }
}
