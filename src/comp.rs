//! Tools for comparing lines and fields
//!
//!  * [Compare] -- a trait for comparing slices
//!  * [Comp] -- a struct containing a [Compare], plus some context
//!  * [CompList] -- an ordered list of [Comp]
//!
//!  * LineCompare -- a trait for comparing lines, often implemented in terms of a [Compare]
//!  * LineComp -- a struct containing a [LineCompare], plus some context
//!  * LineCompList -- an ordered list of [LineComp]
//!
#![allow(clippy::float_cmp)]
use crate::column::NamedCol;
use crate::text::Text;
use crate::{err, Error, Result, TextLine};
use lazy_static::lazy_static;
use libm;
use std::cmp::Ordering;
use std::fmt;
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
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering;
    /// which columns are in use for this file
    fn used_cols(&self, v: &mut Vec<usize>, file_num: usize);
    /// compare lines
    fn equal_cols(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool;
    /// compare lines
    fn comp_lines(
        &self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> Ordering;
    /// compare lines
    fn equal_lines(
        &self,
        left: &[u8],
        right: &[u8],
        delim: u8,
        left_file: usize,
        right_file: usize,
    ) -> bool;
    /// resolve named columns; illegal to call any of the others with a file that has not been looked up
    fn lookup(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()>;
    /// initialize columns in TextLine?
    fn need_split(&self) -> bool {
        true
    }
    /// set cache for this value
    fn fill_cache_cols(&self, item: &mut Item, value: &TextLine);
    /// set cache for this value
    fn fill_cache_line(&self, item: &mut Item, value: &[u8], delim: u8);
    /// set my value
    fn set(&mut self, value: &[u8]);
    /// Compare self to line
    fn comp_self_cols(&self, right: &TextLine) -> Ordering;
    /// Compare self to line for equality
    fn equal_self_cols(&self, right: &TextLine) -> bool;
    /// Compare self to line
    fn comp_self_line(&self, right: &[u8], delim: u8) -> Ordering;
    /// Compare self to line for equality
    fn equal_self_line(&self, right: &[u8], delim: u8) -> bool;
}

/// amount of junk allowed in a number, before falling back to the 'error' value
#[derive(Debug, Copy, Clone)]
pub enum JunkType {
    /// best effort for any input
    Any,
    /// best effort, as long as some non-empty prefix is ok
    Trailing,
    /// every byte but be part of a valid value
    None,
}
impl Default for JunkType {
    fn default() -> Self {
        Self::Any
    }
}

/// Value to assign to junk values
#[derive(Debug, Copy, Clone)]
pub enum JunkVal {
    /// if value is just, make is compare less
    Min,
    /// if value is just, make is compare greater
    Max,
}
impl Default for JunkVal {
    fn default() -> Self {
        Self::Max
    }
}

/// What counts as junk, and what to do about it
#[derive(Debug, Default, Copy, Clone)]
pub struct Junk {
    /// what constitutes junk
    pub junk_type: JunkType,
    /// what do do with junk
    pub junk_val: JunkVal,
}

/// Settings for one Compare object
#[allow(missing_debug_implementations)]
pub struct Comp {
    /// junk allowed, responsibiltiy of inner Compare
    junk: Junk,
    /// optional arg to comparison, responsibiltiy of inner Compare
    pub pattern: String,

    /// type of comparison
    pub ctype: String,
    /// reverse comparison?
    reverse: bool,
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
    pub fn new() -> Self {
        Self::default()
    }
    /// new
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
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        self.comp.comp(left, right)
    }
    /// Compare two slices for equality
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        self.comp.equal(left, right)
    }
    /// set cache for this value
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        self.comp.fill_cache(item, value)
    }
    /// set my value
    fn set(&mut self, value: &[u8]) {
        self.comp.set(value)
    }
    /// Compare self to slice
    fn comp_self(&self, right: &[u8]) -> Ordering {
        self.comp.comp_self(right)
    }
    /// Compare self to slice for equality
    fn equal_self(&self, right: &[u8]) -> bool {
        self.comp.equal_self(right)
    }
}

/// Settings for one Compare object
#[allow(missing_debug_implementations)]
pub struct LineComp {
    /// junk allowed, responsibiltiy of inner LineCompare
    junk: Junk,
    /// optional arg to comparison, responsibiltiy of inner LineCompare
    pub pattern: String,
    /// period delimited list of columns, responsibiltiy of inner LineCompare
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

/// extract column from line
pub fn get_col(data: &[u8], col: usize, delim: u8) -> &[u8] {
    for (n, s) in data.split(|ch| *ch == delim).enumerate() {
        if n == col {
            return if !s.is_empty() && s.last().unwrap() == &b'\n' {
                &s[0..s.len() - 1]
            } else {
                s
            };
        }
    }
    &data[0..0]
}

impl LineComp {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// which columns are in use for this file
    pub fn used_cols(&self, v: &mut Vec<usize>, file_num: usize) {
        self.comp.used_cols(v, file_num)
    }
    /// Compare Items
    pub fn comp_items(&self, base: &[u8], left: &Item, right: &Item) -> Ordering {
        self.reverse({
            if left.cache < right.cache {
                Ordering::Less
            } else if left.cache > right.cache {
                Ordering::Greater
            } else if left.complete() && right.complete() {
                Ordering::Equal
            } else {
                self.comp_lines(left.get(base), right.get(base))
            }
        })
    }
    /// Compare Items
    pub fn equal_items(&self, base: &[u8], left: &Item, right: &Item) -> bool {
        if left.complete() && right.complete() {
            left.cache == right.cache
        } else {
            self.equal_lines(left.get(base), right.get(base))
        }
    }
    /// reverse the ordering if self.reverse is set
    pub const fn reverse(&self, x: Ordering) -> Ordering {
        if self.reverse {
            x.reverse()
        } else {
            x
        }
    }
    /// compare [TextLine]s from a single file
    pub fn comp_cols(&self, left: &TextLine, right: &TextLine) -> Ordering {
        self.reverse(self.comp.comp_cols(left, right, 0, 0))
    }
    /// compare [TextLine]s from multiple files
    pub fn comp_cols_n(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        self.reverse(self.comp.comp_cols(left, right, left_file, right_file))
    }
    /// compare [TextLine]s from a single file
    pub fn equal_cols(&self, left: &TextLine, right: &TextLine) -> bool {
        self.comp.equal_cols(left, right, 0, 0)
    }
    /// compare [TextLine]s from multiple files
    pub fn equal_cols_n(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal_cols(left, right, left_file, right_file)
    }
    /// compare line from a single file
    pub fn comp_lines(&self, left: &[u8], right: &[u8]) -> Ordering {
        self.reverse(self.comp.comp_lines(left, right, self.delim, 0, 0))
    }
    /// compare lines from multiple files
    pub fn comp_lines_n(
        &self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        self.reverse(
            self.comp
                .comp_lines(left, right, self.delim, left_file, right_file),
        )
    }
    /// compare line from a single file
    pub fn equal_lines(&self, left: &[u8], right: &[u8]) -> bool {
        self.comp.equal_lines(left, right, self.delim, 0, 0)
    }
    /// compare lines from multiple files
    pub fn equal_lines_n(
        &self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp
            .equal_lines(left, right, self.delim, left_file, right_file)
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.comp.lookup(fieldnames, 0)
    }
    /// resolve named columns for the given file
    pub fn lookup_n(&mut self, fieldnames: &[&str], file_num: usize) -> Result<()> {
        self.comp.lookup(fieldnames, file_num)
    }
    /// do [TextLine]s need their columns to be initialized
    pub fn need_split(&self) -> bool {
        self.comp.need_split()
    }
    /// set cache of [Item] from [TextLine]
    pub fn fill_cache_cols(&self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache_cols(item, value)
    }
    /// set cache of [Item] from line
    pub fn fill_cache_line(&self, item: &mut Item, value: &[u8]) {
        self.comp.fill_cache_line(item, value, self.delim)
    }
    /// set value for later comparison
    pub fn set(&mut self, value: &[u8]) {
        self.comp.set(value)
    }
    /// compare self to this line
    pub fn comp_self_cols(&self, right: &TextLine) -> Ordering {
        self.reverse(self.comp.comp_self_cols(right))
    }
    /// compare self to this line
    pub fn equal_self_cols(&self, right: &TextLine) -> bool {
        self.comp.equal_self_cols(right)
    }
    /// compare self to this line
    pub fn comp_self_line(&self, right: &[u8]) -> Ordering {
        self.reverse(self.comp.comp_self_line(right, self.delim))
    }
    /// compare self to this line
    pub fn equal_self_line(&self, right: &[u8]) -> bool {
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
        &self,
        left: &TextLine,
        right: &TextLine,
        _left_file: usize,
        _right_file: usize,
    ) -> Ordering {
        self.comp.comp(&left.line, &right.line)
    }
    /// compare lines
    fn equal_cols(
        &self,
        left: &TextLine,
        right: &TextLine,
        _left_file: usize,
        _right_file: usize,
    ) -> bool {
        self.comp.equal(&left.line, &right.line)
    }
    /// compare lines
    fn comp_lines(
        &self,
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
        &self,
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

    fn fill_cache_cols(&self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache(item, &value.line)
    }

    fn fill_cache_line(&self, item: &mut Item, value: &[u8], _delim: u8) {
        self.comp.fill_cache(item, value)
    }

    fn set(&mut self, value: &[u8]) {
        self.comp.set(value)
    }

    fn comp_self_cols(&self, right: &TextLine) -> Ordering {
        self.comp.comp_self(&right.line)
    }

    fn equal_self_cols(&self, right: &TextLine) -> bool {
        self.comp.equal_self(&right.line)
    }

    fn comp_self_line(&self, right: &[u8], _delim: u8) -> Ordering {
        self.comp.comp_self(right)
    }

    fn equal_self_line(&self, right: &[u8], _delim: u8) -> bool {
        self.comp.equal_self(right)
    }
}

/// compare the whole line with the [Compare]
struct LineCompCol {
    cols: Vec<NamedCol>,
    comp: Comp,
}

impl LineCompCol {
    /// cols must not be empty, use LineCompWhole for that
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
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        self.comp.comp(
            left.get(self.cols[left_file].num),
            right.get(self.cols[right_file].num),
        )
    }
    /// compare lines
    fn equal_cols(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        self.comp.equal(
            left.get(self.cols[left_file].num),
            right.get(self.cols[right_file].num),
        )
    }
    /// compare lines
    fn comp_lines(
        &self,
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
        &self,
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
    fn fill_cache_cols(&self, item: &mut Item, value: &TextLine) {
        self.comp.fill_cache(item, value.get(self.cols[0].num))
    }

    fn fill_cache_line(&self, item: &mut Item, value: &[u8], delim: u8) {
        self.comp
            .fill_cache(item, get_col(item.get(value), self.cols[0].num, delim))
    }

    fn set(&mut self, value: &[u8]) {
        self.comp.set(value)
    }

    fn comp_self_cols(&self, right: &TextLine) -> Ordering {
        self.comp.comp_self(right.get(self.cols[0].num))
    }

    fn equal_self_cols(&self, right: &TextLine) -> bool {
        self.comp.equal_self(right.get(self.cols[0].num))
    }

    fn comp_self_line(&self, right: &[u8], delim: u8) -> Ordering {
        self.comp.comp_self(get_col(right, self.cols[0].num, delim))
    }

    fn equal_self_line(&self, right: &[u8], delim: u8) -> bool {
        self.comp
            .equal_self(get_col(right, self.cols[0].num, delim))
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
pub fn make_array(val: &[u8]) -> [u8; 8] {
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

const fn is_e(ch: u8) -> bool {
    (ch == b'e') || (ch == b'E')
}

const fn is_exp_char(ch: u8) -> bool {
    ch.is_ascii_digit() || (ch == b'+') || (ch == b'-')
}

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
        if b.is_empty() || !b[0].is_ascii_digit() {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    } else if b.is_empty() {
        if a[0].is_ascii_digit() {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    } else if a[0].is_ascii_digit() {
        if b[0].is_ascii_digit() {
            a[0].cmp(&b[0])
        } else {
            Ordering::Greater
        }
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
        } else {
            return Ordering::Less;
        }
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

// https://doc.rust-lang.org/std/primitive.f64.html
/// Standard Rust parsing with str_to_d interface
pub fn str_to_d_native(n: &str) -> f64 {
    n.parse().unwrap()
}

/// str_to_d, but always return best guess
pub fn str_to_d_lossy(num: &[u8]) -> f64 {
    match str_to_d(num) {
        Err(_x) => 0.0,
        Ok((x, _y)) => x,
    }
}

/// str_to_d, but fail if any text remains
pub fn str_to_d_whole(num: &[u8]) -> Result<f64> {
    match str_to_d(num) {
        Err(x) => Err(x),
        Ok((x, y)) => {
            if !y.is_empty() {
                return err!(
                    "Extra stuff after number : {}",
                    String::from_utf8_lossy(num)
                );
            }
            Ok(x)
        }
    }
}

/// Convert bytes to integer, return unused portion
pub fn str_to_i(num: &[u8]) -> Result<(i64, &[u8])> {
    let mut neg: i64 = 1;
    let mut curr = num.trimw_start();
    if !curr.is_empty() {
        if curr[0] == b'+' {
            curr = &curr[1..];
        } else if curr[0] == b'-' {
            curr = &curr[1..];
            neg = -1;
        }
    }
    let mut ret: i64 = 0;
    if curr.is_empty() || !curr[0].is_ascii_digit() {
        return err!("Malformed Integer {}", String::from_utf8_lossy(num));
    }
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        ret *= 10;
        ret += (curr[0] - b'0') as i64;
        curr = &curr[1..];
    }
    ret *= neg;
    Ok((ret, curr))
}

/// Convert bytes to integer, return unused portion
pub fn str_to_u(num: &[u8]) -> Result<(u64, &[u8])> {
    let mut curr = num.trimw_start();
    if !curr.is_empty() && curr[0] == b'+' {
        curr = &curr[1..];
    }
    let mut ret: u64 = 0;
    if curr.is_empty() || !curr[0].is_ascii_digit() {
        return err!("Malformed Integer {}", String::from_utf8_lossy(num));
    }
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        ret *= 10;
        ret += (curr[0] - b'0') as u64;
        curr = &curr[1..];
    }
    Ok((ret, curr))
}

/// Convert bytes to integer, fail if unused bytes
pub fn str_to_u_whole(num: &[u8]) -> Result<u64> {
    let x = str_to_u(num)?;
    if !x.1.is_empty() {
        return err!(
            "Extra stuff after number : {}",
            String::from_utf8_lossy(num)
        );
    }
    Ok(x.0)
}

/// Convert bytes to integer, fail if unused bytes
pub fn str_to_i_whole(num: &[u8]) -> Result<i64> {
    let x = str_to_i(num)?;
    if !x.1.is_empty() {
        return err!(
            "Extra stuff after number : {}",
            String::from_utf8_lossy(num)
        );
    }
    Ok(x.0)
}

/// Convert bytes to integer, ignore unused bytes and errors
pub fn str_to_u_lossy(num: &[u8]) -> u64 {
    let x = str_to_u(num);
    if x.is_err() {
        return 0;
    }
    x.unwrap().0
}

/// Convert bytes to integer, ignore unused bytes and errors
pub fn str_to_i_lossy(num: &[u8]) -> i64 {
    let x = str_to_i(num);
    if x.is_err() {
        return 0;
    }
    x.unwrap().0
}

/// turn text into f64, return unused portion of text
///```
/// use cdx::comp::str_to_d;
/// let (x, y) = str_to_d(b"123.456xyz").unwrap();
/// assert_eq!(x, 123.456);
/// assert_eq!(y, b"xyz");
/// let (x, y) = str_to_d(b"123e2").unwrap();
/// assert_eq!(y, b"");
/// assert_eq!(x, 123e2);
/// let (x, y) = str_to_d(b"123e-27").unwrap();
/// assert_eq!(y, b"");
/// assert_eq!(x, 123e-27);
///```
pub fn str_to_d(num: &[u8]) -> Result<(f64, &[u8])> {
    let mut neg: f64 = 1.0;
    let mut curr = num.trimw_start();
    if curr.is_empty() {
        return err!(
            "Can't convert empty string to floating point {}",
            String::from_utf8_lossy(num)
        );
    }
    if curr[0] == b'+' {
        curr = &curr[1..];
    } else if curr[0] == b'-' {
        curr = &curr[1..];
        neg = -1.0;
    }
    let mut saw_digit = false;
    let mut ret: f64 = 0.0;
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        saw_digit = true;
        ret *= 10.0;
        ret += (curr[0] - b'0') as f64;
        curr = &curr[1..];
    }
    if !curr.is_empty() && (curr[0] == b'.') {
        curr = &curr[1..];
        let mut place = 0.1;
        while !curr.is_empty() && curr[0].is_ascii_digit() {
            saw_digit = true;
            ret += place * (curr[0] - b'0') as f64;
            place *= 0.1;
            curr = &curr[1..];
        }
    }
    if (curr.len() > 1) && is_e(curr[0]) && is_exp_char(curr[1]) {
        curr = &curr[1..];
        let mut neg_exp: i32 = 1;
        if !curr.is_empty() {
            if curr[0] == b'+' {
                curr = &curr[1..];
            } else if curr[0] == b'-' {
                neg_exp = -1;
                curr = &curr[1..];
            }
        }
        let mut exponent: i32 = 0;
        while !curr.is_empty() && curr[0].is_ascii_digit() {
            saw_digit = true;
            exponent *= 10;
            exponent += (curr[0] - b'0') as i32;
            curr = &curr[1..];
        }
        exponent *= neg_exp;
        match exponent {
            -2 => {
                ret *= 0.01;
            }
            -1 => {
                ret *= 0.1;
            }
            0 => {}
            1 => {
                ret *= 10.0;
            }
            2 => {
                ret *= 100.0;
            }
            3 => {
                ret *= 1000.0;
            }
            //  	    These two lose accuracy
            //	    _ => {ret *= ((exponent as f64) * std::f64::consts::LOG2_10).exp2();},
            //	    _ => {ret *= ((exponent as f64) * std::f64::consts::LN_10).exp();},
            _ => {
                ret *= libm::exp10(exponent as f64);
            }
        }
    }
    if !saw_digit {
        return err!(
            "Malformed floating point number {}",
            String::from_utf8_lossy(num)
        );
    }
    Ok((neg * ret, curr))
}

/// One line in a buffer of text
#[derive(Debug, Clone, Copy)]
pub struct Item {
    /// offset from beginning of buffer
    pub offset: u32,
    /// length of line, including newline, with high bit reserved
    pub size_plus: u32,
    /// Compare caches, and then if necessary compare actual data
    pub cache: u64,
}
const _: () = assert!(std::mem::size_of::<Item>() == 16);

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
/// A named constructor for a [Compare], used by [CompMaker]
struct CompMakerItem {
    /// matched against Comparator::ctype
    tag: &'static str,
    /// what this matcher does
    help: &'static str,
    /// Create a dyn Match from a pattern
    maker: MakerBox,
}

type LineMakerBox = Box<dyn Fn(&LineComp) -> Result<Box<dyn LineCompare>> + Send>;
/// A named constructor for a [LineCompare], used by [CompMaker]
struct LineCompMakerItem {
    /// matched against Comparator::ctype
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

lazy_static! {
    static ref COMP_MAKER: Mutex<Vec<CompMakerItem>> = Mutex::new(Vec::new());
    static ref LINE_MAKER: Mutex<Vec<LineCompMakerItem>> = Mutex::new(Vec::new());
    static ref COMP_ALIAS: Mutex<Vec<CompMakerAlias>> = Mutex::new(Vec::new());
    static ref MODIFIERS: Vec<&'static str> = vec!["rev", "strict", "trail", "low"];
}

/// Makes a [Compare or LineComp]
#[derive(Debug, Clone, Default)]
pub struct CompMaker {}

impl CompMaker {
    /// add standard match makers
    fn init() -> Result<()> {
        if !COMP_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("length", "len")?;
        Self::do_add_alias("plain", "")?;
        Self::do_push("length", "Sort by length of string", |_p| {
            Ok(Box::new(CompareLen::new()))
        })?;
        Self::do_push("ip", "Sort as IP address or 1.2.3 section numbers", |_p| {
            Ok(Box::new(CompareIP::new()))
        })?;
        Self::do_push("plain", "Sort the plain bytes", |_p| {
            Ok(Box::new(ComparePlain::new()))
        })?;
        Self::do_push("lower", "Sort as the ascii lowercase of the string", |_p| {
            Ok(Box::new(CompareLower::new()))
        })?;
        Self::do_push(
            "float",
            "Convert to floating point, and sort the result.",
            |_p| Ok(Box::new(Comparef64::new())),
        )?;
        Self::do_push("numeric", "Convert NNN.nnn of arbitrary length.", |_p| {
            Ok(Box::new(CompareNumeric::new()))
        })?;
        Self::do_push("equal", "Everything always compares equal.", |_p| {
            Ok(Box::new(CompareEqual {}))
        })?;
        Ok(())
    }
    /// Add a new Compare. If a Compare already exists by that name, replace it.
    pub fn push<F: 'static>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&Comp) -> Result<Box<dyn Compare>> + Send,
    {
        Self::init()?;
        Self::do_push(tag, help, maker)
    }
    /// Add a new alias. If an alias already exists by that name, replace it.
    pub fn add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        Self::init()?;
        Self::do_add_alias(old_name, new_name)
    }
    /// Return name, replaced by its alias, if any.
    fn resolve_alias(name: &str) -> &str {
        let mut mm = COMP_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == name {
                return x.old_name;
            }
        }
        name
    }
    fn do_add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        if MODIFIERS.contains(&new_name) {
            return err!("You can't add an alias named {} because that is reserved for a modifier");
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
        Ok(())
    }
    fn do_push<F: 'static>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&Comp) -> Result<Box<dyn Compare>> + Send,
    {
        if MODIFIERS.contains(&tag) {
            return err!(
                "You can't add a matcher named {} because that is reserved for a modifier"
            );
        }
        let m = CompMakerItem {
            tag,
            help,
            maker: Box::new(maker),
        };
        let mut mm = COMP_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
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
        let mm = COMP_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
        }
        let mm = LINE_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
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
    /// create LineComp from spec
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
    /// create LineComp from columns, method and pattern
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
        let mm = COMP_MAKER.lock().unwrap();
        for x in &*mm {
            if ctype.eq_ignore_ascii_case(x.tag) {
                comp.comp = (x.maker)(comp)?;
                return Ok(());
            }
        }
        err!("No such compare type : '{}'", comp.ctype)
    }
    /// reset the LineCompare inside the LineComp
    pub fn remake_line_comp(comp: &mut LineComp) -> Result<()> {
        Self::init()?;
        let ctype = Self::resolve_alias(&comp.ctype);
        let mm = LINE_MAKER.lock().unwrap();
        for x in &*mm {
            if ctype.eq_ignore_ascii_case(x.tag) {
                comp.comp = (x.maker)(comp)?;
                return Ok(());
            }
        }
        let mm = COMP_MAKER.lock().unwrap();
        let mut new_comp = Comp::with_line_comp(comp);
        for x in &*mm {
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
    /*
        /// Create a Comparator from separate specs
        pub fn make3(columns: &str, method: &str, pattern: &str) -> Result<Comparator> {
            Self::init()?;
            let mut ret = Comparator::default();
            if !columns.is_empty() {
                for x in columns.split('.') {
                    ret.cols.push(NamedCol::new_from(x)?);
                }
            }
            for x in method.split('.') {
                // junk types
                if x.eq_ignore_ascii_case("rev") {
                    ret.reverse = true;
                } else {
                    ret.ctype = x.to_string();
                }
            }
            ret.comp = Self::make_comp(&ret.ctype, pattern)?;
            Ok(ret)
        }
        /// make a Compare from a spec
        pub fn make_comp(ctype: &str, pattern: &str) -> Result<Box<dyn Compare>> {
            Self::init()?;
            let ctype = Self::resolve_alias(ctype);
            let mm = COMP_MAKER.lock().unwrap();
            for x in &*mm {
                if ctype.eq_ignore_ascii_case(x.tag) {
                    return (x.maker)(pattern);
                }
            }
            err!("No such compare type : '{}'", ctype)
        }
        /// Create a Comparator from a full spec, i.e. "Columns,Method,Pattern""
        pub fn make(spec: &str) -> Result<Comparator> {
            if let Some((a, b)) = spec.split_once(',') {
                if let Some((c, d)) = b.split_once(',') {
                    Self::make3(a, c, d)
                } else {
                    Self::make3(a, b, "")
                }
            } else {
                Self::make3(spec, "", "")
            }
        }
    */
}

#[derive(Default, Debug)]
/// Ordered list of [Comp]
pub struct CompList {
    c: Vec<Comp>,
}
#[derive(Default, Debug)]
/// Ordered list of [LineComp]
pub struct LineCompList {
    c: Vec<LineComp>,
}

impl CompList {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// any [Comp]s in the list?
    pub fn is_empty(&self) -> bool {
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
    pub fn new() -> Self {
        Self::default()
    }
    /// any [ListComp]s in the list?
    pub fn is_empty(&self) -> bool {
        self.c.is_empty()
    }
    /// which columns used as part of key?
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
    pub fn comp_items(&self, base: &[u8], left: &Item, right: &Item) -> Ordering {
        if self.c.is_empty() {
            return Ordering::Equal;
        }
        let ret = self.c[0].comp_items(base, left, right);
        if ret != Ordering::Equal {
            return ret;
        }
        for x in self.c.iter().skip(1) {
            let ret = x.comp_lines(left.get(base), right.get(base));
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare [Item]s
    pub fn equal_items(&self, base: &[u8], left: &Item, right: &Item) -> bool {
        if self.c.is_empty() {
            return true;
        }
        if !self.c[0].equal_items(base, left, right) {
            return false;
        }
        for x in self.c.iter().skip(1) {
            if !x.equal_lines(left.get(base), right.get(base)) {
                return false;
            }
        }
        true
    }
    /// compare [TextLine]s in the same file
    pub fn comp_cols(&self, left: &TextLine, right: &TextLine) -> Ordering {
        self.comp_cols_n(left, right, 0, 0)
    }
    /// compare [TextLine]s in different files
    pub fn comp_cols_n(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        for x in &self.c {
            let ret = x.comp_cols_n(left, right, left_file, right_file);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare [TextLine]s in the same file
    pub fn equal_cols(&self, left: &TextLine, right: &TextLine) -> bool {
        self.equal_cols_n(left, right, 0, 0)
    }
    /// compare [TextLine]s in different files
    pub fn equal_cols_n(
        &self,
        left: &TextLine,
        right: &TextLine,
        left_file: usize,
        right_file: usize,
    ) -> bool {
        for x in &self.c {
            if !x.equal_cols_n(left, right, left_file, right_file) {
                return false;
            }
        }
        true
    }
    /// compare liness from the same
    pub fn comp_lines(&self, left: &[u8], right: &[u8]) -> Ordering {
        self.comp_lines_n(left, right, 0, 0)
    }
    /// compare lines from different files
    pub fn comp_lines_n(
        &self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> Ordering {
        for x in &self.c {
            let ret = x.comp_lines_n(left, right, left_file, right_file);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare lines from the same file
    pub fn equal_lines(&self, left: &[u8], right: &[u8]) -> bool {
        self.equal_lines_n(left, right, 0, 0)
    }
    /// compare lines from different files
    pub fn equal_lines_n(
        &self,
        left: &[u8],
        right: &[u8],
        left_file: usize,
        right_file: usize,
    ) -> bool {
        for x in &self.c {
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
            x.lookup_n(fieldnames, file_num)?
        }
        Ok(())
    }
    /// do [TextLine]s need their columns initialized
    pub fn need_split(&self) -> bool {
        for x in &self.c {
            if x.need_split() {
                return true;
            }
        }
        false
    }
    /// fill [Item]'s cache
    pub fn fill_cache_cols(&self, item: &mut Item, value: &TextLine) {
        if !self.c.is_empty() {
            self.c[0].fill_cache_cols(item, value);
        }
    }
    /// fill [Item]'s cache
    pub fn fill_cache_line(&self, item: &mut Item, value: &[u8]) {
        if !self.c.is_empty() {
            self.c[0].fill_cache_line(item, value);
        }
    }
    /// set value fo later comparison
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
    /// compare my value to this line
    pub fn comp_self_cols(&self, right: &TextLine) -> Ordering {
        for x in &self.c {
            let ret = x.comp_self_cols(right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare my value to this line
    pub fn equal_self_cols(&self, right: &TextLine) -> bool {
        for x in &self.c {
            if !x.equal_self_cols(right) {
                return false;
            }
        }
        true
    }
    /// compare my value to this line
    pub fn comp_self_line(&self, right: &[u8]) -> Ordering {
        for x in &self.c {
            let ret = x.comp_self_line(right);
            if ret != Ordering::Equal {
                return ret;
            }
        }
        Ordering::Equal
    }
    /// compare my value to this line
    pub fn equal_self_line(&self, right: &[u8]) -> bool {
        for x in &self.c {
            if !x.equal_self_line(right) {
                return false;
            }
        }
        true
    }
}

/// IP Address Comparison
#[derive(Default, Debug)]
pub struct CompareIP {
    value: Vec<u8>,
}

/// Default comparison
#[derive(Default, Debug)]
pub struct ComparePlain {
    value: Vec<u8>,
}

/// Default comparison
#[derive(Default, Debug)]
pub struct CompareLower {
    value: Vec<u8>,
}

/// Compare by length of string
#[derive(Default, Debug)]
pub struct CompareLen {
    value: u32,
}

/// always equal comparison
#[derive(Default, Debug)]
pub struct CompareEqual {}

/// f64 comparison
#[derive(Default, Debug)]
pub struct Comparef64 {
    value: f64,
}

impl Comparef64 {
    const fn new() -> Self {
        Self { value: 0.0 }
    }
}

/// nnn.nnn comparison
#[derive(Default, Debug)]
pub struct CompareNumeric {
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
            x.make_ascii_lowercase()
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

fn fcmp(x: f64, y: f64) -> Ordering {
    if x == y {
        return Ordering::Equal;
    }
    if x > y {
        return Ordering::Greater;
    }
    Ordering::Less
}

fn ulp_to_ulong(d: f64) -> u64 {
    let x = u64::from_ne_bytes(d.to_ne_bytes());
    if (x & 0x8000000000000000u64) != 0 {
        x ^ 0xffffffffffffffffu64
    } else {
        x | 0x8000000000000000u64
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
        item.cache = ulp_to_ulong(str_to_d_lossy(value));
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
        fcmp(str_to_d_lossy(left), str_to_d_lossy(right))
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        str_to_d_lossy(left) == str_to_d_lossy(right)
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        item.cache = ulp_to_ulong(str_to_d_lossy(value));
        item.set_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = str_to_d_lossy(value);
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        fcmp(self.value, str_to_d_lossy(right))
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value == str_to_d_lossy(right)
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
    pub const fn new() -> Self {
        Self {
            offset: 0,
            size_plus: 0,
            cache: 0,
        }
    }
    /// return line as slice
    pub fn get<'a>(&self, base: &'a [u8]) -> &'a [u8] {
        &base[self.begin()..self.end()]
    }
    /// size in bytes of line
    pub const fn size(&self) -> u32 {
        self.size_plus & 0x7fffffff
    }
    /// test complete bit
    pub const fn complete(&self) -> bool {
        (self.size_plus & 0x80000000) != 0
    }
    /// set complete bit
    pub fn set_complete(&mut self) {
        self.size_plus |= 0x80000000;
    }
    /// clear complete bit
    pub fn clear_complete(&mut self) {
        self.size_plus &= 0x7fffffff;
    }
    /// conditionally set complete bit
    pub fn assign_complete(&mut self, tag: bool) {
        if tag {
            self.set_complete();
        } else {
            self.clear_complete();
        }
    }
    /// offset of begin
    pub const fn begin(&self) -> usize {
        self.offset as usize
    }
    /// offset of end
    pub const fn end(&self) -> usize {
        self.offset as usize + self.size() as usize
    }
    /// Test an Item for equality to myself
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
