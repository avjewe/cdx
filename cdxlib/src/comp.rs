//! Tools for comparing lines and fields
//! Comparison -- an enum for comparison semantics
//! Compare -- a trait. Roughly one impl per Comparison
//! CompareSettings -- a Comparison, plus a column and other context
//! Comparator -- a CompareSettings, plus an instance of Compare matching the Comparison
#![allow(dead_code)]
#![allow(clippy::float_cmp)]
use crate::column::NamedCol;
use crate::{err, Error, Result, TextLine};
use libm;
use std::cmp::Ordering;
use std::fmt;

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

fn is_e(ch: u8) -> bool {
    (ch == b'e') || (ch == b'E')
}

fn is_exp_char(ch: u8) -> bool {
    ch.is_ascii_digit() || (ch == b'+') || (ch == b'-')
}

/// skip leading whitespace
pub fn skip_leading_white(num: &[u8]) -> &[u8] {
    let mut pos: usize = 0;
    for ch in num {
        if *ch <= b' ' {
            pos += 1;
        } else {
            break;
        }
    }
    &num[pos..]
}

/// https://doc.rust-lang.org/std/primitive.f64.html
/// Standard Rust parsing with str_to_d interface
pub fn str_to_d2(n: &str) -> f64 {
    n.parse().unwrap()
}

/// turn text into f64, return unused portion of text
///```
/// use cdxlib::comp::str_to_d;
/// let (x, y) = str_to_d(b"123.456xyz");
/// assert_eq!(x, 123.456);
/// assert_eq!(y, b"xyz");
/// let (x, y) = str_to_d(b"123e2");
/// assert_eq!(y, b"");
/// assert_eq!(x, 123e2);
/// let (x, y) = str_to_d(b"123e-27");
/// assert_eq!(y, b"");
/// assert_eq!(x, 123e-27);
///```
pub fn str_to_d_q(num: &[u8]) -> f64 {
    let (x, _y) = str_to_d(num);
    x
}

/// Convert slice to float, return unused portion
pub fn str_to_d(num: &[u8]) -> (f64, &[u8]) {
    let mut neg: f64 = 1.0;
    let mut curr = skip_leading_white(num);
    if !curr.is_empty() {
        if curr[0] == b'+' {
            curr = &curr[1..];
        } else if curr[0] == b'-' {
            curr = &curr[1..];
            neg = -1.0;
        }
    }
    let mut ret: f64 = 0.0;
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        ret *= 10.0;
        ret += (curr[0] - b'0') as f64;
        curr = &curr[1..];
    }
    if !curr.is_empty() && (curr[0] == b'.') {
        curr = &curr[1..];
        let mut place = 0.1;
        while !curr.is_empty() && curr[0].is_ascii_digit() {
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
    (neg * ret, curr)
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

/// How to compare two slices
#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    /// bytewise comparison of whole line
    Whole,
    /// bytewise comparison of one column
    Plain,
    /// compare length of data, not contents
    Length,
    /// parse as f64, and compare that. Needs option for malfomed numbers.
    Double,
    /*
        Reverse, // compare right to left
        Lower, // ascii_tolower before comparison
        Human,  // 2.3K or 4.5G
        Numeric, // compare nnn.nnn with no limit on length
        Fuzzy(u32 n), // integer compare, but equal if within n
        Date(String fmt), // formatted date compare
        Url, //. Compare backwards from first slash, then forwards from first slash
        Prefix, // columns are equal if either is a prefix of the other
    */
}

/// Settings for one compare object
#[derive(Debug, Clone)]
pub struct CompareSettings {
    /// type of comparison
    pub kind: Comparison,
    /// column to compare
    pub left_col: NamedCol,
    /// column to compare
    pub right_col: NamedCol,
    /// reverse comparison?
    pub reverse: bool,
    // failure 0, -inf +inf
    // Vec<CompareSettings>
    // stable: bool
}

/// CompareSettings, wiht 'kind' turned into an actual object
pub struct Comparator {
    /// the Compare Settings
    pub mode: CompareSettings,
    /// Compare made from Comparison
    pub comp: Box<dyn Compare>,
}

impl fmt::Debug for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Comparator {:?}", self.mode)
    }
}

impl CompareSettings {
    /// new CompareSettings
    pub fn new() -> Self {
        Self {
            kind: Comparison::Plain,
            left_col: NamedCol::new(),
            right_col: NamedCol::new(),
            reverse: false,
        }
    }
    /// reverse a comparison, if reverse flag is set
    pub fn do_reverse(&self, x: Ordering) -> Ordering {
	if self.reverse {
	    x.reverse()
	}
	else {
	    x
	}
    }
    /// Resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.left_col.lookup(fieldnames)?;
        self.right_col.lookup(fieldnames)
    }
    /// make Compare from Comparison
    fn make_box(&self) -> Box<dyn Compare> {
        match self.kind {
            Comparison::Whole => Box::new(CompareWhole::new()),
            Comparison::Plain => Box::new(ComparePlain::new()),
            Comparison::Length => Box::new(CompareLen::new()),
            Comparison::Double => Box::new(Comparef64::new()),
        }
    }
    /// make Comparator
    pub fn make_comp(&self) -> Comparator {
        Comparator::new(self.clone(), self.make_box())
    }
}
impl Default for CompareSettings {
    fn default() -> Self {
        Self::new()
    }
}

impl Comparator {
    /// new Comparator
    pub fn new(mode: CompareSettings, comp: Box<dyn Compare>) -> Self {
        Self { mode, comp }
    }
    /// compare two lines for equality
    pub fn equal_cols(&self, left: &TextLine, right: &TextLine) -> bool {
        self.comp.equal_cols(self, left, right)
    }
    /// compare two lines
    pub fn comp_cols(&self, left: &TextLine, right: &TextLine) -> Ordering {
        self.comp.comp_cols(self, left, right)
    }
    /// resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.mode.lookup(fieldnames)
    }
    /// Does comparison require line split into columns
    pub fn need_split(&self) -> bool {
        self.comp.need_split()
    }
    /// Get the column out of an unparsed line
    /// FIXME - need delim
    /// FIXME - need left vs right
    pub fn get_col<'a>(&self, line: &'a [u8]) -> &'a [u8] {
	let col = self.mode.left_col.num;
	let mut curr_col = 0;
	let mut start = 0;
	for x in line.iter().enumerate() {
	    if x.1 == &b'\t' {
		if curr_col == col {
		    return &line[start..x.0];
		}
		curr_col += 1;
		start = x.0 + 1;
	    }
	}
	&line[0..0]
    }
}

/// method of comparing two slices
pub trait Compare {
    /// Compare two slices
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

    /// return true if comparator needs line split into columns
    fn need_split(&self) -> bool {
        true
    }
    /// Compare two columns
    fn comp_cols(&self, spec: &Comparator, left: &TextLine, right: &TextLine) -> Ordering {
        spec.mode.do_reverse(self.comp(
            left.get(spec.mode.left_col.num),
            right.get(spec.mode.right_col.num),
        ))
    }
    /// Compare two columns for equality
    fn equal_cols(&self, spec: &Comparator, left: &TextLine, right: &TextLine) -> bool {
        self.equal(
            left.get(spec.mode.left_col.num),
            right.get(spec.mode.right_col.num),
        )
    }
    /// Compare two items
    fn comp_items(&self, spec: &Comparator, base: &[u8], left: &Item, right: &Item) -> Ordering {
	spec.mode.do_reverse({
            if left.cache < right.cache {
		Ordering::Less
            }
            else if left.cache > right.cache {
		Ordering::Greater
            }
            else if left.complete() && right.complete() {
		Ordering::Equal
            }
	    else {
		self.comp(left.get(base), right.get(base))
	    }
	})
    }
    /// Compare two Items for equality
    fn equal_items(&self, _spec: &Comparator, base: &[u8], left: &Item, right: &Item) -> bool {
        if left.cache != right.cache {
            return false;
        }
        if left.complete() && right.complete() {
            return true;
        }
        self.equal(left.get(base), right.get(base))
    }
    /// set cache for this item
    fn fill_cache_item(&self, spec: &Comparator, item: &mut Item, base: &[u8]) {
	let line = item.get(base);
	let col = spec.get_col(line);
	self.fill_cache(item, col);
    }
}

/// Compare for a list of compares
struct CompareList {
    c: Vec<Box<dyn Compare>>,
}

/// Whole Line comparison
struct CompareWhole {
    value: Vec<u8>,
}

/// Default comparison
struct ComparePlain {
    value: Vec<u8>,
}

/// Compare by length of string
struct CompareLen {
    value: u32,
}

/// f64 comparison
struct Comparef64 {
    value: f64,
}

impl Comparef64 {
    fn new() -> Self {
        Self { value: 0.0 }
    }
}

impl CompareWhole {
    fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl ComparePlain {
    fn new() -> Self {
        Self { value: Vec::new() }
    }
}

impl CompareLen {
    fn new() -> Self {
        Self { value: 0 }
    }
}

impl Compare for CompareList {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        for x in &self.c {
            let o = x.comp(left, right);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        for x in &self.c {
            if !x.equal(left, right) {
                return false;
            }
        }
        true
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
        self.c[0].fill_cache(item, value);
        item.clear_complete();
    }
    fn set(&mut self, value: &[u8]) {
        for x in self.c.iter_mut() {
            x.set(value);
        }
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        for x in &self.c {
            let o = x.comp_self(right);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        for x in &self.c {
            if !x.equal_self(right) {
                return false;
            }
        }
        true
    }
}

impl Compare for CompareWhole {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        left.cmp(right)
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        left == right
    }
    fn fill_cache(&self, _item: &mut Item, _value: &[u8]) {
	unreachable!();
    }
    fn fill_cache_item(&self, _spec: &Comparator, item: &mut Item, base: &[u8]) {
	let line = item.get(base);
        item.cache = u64::from_be_bytes(make_array(line));
        item.assign_complete(line.len() <= 8);
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
    fn comp_cols(&self, spec: &Comparator, left: &TextLine, right: &TextLine) -> Ordering {
        spec.mode.do_reverse(self.comp(&left.line, &right.line))
    }
    fn equal_cols(&self, _spec: &Comparator, left: &TextLine, right: &TextLine) -> bool {
        self.equal(&left.line, &right.line)
    }
    fn need_split(&self) -> bool {
        false
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

fn fcmp(x: f64, y: f64) -> Ordering {
    if x == y {
        return Ordering::Equal;
    }
    if x > y {
        return Ordering::Greater;
    }
    Ordering::Less
}

fn ulp_to_ulong(d : f64) -> u64 {
    let x = u64::from_ne_bytes(d.to_ne_bytes());
    if (x & 0x8000000000000000u64) != 0 {
	x ^ 0xffffffffffffffffu64
    }
    else {
	x | 0x8000000000000000u64
    }
}

impl Compare for Comparef64 {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        fcmp(str_to_d_q(left), str_to_d_q(right))
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        str_to_d_q(left) == str_to_d_q(right)
    }
    fn fill_cache(&self, item: &mut Item, value: &[u8]) {
	item.cache = ulp_to_ulong(str_to_d_q(value));
	item.set_complete();
    }
    fn set(&mut self, value: &[u8]) {
        self.value = str_to_d_q(value);
    }
    fn comp_self(&self, right: &[u8]) -> Ordering {
        fcmp(self.value, str_to_d_q(right))
    }
    fn equal_self(&self, right: &[u8]) -> bool {
        self.value == str_to_d_q(right)
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

/// construct Comparator from text specification
pub fn make_comp(spec: &str) -> Result<Comparator> {
    let mut c = CompareSettings::new();
    if spec.is_empty() {
        c.kind = Comparison::Whole;
    } else {
	// FIXME - need left vs right
        let mut rest = c.left_col.parse(spec)?.as_bytes();
	c.right_col = c.left_col.clone();
        if !rest.is_empty() && (rest[0] == b':') {
            rest = &rest[1..];
        }
        for ch in rest {
            match ch {
                b'r' => {
                    c.reverse = true;
                }
                b'L' => {
                    c.kind = Comparison::Length;
                }
                b'g' => {
                    c.kind = Comparison::Double;
                }
                _ => {
                    return err!("Bad parse of compare spec for {}", spec);
                }
            }
        }
    }
    Ok(c.make_comp())
}

impl Item {
    /// new item
    pub fn new() -> Self {
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
    pub fn size(&self) -> u32 {
        self.size_plus & 0x7fffffff
    }
    /// test complete bit
    pub fn complete(&self) -> bool {
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
    pub fn begin(&self) -> usize {
        self.offset as usize
    }
    /// offset of end
    pub fn end(&self) -> usize {
        self.offset as usize + self.size() as usize
    }
    /// Test an Item for equality to myself
    pub fn equal(&self, other: &Item, base: &[u8]) -> bool {
        if self.cache != other.cache {
            return false;
        }
        if self.complete() && other.complete() {
            return true;
        }
        self.get(base) == other.get(base)
    }
    /// Compare Item to myself with standard ordering
    pub fn compare(&self, other: &Item, base: &[u8]) -> Ordering {
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
    pub fn compare2(&self, other: &Item, base: &[u8], cmp: &dyn Compare) -> Ordering {
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
