//! Tools for comparing lines and fields
//!  * Comparison -- an enum for comparison semantics
//!  * Compare -- a trait. Roughly one impl per Comparison
//!  * CompareSettings -- a Comparison, plus a column and other context
//!  * Comparator -- a CompareSettings, plus an instance of Compare matching the Comparison
#![allow(clippy::float_cmp)]
use crate::column::NamedCol;
use crate::text::Text;
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
    a = skip_leading_white(a);
    b = skip_leading_white(b);

    let mut left_minus = false;
    if !a.is_empty() && a[0] == b'-' {
        a = &a[1..];
        left_minus = true;
    }
    let mut right_minus = false;
    if !b.is_empty() && b[0] == b'-' {
        b = &b[1..];
        right_minus = true;
    }

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

/*
int ipnumcmp(char const * a, char const * b)
{
  while ((*a == ' ') || (*a == '\t')) ++a;
  while ((*b == ' ') || (*b == '\t')) ++b;
  while (true) {
    const char * a2 = a;
    while (ISDIGIT(*a2)) ++a2;
    const char * b2 = b;
    while (ISDIGIT(*b2)) ++b2;
    if ((a2 - a) > (b2 - b)) return 1;
    if ((a2 - a) < (b2 - b)) return -1;
    while (a < a2) {
      if (*a > *b) return 1;
      if (*a < *b) return -1;
      ++a;
      ++b;
    }
    if ((*a != '.') && (*b != '.')) return 0;
    if (*a != '.') return -1;
    if (*b != '.') return 1;
    ++a;
    ++b;
  }
}
*/
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
    let mut curr = skip_leading_white(num);
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
    let mut curr = skip_leading_white(num);
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
    let mut curr = skip_leading_white(num);
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
    /// compare nnn.nnn with no limit on length
    Numeric,
    /*
        Reverse, // compare right to left
        Lower, // ascii_tolower before comparison
        Human,  // 2.3K or 4.5G
        Fuzzy(u32 n), // integer compare, but equal if within n
        Date(String fmt), // formatted date compare
        Url, //. Compare backwards from first slash, then forwards from first slash
        Prefix, // columns are equal if either is a prefix of the other
    */
}

impl Default for Comparison {
    fn default() -> Self {
        Self::Whole
    }
}

/// Settings for one compare object
#[derive(Debug, Clone, Default)]
pub struct CompareSettings {
    /// type of comparison
    pub kind: Comparison,
    /// column to compare
    pub left_col: NamedCol,
    /// column to compare
    pub right_col: NamedCol,
    /// reverse comparison?
    pub reverse: bool,
    /// column delimiter
    pub delim: u8,
    // failure 0, -inf +inf
    // Vec<CompareSettings>
    // stable: bool
}

/// CompareSettings, with 'kind' turned into an actual object
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
            delim: b'\t', // FIXME - set this somehow
        }
    }
    /// extract column from line
    pub fn get<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        for (n, s) in data.split(|ch| *ch == self.delim).enumerate() {
            if n == self.left_col.num {
                return s;
            }
        }
        &data[0..0]
    }
    /// reverse a comparison, if reverse flag is set
    pub fn do_reverse(&self, x: Ordering) -> Ordering {
        if self.reverse {
            x.reverse()
        } else {
            x
        }
    }
    /// Resolve any named columns
    pub fn lookup_left(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.left_col.lookup(fieldnames)
    }
    /// Resolve any named columns
    pub fn lookup_right(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.right_col.lookup(fieldnames)
    }
    /// Resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
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
            Comparison::Numeric => Box::new(CompareNumeric::new()),
        }
    }
    /// make Comparator
    pub fn make_comp(&self) -> Comparator {
        Comparator::new(self.clone(), self.make_box())
    }
}

impl Default for Comparator {
    fn default() -> Self {
        Self {
            mode: CompareSettings::default(),
            comp: Box::new(CompareWhole::new()),
        }
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
    /// compare two lines for equality
    pub fn equal_lines(&self, left: &[u8], right: &[u8]) -> bool {
        self.comp.equal_lines(left, right, &self.mode)
    }
    /// compare two lines
    pub fn comp_lines(&self, left: &[u8], right: &[u8]) -> Ordering {
        self.comp.comp_lines(left, right, &self.mode)
    }
    /// resolve any named columns
    pub fn lookup_left(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.mode.lookup_left(fieldnames)
    }
    /// resolve any named columns
    pub fn lookup_right(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.mode.lookup_right(fieldnames)
    }
    /// resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
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

    /// return true if TextLine comparisons need line split into columns
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
            } else if left.cache > right.cache {
                Ordering::Greater
            } else if left.complete() && right.complete() {
                Ordering::Equal
            } else {
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
    /// compare two lines for equality
    fn equal_lines(&self, left: &[u8], right: &[u8], mode: &CompareSettings) -> bool {
        self.equal(mode.get(left), mode.get(right))
    }
    /// compare two lines
    fn comp_lines(&self, left: &[u8], right: &[u8], mode: &CompareSettings) -> Ordering {
        self.comp(mode.get(left), mode.get(right))
    }
}

/// Compare for a list of compares
#[derive(Default)]
pub struct CompareList {
    c: Vec<Box<dyn Compare>>,
}
impl fmt::Debug for CompareList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CompareList")
    }
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

/// nnn.nnn comparison
struct CompareNumeric {
    value: Vec<u8>,
}

impl CompareNumeric {
    fn new() -> Self {
        Self { value: Vec::new() }
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

impl CompareList {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// add
    pub fn push(&mut self, x: Box<dyn Compare>) {
        self.c.push(x);
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

/// construct Comparator from text specification
pub fn make_comp(spec: &str) -> Result<Comparator> {
    let mut c = CompareSettings::new();
    if spec.is_empty() {
        c.kind = Comparison::Whole;
    } else {
        // FIXME - need 'column from left file' vs 'column from right file'
        let mut rest = c.left_col.parse(spec)?;
        if !rest.is_empty() && rest.first() == '.' {
            rest = rest.skip_first();
            rest = c.right_col.parse(rest)?;
        } else {
            c.right_col = c.left_col.clone();
        }
        let mut rest = rest.as_bytes();
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
                b'n' => {
                    c.kind = Comparison::Numeric;
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

#[cfg(test)]
mod tests {
    use super::*;
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
