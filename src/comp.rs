#![allow(dead_code)]
#![allow(clippy::float_cmp)]
use super::*;
use libm;
use std::cmp::*;

//use std::convert::TryInto;

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

// https://doc.rust-lang.org/std/primitive.f64.html
pub fn str_to_d2(n: &str) -> f64 {
    n.parse().unwrap()
}

/// turn text into f64, return unused portion of text
///```
/// use cdx::comp::str_to_d;
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
    offset: u32,
    size_plus: u32,
    cache: u64,
}

#[derive(Debug, Clone)]
pub struct NamedCol {
    pub name: Vec<u8>,
    pub num: usize,
}

impl NamedCol {
    pub fn new() -> Self {
        Self {
            name: Vec::new(),
            num: 0,
        }
    }
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        if !self.name.is_empty() {
            self.num = ColumnSet::lookup_col2(fieldnames, &self.name)?
        }
        Ok(())
    }
    pub fn parse<'a>(&mut self, spec: &'a str) -> Result<&'a str> {
        self.num = 0;
        self.name.clear();
        if spec.is_empty() {
            return Ok(spec);
        }
        let ch = spec.chars().next().unwrap();
        if ch.is_digit(10) {
            let mut pos: usize = 0;
            for x in spec.chars() {
                if x.is_digit(10) {
                    self.num = self.num * 10 + (x as usize) - ('0' as usize);
                    pos += 1;
                } else {
                    break;
                }
            }
            self.num -= 1;
            Ok(&spec[pos..])
        } else if ch.is_alphabetic() {
            let mut pos: usize = 0;
            let mut tmp_name = String::new();
            for x in spec.chars() {
                if x.is_alphanumeric() || x == '_' {
                    tmp_name.push(x);
                    pos += 1;
                } else {
                    break;
                }
            }
            self.name = tmp_name.as_bytes().to_vec();
            Ok(&spec[pos..])
        } else {
            err!("Bad parse of compare spec for {}", spec)
        }
    }
}

impl Default for NamedCol {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Whole,  // bytewise comparison of whole line
    Plain,  // bytewise comparison of one column
    Length, // compare length of data
    Double, // parse as f64
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

#[derive(Debug, Clone)]
pub struct CompareSettings {
    pub kind: Comparison,
    pub column: NamedCol,
    pub reverse: bool,
    // failure 0, -inf +inf
    // Vec<CompareSettings>
    // stable: bool
}

pub struct Comparator {
    pub mode: CompareSettings,
    pub comp: Box<dyn Compare>,
}

impl fmt::Debug for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Comparator {:?}", self.mode)
    }
}

impl CompareSettings {
    pub fn new() -> Self {
        Self {
            kind: Comparison::Plain,
            column: NamedCol::new(),
            reverse: false,
        }
    }
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.column.lookup(fieldnames)
    }
    fn make_box(&self) -> Box<dyn Compare> {
        match self.kind {
            Comparison::Whole => Box::new(CompareWhole::new()),
            Comparison::Plain => Box::new(ComparePlain::new()),
            Comparison::Length => Box::new(CompareLen::new()),
            Comparison::Double => Box::new(Comparef64::new()),
        }
    }
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
    pub fn new(mode: CompareSettings, comp: Box<dyn Compare>) -> Self {
        Self { mode, comp }
    }
    pub fn equal_cols(&self, left: &TextLine, right: &TextLine) -> bool {
        self.comp.equal_cols(self, left, right)
    }
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.mode.lookup(fieldnames)
    }
    pub fn need_split(&self) -> bool {
        self.comp.need_split()
    }
}

/// method of comparing two slices
pub trait Compare {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering;
    fn equal(&self, left: &[u8], right: &[u8]) -> bool;
    fn fill_cache(&self, item: &mut Item, base: &[u8]);
    fn set(&mut self, value: &[u8]);
    fn comp_self(&self, right: &[u8]) -> Ordering;
    fn equal_self(&self, right: &[u8]) -> bool;

    fn need_split(&self) -> bool {
        true
    }
    fn comp_cols(&self, spec: &Comparator, left: &TextLine, right: &TextLine) -> Ordering {
        self.comp(
            left.get(spec.mode.column.num),
            right.get(spec.mode.column.num),
        )
    }
    fn equal_cols(&self, spec: &Comparator, left: &TextLine, right: &TextLine) -> bool {
        self.equal(
            left.get(spec.mode.column.num),
            right.get(spec.mode.column.num),
        )
    }
    fn comp_items(&self, _spec: &Comparator, base: &[u8], left: &Item, right: &Item) -> Ordering {
        if left.cache < right.cache {
            return Ordering::Less;
        }
        if left.cache > right.cache {
            return Ordering::Greater;
        }
        if left.complete() && right.complete() {
            return Ordering::Equal;
        }
        self.comp(left.get(base), right.get(base))
    }
    fn equal_items(&self, _spec: &Comparator, base: &[u8], left: &Item, right: &Item) -> bool {
        if left.cache != right.cache {
            return false;
        }
        if left.complete() && right.complete() {
            return true;
        }
        self.equal(left.get(base), right.get(base))
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
    fn fill_cache(&self, item: &mut Item, base: &[u8]) {
        self.c[0].fill_cache(item, base);
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
    fn fill_cache(&self, item: &mut Item, base: &[u8]) {
        item.cache = u64::from_be_bytes(make_array(base));
        item.assign_complete(base.len() <= 8);
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
    fn comp_cols(&self, _spec: &Comparator, left: &TextLine, right: &TextLine) -> Ordering {
        self.comp(&left.line, &right.line)
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
    fn fill_cache(&self, item: &mut Item, base: &[u8]) {
        item.cache = u64::from_be_bytes(make_array(base));
        item.assign_complete(base.len() <= 8);
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
impl Compare for Comparef64 {
    fn comp(&self, left: &[u8], right: &[u8]) -> Ordering {
        fcmp(str_to_d_q(left), str_to_d_q(right))
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        str_to_d_q(left) == str_to_d_q(right)
    }
    fn fill_cache(&self, _item: &mut Item, _base: &[u8]) {
        //        item.cache = u64::from_be_bytes(make_array(base));
        //        item.clear_complete(base.len() <= 8);
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
    fn fill_cache(&self, item: &mut Item, base: &[u8]) {
        item.cache = base.len() as u64;
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

pub fn make_comp(spec: &str) -> Result<Comparator> {
    let mut c = CompareSettings::new();
    if spec.is_empty() {
        c.kind = Comparison::Whole;
    } else {
        let mut rest = c.column.parse(spec)?.as_bytes();
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
    pub fn new() -> Self {
        Self {
            offset: 0,
            size_plus: 0,
            cache: 0,
        }
    }
    pub fn get<'a>(&self, base: &'a [u8]) -> &'a [u8] {
        &base[self.begin()..self.end()]
    }
    pub fn size(&self) -> u32 {
        self.size_plus & 0x7fffffff
    }
    pub fn complete(&self) -> bool {
        (self.size_plus & 0x10000000) != 0
    }
    pub fn set_complete(&mut self) {
        self.size_plus |= 0x10000000;
    }
    pub fn clear_complete(&mut self) {
        self.size_plus &= 0x7fffffff;
    }
    pub fn assign_complete(&mut self, tag: bool) {
        if tag {
            self.set_complete();
        } else {
            self.clear_complete();
        }
    }
    pub fn begin(&self) -> usize {
        self.offset as usize
    }
    pub fn end(&self) -> usize {
        self.offset as usize + self.size() as usize
    }
    pub fn equal(&self, other: &Item, base: &[u8]) -> bool {
        if self.cache != other.cache {
            return false;
        }
        if self.complete() && other.complete() {
            return true;
        }
        self.get(base) == other.get(base)
    }
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
