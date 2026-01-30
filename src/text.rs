//! The Text trait, with implementations for str and &[u8]
//! for all those things that are needed for both types

use crate::num::{suffix_valf, suffix_valu, Junk, JunkType};
use crate::prelude::*;
use std::ops::Range;

/// Case Sensitive or not
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Default)]
pub enum Case {
    /// Case Sensitive
    #[default]
    Sens,
    /// Case Insensitive
    Insens,
}

/// start of range contianing last 'num' parts delimited by 'delim'
fn tail_u8_len(buff: &[u8], num: usize, delim: u8) -> usize {
    let mut left = num;
    if left == 0 {
        return buff.len();
    }
    for pos in (0..buff.len()).rev() {
        if buff[pos] == delim {
            left -= 1;
            if left == 0 {
                return pos + 1;
            }
        }
    }
    0
}

/// abstraction of str and [u8]
pub trait Text {
    /// the underlying type of the slice, e.g. char or u8
    type Char: PartialEq + Copy;
    /// A container for the underlying type, e.g. String or Vec<u8>
    type Container;

    /// is the slice empty?
    fn is_empty(&self) -> bool;
    /// return an empty slice
    fn empty(&self) -> &Self;
    /// return first character in the slice
    fn first(&self) -> Self::Char;
    /// return last byte in the slice
    fn last_byte(&self) -> u8;
    /// return all but first character in the slice
    fn skip_first(&self) -> &Self;
    /// create a character from an ascii value
    fn ascii(&self, ch: u8) -> Self::Char;
    /// are two characters equal FIXME, should be `is_XXX`
    fn chars_equal(&self, c1: Self::Char, c2: Self::Char, ic: Case) -> bool;
    /// return first character and remove it from slice
    fn take_first(self: &mut &Self) -> Self::Char;
    /// extract slice
    fn get(&self, r: Range<usize>) -> &Self;
    /// case insensitive equality
    fn equal_insens(&self, other: &Self) -> bool;
    /// case insensitive equality, self is already lowercase
    fn equal_insens_quick(&self, other: &Self) -> bool;
    /// case insensitive ordering
    fn cmp_insens(&self, other: &Self) -> Ordering;
    /// case insensitive ordering, self is already lowercase
    fn cmp_insens_quick(&self, other: &Self) -> Ordering;

    /// assign lowercase version of self to dst
    fn assign_lower(&self, dst: &mut Self::Container);
    /// new lowercase version of self
    fn new_lower(&self) -> Self::Container;

    /// assign lowercase version of self to dst
    fn assign_upper(&self, dst: &mut Self::Container);
    /// new lowercase version of self
    fn new_upper(&self) -> Self::Container;
    /// is white space
    fn is_blank(&self, ch: Self::Char) -> bool;
    /// length in bytes
    fn len(&self) -> usize;
    /// lenght in characters
    fn num_chars(&self) -> usize;
    /// bytes
    fn as_bytes(&self) -> &[u8];
    /// trim whitespace
    fn trimw(&self) -> &Self;
    /// trim whitespace at start of slice
    fn trimw_start(&self) -> &Self;
    /// convert to printable str
    fn to_str(&self) -> std::borrow::Cow<'_, str>;
    /// convert to usize
    fn to_usize(&self) -> (usize, &Self);
    /// `to_usize`, but simply retrj best guess
    fn to_usize_lossy(&self) -> usize {
        self.to_usize().0
    }
    /// `to_usize`, but fail if any text remains
    fn to_usize_whole(&self, spec: &[u8], what: &str) -> Result<usize> {
        let (val, rest) = self.to_usize();
        if !rest.is_empty() {
            err!(
                "Malformed unsigned integer '{}' trying to make '{what}' from '{}'",
                self.to_str(),
                String::from_utf8_lossy(spec)
            )
        } else {
            Ok(val)
        }
    }
    /// `bytes_to_d`, returning junk value as appropriate
    fn to_usize_junk(&self, junk: &Junk) -> usize {
        let (val, rest) = self.to_usize();
        match junk.junk_type {
            JunkType::Any => val,
            JunkType::Trailing => {
                if self.len() != rest.len() {
                    val
                } else {
                    junk.val()
                }
            }
            JunkType::None => {
                if rest.is_empty() && !self.is_empty() {
                    val
                } else {
                    junk.val()
                }
            }
        }
    }
    /// convert to isize
    fn to_isize(&self) -> (isize, &Self);
    /// `to_isize`, but simply retrj best guess
    fn to_isize_lossy(&self) -> isize {
        self.to_isize().0
    }
    /// `to_isize`, but fail if any text remains
    fn to_isize_whole(&self, spec: &[u8], what: &str) -> Result<isize> {
        let (val, rest) = self.to_isize();
        if !rest.is_empty() {
            err!(
                "Malformed signed integer '{}' trying to make '{what}' from '{}'",
                self.to_str(),
                String::from_utf8_lossy(spec)
            )
        } else {
            Ok(val)
        }
    }
    /// `bytes_to_d`, returning junk value as appropriate
    fn to_isize_junk(&self, junk: &Junk) -> isize {
        let (val, rest) = self.to_isize();
        match junk.junk_type {
            JunkType::Any => val,
            JunkType::Trailing => {
                if self.len() != rest.len() {
                    val
                } else {
                    junk.val()
                }
            }
            JunkType::None => {
                if rest.is_empty() && !self.is_empty() {
                    val
                } else {
                    junk.val()
                }
            }
        }
    }
    /// convert to f64, return value and remainder
    fn to_f64(&self) -> (f64, &Self);
    /// `to_f64`, but simply return best guess
    fn to_f64_lossy(&self) -> f64 {
        self.to_f64().0
    }
    /// `to_f64`, but fail if any text remains
    fn to_f64_whole(&self, spec: &[u8], what: &str) -> Result<f64> {
        let (val, rest) = self.to_f64();
        if !rest.is_empty() {
            err!(
                "Malformed float '{}' trying to make '{what}' from '{}'",
                self.to_str(),
                String::from_utf8_lossy(spec)
            )
        } else {
            Ok(val)
        }
    }
    /// `bytes_to_d`, returning junk value as appropriate
    fn to_f64_junk(&self, junk: &Junk) -> f64 {
        let (val, rest) = self.to_f64();
        match junk.junk_type {
            JunkType::Any => val,
            JunkType::Trailing => {
                if self.len() != rest.len() {
                    val
                } else {
                    junk.val()
                }
            }
            JunkType::None => {
                if rest.is_empty() && !self.is_empty() {
                    val
                } else {
                    junk.val()
                }
            }
        }
    }

    /// match in glob format
    fn glob(&self, in_wild: &Self, ic: Case) -> bool {
        let mut buff: &Self = self;
        let mut wild: &Self = in_wild;

        #[allow(clippy::suspicious_operation_groupings)] // clippy false positive
        while !buff.is_empty() && !wild.is_empty() && (wild.first() != self.ascii(b'*')) {
            if !self.chars_equal(wild.first(), buff.first(), ic) {
                return false;
            }
            wild = wild.skip_first();
            buff = buff.skip_first();
        }
        if wild.is_empty() && !buff.is_empty() {
            return false;
        }

        let mut cp: &Self = self.empty();
        let mut mp: &Self = self.empty();

        while !buff.is_empty() {
            if !wild.is_empty() && (wild.first() == self.ascii(b'*')) {
                wild = wild.skip_first();
                if wild.is_empty() {
                    return true;
                }
                mp = wild;
                cp = buff.skip_first();
            } else if !wild.is_empty() && self.chars_equal(wild.first(), buff.first(), ic) {
                wild = wild.skip_first();
                buff = buff.skip_first();
            } else {
                wild = mp;
                cp = cp.skip_first();
                buff = cp;
            }
        }

        while !wild.is_empty() && (wild.first() == self.ascii(b'*')) {
            wild = wild.skip_first();
        }
        wild.is_empty()
    }
    /// return last 'num' elements, delimited by 'delim'
    fn tail_u8(&self, num: usize, delim: u8) -> &Self;
    /// return first 'num' elements, delimited by 'delim'
    fn head(&self, num: usize, delim: Self::Char) -> &Self {
        let mut left = num;
        if left == 0 || self.is_empty() {
            return self.empty();
        }
        let mut rest = self;
        while !rest.is_empty() {
            if rest.first() == delim {
                left -= 1;
                if left == 0 {
                    return self.get(0..self.len() - rest.len());
                }
            }
            rest = rest.skip_first();
        }
        self
    }
    /// return last 'num' elements, delimited by 'delim'
    fn tail_path_u8(&self, num: usize, delim: u8) -> &Self {
        if self.is_empty() {
            return self.empty();
        }
        if self.last_byte() == delim {
            self.tail_u8(num + 1, delim)
        } else {
            self.tail_u8(num, delim)
        }
    }
    /// return first 'num' elements, delimited by 'delim'
    /// but if the first char is the delim, don't count it
    fn head_path(&self, num: usize, delim: Self::Char) -> &Self {
        if self.is_empty() {
            return self.empty();
        }
        if self.first() == delim {
            self.head(num + 1, delim)
        } else {
            self.head(num, delim)
        }
    }
}
/*
/// compare for equality, ignoring case.
fn equal_nocase(a: &str, b: &str) -> bool {
    for ch in a
        .chars()
        .flat_map(char::to_lowercase)
        .zip(b.chars().flat_map(char::to_lowercase))
    {
        if ch.0 != ch.1 {
            return false;
    }
    }
    a.len() == b.len()
}

*/
impl Text for str {
    type Char = char;
    type Container = String;

    fn len(&self) -> usize {
        self.len()
    }
    fn num_chars(&self) -> usize {
        self.chars().count()
    }
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes()
    }

    fn ascii(&self, ch: u8) -> Self::Char {
        char::from_u32(u32::from(ch)).unwrap()
    }
    fn empty(&self) -> &Self {
        ""
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn first(&self) -> char {
        debug_assert!(!self.is_empty());
        self.chars().next().unwrap()
    }

    fn trimw(&self) -> &Self {
        self.trim()
    }
    fn trimw_start(&self) -> &Self {
        self.trim_start()
    }
    fn skip_first(&self) -> &Self {
        debug_assert!(!self.is_empty());
        &self[self.chars().next().unwrap().len_utf8()..]
    }
    fn is_blank(&self, ch: Self::Char) -> bool {
        ch <= ' ' || ch.is_whitespace()
    }

    fn chars_equal(&self, c1: char, c2: char, ic: Case) -> bool {
        if c1 == c2 {
            return true;
        }
        if c1 == '?' {
            return true;
        }
        if ic == Case::Sens {
            return false;
        }
        c1.to_lowercase().eq(c2.to_lowercase())
    }

    fn take_first(self: &mut &Self) -> char {
        debug_assert!(!self.is_empty());
        let x = self.chars().next().unwrap();
        *self = &self[x.len_utf8()..];
        x
    }

    fn equal_insens(&self, other: &Self) -> bool {
        for ch in self
            .chars()
            .flat_map(char::to_lowercase)
            .zip(other.chars().flat_map(char::to_lowercase))
        {
            if ch.0 != ch.1 {
                return false;
            }
        }
        self.len() == other.len()
    }

    fn equal_insens_quick(&self, other: &Self) -> bool {
        for ch in self.chars().zip(other.chars().flat_map(char::to_lowercase)) {
            if ch.0 != ch.1 {
                return false;
            }
        }
        self.len() == other.len()
    }

    fn cmp_insens(&self, other: &Self) -> Ordering {
        for ch in self
            .chars()
            .flat_map(char::to_lowercase)
            .zip(other.chars().flat_map(char::to_lowercase))
        {
            if ch.0 < ch.1 {
                return Ordering::Less;
            }
            if ch.0 > ch.1 {
                return Ordering::Greater;
            }
        }
        self.len().cmp(&other.len())
    }

    fn cmp_insens_quick(&self, other: &Self) -> Ordering {
        for ch in self.chars().zip(other.chars().flat_map(char::to_lowercase)) {
            if ch.0 < ch.1 {
                return Ordering::Less;
            }
            if ch.0 > ch.1 {
                return Ordering::Greater;
            }
        }
        self.len().cmp(&other.len())
    }
    fn assign_lower(&self, dst: &mut String) {
        dst.clear();
        dst.extend(self.chars().flat_map(char::to_lowercase));
    }
    fn new_lower(&self) -> String {
        let mut dst = String::new();
        self.assign_lower(&mut dst);
        dst
    }
    fn assign_upper(&self, dst: &mut String) {
        dst.clear();
        dst.extend(self.chars().flat_map(char::to_uppercase));
    }
    fn new_upper(&self) -> String {
        let mut dst = String::new();
        self.assign_upper(&mut dst);
        dst
    }
    fn tail_u8(&self, num: usize, delim: u8) -> &Self {
        &self[tail_u8_len(self.as_bytes(), num, delim)..]
    }
    fn get(&self, r: Range<usize>) -> &Self {
        &self[r]
    }
    fn last_byte(&self) -> u8 {
        self.as_bytes()[self.len() - 1]
    }
    fn to_str(&self) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::from(self)
    }
    fn to_usize(&self) -> (usize, &Self) {
        let (a, b) = self.as_bytes().to_usize();
        (a, &self[self.len() - b.len()..])
    }
    fn to_isize(&self) -> (isize, &Self) {
        let (a, b) = self.as_bytes().to_isize();
        (a, &self[self.len() - b.len()..])
    }
    fn to_f64(&self) -> (f64, &Self) {
        let (a, b) = self.as_bytes().to_f64();
        (a, &self[self.len() - b.len()..])
    }
}

const fn is_exp(buf: &[u8]) -> bool {
    if buf.len() < 2 {
        return false;
    }
    if buf[0] != b'e' && buf[0] != b'E' {
        return false;
    }
    if buf[1].is_ascii_digit() {
        return true;
    }
    if buf[1] != b'+' && buf[1] != b'-' {
        return false;
    }
    if buf.len() < 3 {
        return false;
    }
    buf[2].is_ascii_digit()
}

impl Text for [u8] {
    type Char = u8;
    type Container = Vec<u8>;

    fn len(&self) -> usize {
        self.len()
    }
    fn num_chars(&self) -> usize {
        self.len()
    }
    fn as_bytes(&self) -> &[u8] {
        self
    }
    fn ascii(&self, ch: u8) -> Self::Char {
        ch
    }
    fn empty(&self) -> &Self {
        b""
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn first(&self) -> u8 {
        debug_assert!(!self.is_empty());
        self[0]
    }

    fn trimw(&self) -> &Self {
        let from = match self.iter().position(|x| *x > b' ') {
            Some(i) => i,
            None => return &self[0..0],
        };
        let to = self.iter().rposition(|x| *x > b' ').unwrap();
        &self[from..=to]
    }
    fn trimw_start(&self) -> &Self {
        match self.iter().position(|x| *x > b' ') {
            Some(i) => &self[i..],
            None => &self[0..0],
        }
    }
    fn skip_first(&self) -> &[u8] {
        debug_assert!(!self.is_empty());
        &self[1..]
    }
    fn is_blank(&self, ch: Self::Char) -> bool {
        ch <= b' '
    }

    fn chars_equal(&self, c1: u8, c2: u8, ic: Case) -> bool {
        if c1 == c2 {
            return true;
        }
        if c1 == b'?' {
            return true;
        }
        if ic == Case::Sens {
            return false;
        }
        c1.eq_ignore_ascii_case(&c2)
    }

    fn take_first(self: &mut &Self) -> u8 {
        debug_assert!(!self.is_empty());
        let x = self[0];
        *self = &self[1..];
        x
    }
    fn equal_insens(&self, other: &Self) -> bool {
        self.eq_ignore_ascii_case(other)
    }
    fn equal_insens_quick(&self, other: &Self) -> bool {
        self.eq_ignore_ascii_case(other)
    }
    fn cmp_insens(&self, other: &Self) -> Ordering {
        for ch in self
            .iter()
            .map(u8::to_ascii_lowercase)
            .zip(other.iter().map(u8::to_ascii_lowercase))
        {
            if ch.0 < ch.1 {
                return Ordering::Less;
            }
            if ch.0 > ch.1 {
                return Ordering::Greater;
            }
        }
        self.len().cmp(&other.len())
    }
    fn cmp_insens_quick(&self, other: &Self) -> Ordering {
        for ch in self.iter().zip(other.iter().map(u8::to_ascii_lowercase)) {
            if ch.0 < &ch.1 {
                return Ordering::Less;
            }
            if ch.0 > &ch.1 {
                return Ordering::Greater;
            }
        }
        self.len().cmp(&other.len())
    }
    fn assign_lower(&self, dst: &mut Vec<u8>) {
        dst.clear();
        dst.extend(self.iter().map(u8::to_ascii_lowercase));
    }
    fn new_lower(&self) -> Vec<u8> {
        let mut dst = Vec::new();
        self.assign_lower(&mut dst);
        dst
    }
    fn assign_upper(&self, dst: &mut Vec<u8>) {
        dst.clear();
        dst.extend(self.iter().map(u8::to_ascii_uppercase));
    }
    fn new_upper(&self) -> Vec<u8> {
        let mut dst = Vec::new();
        self.assign_upper(&mut dst);
        dst
    }
    fn tail_u8(&self, num: usize, delim: u8) -> &Self {
        &self[tail_u8_len(self, num, delim)..]
    }
    fn get(&self, r: Range<usize>) -> &Self {
        &self[r]
    }
    fn last_byte(&self) -> u8 {
        self[self.len() - 1]
    }
    fn to_str(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(self)
    }
    fn to_usize(&self) -> (usize, &Self) {
        let mut curr = self;
        if !curr.is_empty() && curr[0] == b'+' {
            curr = &curr[1..];
        }
        let mut ret: usize = 0;
        if curr.is_empty() || !curr[0].is_ascii_digit() {
            return (0, self);
        }
        while !curr.is_empty() && curr[0].is_ascii_digit() {
            ret *= 10;
            ret += (curr[0] - b'0') as usize;
            curr = &curr[1..];
        }
        if !curr.is_empty() {
            if let Some(mul) = suffix_valu(curr[0]) {
                curr = &curr[1..];
                ret *= mul;
            }
        }
        (ret, curr)
    }
    fn to_isize(&self) -> (isize, &Self) {
        let mut neg: isize = 1;
        let mut curr = self;
        if !curr.is_empty() {
            if curr[0] == b'+' {
                curr = &curr[1..];
            } else if curr[0] == b'-' {
                curr = &curr[1..];
                neg = -1;
            }
        }
        let mut ret: isize = 0;
        if curr.is_empty() || !curr[0].is_ascii_digit() {
            return (0, self);
        }
        while !curr.is_empty() && curr[0].is_ascii_digit() {
            ret *= 10;
            ret += (curr[0] - b'0') as isize;
            curr = &curr[1..];
        }
        if !curr.is_empty() {
            if let Some(mul) = suffix_valu(curr[0]) {
                curr = &curr[1..];
                ret *= mul as isize;
            }
        }
        ret *= neg;
        (ret, curr)
    }
    fn to_f64(&self) -> (f64, &Self) {
        let mut neg: f64 = 1.0;
        let mut curr = self;
        if curr.is_empty() {
            return (0.0, self);
        }
        if curr[0] == b'+' {
            curr = &curr[1..];
        } else if curr[0] == b'-' {
            curr = &curr[1..];
            neg = -1.0;
        }
        let mut ret: f64 = 0.0;
        while !curr.is_empty() && curr[0].is_ascii_digit() {
            ret *= 10.0;
            ret += f64::from(curr[0] - b'0');
            curr = &curr[1..];
        }
        if !curr.is_empty() && (curr[0] == b'.') {
            curr = &curr[1..];
            let mut place = 0.1;
            while !curr.is_empty() && curr[0].is_ascii_digit() {
                ret += place * f64::from(curr[0] - b'0');
                place *= 0.1;
                curr = &curr[1..];
            }
        }

        if is_exp(curr) {
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
                exponent += i32::from(curr[0] - b'0');
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
                    ret *= libm::exp10(f64::from(exponent));
                }
            }
        }
        if !curr.is_empty() {
            if let Some(mul) = suffix_valf(curr[0]) {
                curr = &curr[1..];
                ret *= mul;
            }
        }
        (neg * ret, curr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers() {
        let (x, y) = "123.456xyz".to_f64();
        assert_eq!(x, 123.456);
        assert_eq!(y, "xyz");
        let (x, y) = "123e2".to_f64();
        assert_eq!(y, "");
        assert_eq!(x, 123e2);
        let (x, y) = "123e-27".to_f64();
        assert_eq!(y, "");
        assert_eq!(x, 123e-27);
    }

    #[test]
    fn head_path() {
        assert_eq!("aa.bb".head_path(0, '.'), "");
        assert_eq!("aa.bb".head_path(1, '.'), "aa");
        assert_eq!("aa.bb".head_path(2, '.'), "aa.bb");
        assert_eq!("aa.bb".head_path(3, '.'), "aa.bb");
        assert_eq!(".aa.bb".head_path(0, '.'), "");
        assert_eq!(".aa.bb".head_path(1, '.'), ".aa");
        assert_eq!(".aa.bb".head_path(2, '.'), ".aa.bb");
        assert_eq!(".aa.bb".head_path(3, '.'), ".aa.bb");
    }

    #[test]
    fn tail_path_u8() {
        assert_eq!("aa.bb".tail_path_u8(0, b'.'), "");
        assert_eq!("aa.bb".tail_path_u8(1, b'.'), "bb");
        assert_eq!("aa.bb".tail_path_u8(2, b'.'), "aa.bb");
        assert_eq!("aa.bb".tail_path_u8(3, b'.'), "aa.bb");
        assert_eq!("aa.bb.".tail_path_u8(0, b'.'), "");
        assert_eq!("aa.bb.".tail_path_u8(1, b'.'), "bb.");
        assert_eq!("aa.bb.".tail_path_u8(2, b'.'), "aa.bb.");
        assert_eq!("aa.bb.".tail_path_u8(3, b'.'), "aa.bb.");
    }

    #[test]
    fn head() {
        assert_eq!(b"aa.bb.cc".head(0, b'.'), b"");
        assert_eq!(b"aa.bb.cc".head(1, b'.'), b"aa");
        assert_eq!(b"aa.bb.cc".head(2, b'.'), b"aa.bb");
        assert_eq!(b"aa.bb.cc".head(3, b'.'), b"aa.bb.cc");
        assert_eq!(b"aa.bb.cc".head(4, b'.'), b"aa.bb.cc");

        assert_eq!("aa.bb.cc".head(0, '.'), "");
        assert_eq!("aa.bb.cc".head(1, '.'), "aa");
        assert_eq!("aa.bb.cc".head(2, '.'), "aa.bb");
        assert_eq!("aa.bb.cc".head(3, '.'), "aa.bb.cc");
        assert_eq!("aa.bb.cc".head(4, '.'), "aa.bb.cc");

        assert_eq!(b"".head(1, b'.'), b"");
        assert_eq!(b".".head(1, b'.'), b"");
        assert_eq!(b".".head(2, b'.'), b".");
        assert_eq!(b".aa".head(1, b'.'), b"");
        assert_eq!(b".aa".head(2, b'.'), b".aa");
        assert_eq!(b".aa".head(3, b'.'), b".aa");
        assert_eq!(b".aa.".head(1, b'.'), b"");
        assert_eq!(b".aa.".head(2, b'.'), b".aa");
        assert_eq!(b".aa.".head(3, b'.'), b".aa.");
        assert_eq!(b".aa.".head(4, b'.'), b".aa.");
        assert_eq!(b".".head(1, b'.'), b"");
    }

    #[test]
    fn tail_u8() {
        assert_eq!(b"aa.bb.cc".tail_u8(0, b'.'), b"");
        assert_eq!(b"aa.bb.cc".tail_u8(1, b'.'), b"cc");
        assert_eq!(b"aa.bb.cc".tail_u8(2, b'.'), b"bb.cc");
        assert_eq!(b"aa.bb.cc".tail_u8(3, b'.'), b"aa.bb.cc");
        assert_eq!(b"aa.bb.cc".tail_u8(4, b'.'), b"aa.bb.cc");

        assert_eq!("aa.bb.cc".tail_u8(0, b'.'), "");
        assert_eq!("aa.bb.cc".tail_u8(1, b'.'), "cc");
        assert_eq!("aa.bb.cc".tail_u8(2, b'.'), "bb.cc");
        assert_eq!("aa.bb.cc".tail_u8(3, b'.'), "aa.bb.cc");
        assert_eq!("aa.bb.cc".tail_u8(4, b'.'), "aa.bb.cc");

        assert_eq!(b"".tail_u8(1, b'.'), b"");
        assert_eq!(b".".tail_u8(1, b'.'), b"");
        assert_eq!(b".".tail_u8(2, b'.'), b".");
        assert_eq!(b"aa.".tail_u8(1, b'.'), b"");
        assert_eq!(b"aa.".tail_u8(2, b'.'), b"aa.");
        assert_eq!(b"aa.".tail_u8(3, b'.'), b"aa.");
        assert_eq!(b".aa.".tail_u8(1, b'.'), b"");
        assert_eq!(b".aa.".tail_u8(2, b'.'), b"aa.");
        assert_eq!(b".aa.".tail_u8(3, b'.'), b".aa.");
        assert_eq!(b".aa.".tail_u8(4, b'.'), b".aa.");
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn uwild() {
        assert!(b"".glob(b"", Case::Sens));
        assert!(!b"".glob(b"foo", Case::Sens));
        assert!(!b"foo".glob(b"", Case::Sens));
        assert!(b"foo".glob(b"*", Case::Sens));
        assert!(b"".glob(b"*", Case::Sens));

        assert!(b"foo".glob(b"foo", Case::Sens));
        assert!(!b"bar".glob(b"foo", Case::Sens));
        assert!(b"FoO".glob(b"foo", Case::Insens));
        assert!(!b"FoO".glob(b"bar", Case::Insens));

        assert!(!b"foo.txtt".glob(b"*.txt", Case::Sens));
        assert!(b"foo.txt".glob(b"*.txt", Case::Sens));
        assert!(b"foo.txt".glob(b"foo*.txt", Case::Sens));
        assert!(!b"foo.txtt".glob(b"foo*.txt", Case::Sens));
        assert!(b"foo.txt".glob(b"foo?txt", Case::Sens));
        assert!(!b"foo..txt".glob(b"foo?txt", Case::Sens));

        assert!(b"foO.txt".glob(b"*.txt", Case::Insens));
        assert!(!b"foO.txtt".glob(b"*.txt", Case::Insens));
        assert!(b"foO.txt".glob(b"foo*.txt", Case::Insens));
        assert!(!b"foO.txtt".glob(b"foo*.txt", Case::Insens));
        assert!(b"foO.txt".glob(b"foo?txt", Case::Insens));
        assert!(!b"foO..txt".glob(b"foo?txt", Case::Insens));

        assert!(b"anb".glob(b"a?b", Case::Sens));
        assert!(!"añb".as_bytes().glob(b"a?b", Case::Sens));

        assert!(b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz".glob(
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/*/add_hour_*", Case::Sens));

        assert!(b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz".glob(
	    b"s3://com.company.metric-holding-bin-prod//2011/02/12/23/00/acctid/???/add_hour_*", Case::Sens));
    }

    #[test]
    fn swild() {
        assert!("".glob("", Case::Sens));
        assert!(!"".glob("foo", Case::Sens));
        assert!(!"foo".glob("", Case::Sens));

        assert!("foo".glob("*", Case::Sens));
        assert!("".glob("*", Case::Sens));

        assert!("foo".glob("foo", Case::Sens));
        assert!(!"foo".glob("bar", Case::Sens));
        assert!("FoO".glob("foo", Case::Insens));
        assert!(!"FoO".glob("bar", Case::Sens));

        assert!(!"foo.txtt".glob("*.txt", Case::Sens));
        assert!("foo.txt".glob("*.txt", Case::Sens));
        assert!("foo.txt".glob("foo*.txt", Case::Sens));
        assert!(!"foo.txtt".glob("foo*.txt", Case::Sens));
        assert!("foo.txt".glob("foo?txt", Case::Sens));
        assert!(!"foo..txt".glob("foo?txt", Case::Sens));

        assert!("foO.txt".glob("*.txt", Case::Insens));
        assert!(!"foO.txtt".glob("*.txt", Case::Insens));
        assert!("foO.txt".glob("foo*.txt", Case::Insens));
        assert!(!"foO.txtt".glob("foo*.txt", Case::Insens));
        assert!("foO.txt".glob("foo?txt", Case::Insens));
        assert!(!"foO..txt".glob("foo?txt", Case::Insens));

        assert!("anb".glob("a?b", Case::Sens));
        assert!("añb".glob("a?b", Case::Sens));
    }
    #[test]
    fn swild2() {
        assert!(!"aÑb".glob("añb", Case::Sens));
        assert!(!"añb".glob("aÑb", Case::Sens));
        assert!("aÑb".glob("añb", Case::Insens));
        assert!("añb".glob("aÑb", Case::Insens));

        assert!("s3://com.company.metric-holding-bin-prod/2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz".glob(
	    "s3://com.company.metric-holding-bin-prod/2011/02/12/23/00/acctid/*/add_hour_*", Case::Sens));
        assert!("s3://com.company.metric-holding-bin-prod/2011/02/12/23/00/acctid/2da/add_hour_201102122300_2da.gz".glob(
	    "s3://com.company.metric-holding-bin-prod/2011/02/12/23/00/acctid/???/add_hour_*", Case::Sens));
    }
}
