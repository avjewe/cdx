//! Numeric Helpers

use crate::text::Text;
use crate::util::{err, Error, Result};
use libm;
use std::cmp::Ordering;

/// amount of junk allowed in a number, before falling back to the 'error' value
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum JunkType {
    /// best effort for any input
    Any,
    /// trailing junk is ok, but must have a valid prefix
    Trailing,
    /// every byte must be part of a valid value
    None,
}
impl Default for JunkType {
    fn default() -> Self {
        Self::Any
    }
}

/// Value to assign to junk values
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum JunkVal {
    /// if value is junk, make it compare less
    Min,
    /// if value is junk, make it compare greater
    Max,
}
impl Default for JunkVal {
    fn default() -> Self {
        Self::Max
    }
}

/// return appropriate value for type and JunkVal
pub trait JunkValue {
    /// return appropriate value for type and JunkVal
    fn val(j: JunkVal) -> Self;
}

macro_rules! jv {
    ($e:ty) => {
        impl JunkValue for $e {
            fn val(j: JunkVal) -> Self {
                if j == JunkVal::Min {
                    Self::MIN
                } else {
                    Self::MAX
                }
            }
        }
    };
}
jv!(f64);
jv!(i64);
jv!(u64);

/// What counts as junk, and what to do about it
#[derive(Debug, Default, Copy, Clone)]
pub struct Junk {
    /// what constitutes junk
    pub junk_type: JunkType,
    /// what do do with junk
    pub junk_val: JunkVal,
}
impl Junk {
    /// return value associated with JunkVal
    pub fn val<T: JunkValue>(&self) -> T {
        T::val(self.junk_val)
    }
}

// https://doc.rust-lang.org/std/primitive.f64.html

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

/// str_to_d, but always return best guess
pub fn str_to_d_lossy(num: &[u8]) -> f64 {
    str_to_d(num).0
}

/// str_to_i, but always return best guess
pub fn str_to_i_lossy(num: &[u8]) -> i64 {
    str_to_i(num).0
}

/// str_to_u, but always return best guess
pub fn str_to_u_lossy(num: &[u8]) -> u64 {
    str_to_u(num).0
}

/// str_to_d, but fail if any text remains
pub fn str_to_d_whole(num: &[u8]) -> Result<f64> {
    let (val, rest) = str_to_d(num);
    if !rest.is_empty() {
        err!(
            "Malformed floating point number {}",
            String::from_utf8_lossy(num)
        )
    } else {
        Ok(val)
    }
}

/// str_to_i, but fail if any text remains
pub fn str_to_i_whole(num: &[u8]) -> Result<i64> {
    let (val, rest) = str_to_i(num);
    if !rest.is_empty() {
        err!(
            "Malformed floating point number {}",
            String::from_utf8_lossy(num)
        )
    } else {
        Ok(val)
    }
}

/// str_to_u, but fail if any text remains
pub fn str_to_u_whole(num: &[u8]) -> Result<u64> {
    let (val, rest) = str_to_u(num);
    if !rest.is_empty() {
        err!(
            "Malformed floating point number {}",
            String::from_utf8_lossy(num)
        )
    } else {
        Ok(val)
    }
}

/// str_to_d, returning junk value as appropriate
pub fn str_to_d_junk(num: &[u8], junk: &Junk) -> f64 {
    let (val, rest) = str_to_d(num);
    match junk.junk_type {
        JunkType::Any => val,
        JunkType::Trailing => {
            if num.len() != rest.len() {
                val
            } else {
                junk.val()
            }
        }
        JunkType::None => {
            if rest.is_empty() && !num.is_empty() {
                val
            } else {
                junk.val()
            }
        }
    }
}

/// str_to_i, returning junk value as appropriate
pub fn str_to_i_junk(num: &[u8], junk: &Junk) -> i64 {
    let (val, rest) = str_to_i(num);
    match junk.junk_type {
        JunkType::Any => val,
        JunkType::Trailing => {
            if num.len() != rest.len() {
                val
            } else {
                junk.val()
            }
        }
        JunkType::None => {
            if rest.is_empty() && !num.is_empty() {
                val
            } else {
                junk.val()
            }
        }
    }
}

/// str_to_u, returning junk value as appropriate
pub fn str_to_u_junk(num: &[u8], junk: &Junk) -> u64 {
    let (val, rest) = str_to_u(num);
    match junk.junk_type {
        JunkType::Any => val,
        JunkType::Trailing => {
            if num.len() != rest.len() {
                val
            } else {
                junk.val()
            }
        }
        JunkType::None => {
            if rest.is_empty() && !num.is_empty() {
                val
            } else {
                junk.val()
            }
        }
    }
}

/// Convert bytes to integer, return unused portion
pub fn str_to_i(num: &[u8]) -> (i64, &[u8]) {
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
        return (0, num);
    }
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        ret *= 10;
        ret += (curr[0] - b'0') as i64;
        curr = &curr[1..];
    }
    ret *= neg;
    (ret, curr)
}

/// Convert bytes to integer, return unused portion
pub fn str_to_u(num: &[u8]) -> (u64, &[u8]) {
    let mut curr = num.trimw_start();
    if !curr.is_empty() && curr[0] == b'+' {
        curr = &curr[1..];
    }
    let mut ret: u64 = 0;
    if curr.is_empty() || !curr[0].is_ascii_digit() {
        return (0, num);
    }
    while !curr.is_empty() && curr[0].is_ascii_digit() {
        ret *= 10;
        ret += (curr[0] - b'0') as u64;
        curr = &curr[1..];
    }
    (ret, curr)
}

/// turn text into f64, return unused portion of text
///```
/// use cdx::num::str_to_d;
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
pub fn str_to_d(num: &[u8]) -> (f64, &[u8]) {
    let mut neg: f64 = 1.0;
    let mut curr = num.trimw_start();
    if curr.is_empty() {
        return (0.0, num);
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

/// ordering for f64
pub fn fcmp(x: f64, y: f64) -> Ordering {
    if x == y {
        return Ordering::Equal;
    }
    if x > y {
        return Ordering::Greater;
    }
    Ordering::Less
}

/// convert an f64 to a similarly ordered u64
pub fn ulp_to_ulong(d: f64) -> u64 {
    let x = u64::from_ne_bytes(d.to_ne_bytes());
    if (x & 0x8000000000000000u64) != 0 {
        x ^ 0xffffffffffffffffu64
    } else {
        x | 0x8000000000000000u64
    }
}
