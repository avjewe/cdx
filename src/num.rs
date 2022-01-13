//! Numeric Helpers

use crate::util::{err, Error, Result};
use std::cmp::Ordering;
use std::io::Write;

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
jv!(isize);
jv!(usize);

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

/// how to format a number
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumFormat {
    /// as integer
    Plain,
    /// 1.2e34
    Float,
    /// power of 2, e.g. 3K
    Power2,
    /// power of 10 e.g. 3k
    Power10,
}
impl Default for NumFormat {
    fn default() -> Self {
        Self::Plain
    }
}
impl NumFormat {
    /// new from string
    pub fn new(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("plain")
            || spec.eq_ignore_ascii_case("int")
            || spec.eq_ignore_ascii_case("integer")
        {
            Ok(Self::Plain)
        } else if spec.eq_ignore_ascii_case("float") || spec.eq_ignore_ascii_case("exp") {
            Ok(Self::Float)
        } else if spec.eq_ignore_ascii_case("power2") || spec.eq_ignore_ascii_case("p2") {
            Ok(Self::Power2)
        } else if spec.eq_ignore_ascii_case("power10") || spec.eq_ignore_ascii_case("p10") {
            Ok(Self::Power10)
        } else {
            err!("Number format must be plain, float, power1 or power10")
        }
    }
}

/// format a number
pub fn format_hnum(mut num: f64, fmt: NumFormat, mut w: impl Write) -> Result<()> {
    if fmt == NumFormat::Plain || num.abs() < 1000.0 {
        write!(w, "{}", num)?;
        return Ok(());
    }
    if fmt == NumFormat::Float {
        write!(w, "{:.2e}", num)?;
        return Ok(());
    }
    const P2_LETTERS: &[u8] = b"0KMGTPEZY";
    const P10_LETTERS: &[u8] = b"0kmgtpezy";
    let sign = if num < 0.0 {
        num = -num;
        "-"
    } else {
        ""
    };
    let (exp, letters) = if fmt == NumFormat::Power2 {
        (1024.0, P2_LETTERS)
    } else {
        (1000.0, P10_LETTERS)
    };

    let mut curr_exp: f64 = 1.0;
    let mut exp_num: usize = 0;

    loop {
        if num < (999.0 * curr_exp) {
            if num >= (10.0 * curr_exp) {
                write!(
                    w,
                    "{}{:.0}{}",
                    sign,
                    num / curr_exp,
                    letters[exp_num] as char
                )?;
            } else if num >= (1.1 * curr_exp) {
                write!(
                    w,
                    "{}{:.1}{}",
                    sign,
                    num / curr_exp,
                    letters[exp_num] as char
                )?;
            } else {
                write!(w, "{}1{}", sign, letters[exp_num] as char)?;
            }
            return Ok(());
        }
        exp_num += 1;
        curr_exp *= exp;
        if exp_num >= 8 {
            write!(w, "{}{:.1}{}", sign, num / curr_exp, letters[8] as char)?;
            return Ok(());
        }
    }
}
