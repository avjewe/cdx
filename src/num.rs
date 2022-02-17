//! Numeric Helpers

use crate::prelude::*;

/// is target closer to num/denom than to (num-1)/denom or (num+1)/denom
pub fn f64_equal(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    (lower..upper).contains(&target)
}

/// is target closer to num/denom than to (num-1)/denom
pub fn f64_greater(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    target > lower
}

/// is target closer to num/denom than to (num+1)/denom
pub fn f64_less(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    target < upper
}

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
    Plain(usize),
    /// 1.2e34
    Float(usize),
    /// power of 2, e.g. 3K
    Power2,
    /// power of 10 e.g. 3k
    Power10,
}
impl Default for NumFormat {
    fn default() -> Self {
        Self::Plain(0)
    }
}
impl NumFormat {
    /// new from string
    pub fn new(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("plain") {
            Ok(Self::Plain(0))
        } else if spec.eq_ignore_ascii_case("float") {
            Ok(Self::Float(0))
        } else if spec.eq_ignore_ascii_case("power2") || spec.eq_ignore_ascii_case("p2") {
            Ok(Self::Power2)
        } else if spec.eq_ignore_ascii_case("power10") || spec.eq_ignore_ascii_case("p10") {
            Ok(Self::Power10)
        } else if let Some((a, b)) = spec.split_once('.') {
            let p = b.to_usize_whole(spec.as_bytes(), "Num Format")?;
            if a.eq_ignore_ascii_case("plain") {
                Ok(Self::Plain(p))
            } else if a.eq_ignore_ascii_case("float") {
                Ok(Self::Float(p))
            } else {
                err!("Number format must be plain[.N], float[.N], power2 or power10")
            }
        } else {
            err!("Number format must be plain[.N], float[.N], power2 or power10")
        }
    }

    /// format a number
    pub fn print(self, mut num: f64, mut w: impl Write) -> Result<()> {
        if let NumFormat::Plain(n) = self {
            if n == 0 {
                write!(w, "{num}")?;
            } else {
                write!(w, "{num:.0$}", n)?;
            }
            return Ok(());
        }
        if let NumFormat::Float(n) = self {
            if n == 0 {
                write!(w, "{num:e}")?;
            } else {
                write!(w, "{num:.0$e}", n)?;
            }
            return Ok(());
        }
        num = num.round();
        if num.abs() < 1000.0 || num.is_nan() || num.is_infinite() {
            write!(w, "{num}")?;
            return Ok(());
        }
        let sign = if num < 0.0 {
            num = -num;
            "-"
        } else {
            ""
        };
        let (exp, letters) = if self == Self::Power2 {
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
}

const P2_LETTERS: &[u8] = b"0KMGTPEZY";
const P10_LETTERS: &[u8] = b"0kmgtpezy";
const P2_VALUES_U: [usize; 9] = [
    1,
    1024,
    1024 ^ 2,
    1024 ^ 3,
    1024 ^ 4,
    1024 ^ 5,
    1024 ^ 6,
    usize::MAX,
    usize::MAX,
];
const P2_VALUES_F: [f64; 9] = [
    1.0,
    1024.0,
    1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
];

/// return value associated with suffix character, e.g. K returns 1024 and k returns 1000
pub fn suffix_valf(ch: u8) -> Option<f64> {
    for (i, x) in P2_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P2_VALUES_F[i]);
        }
    }
    None
}
/// return value associated with suffix character, e.g. K returns 1024 and k returns 1000
pub fn suffix_valu(ch: u8) -> Option<usize> {
    for (i, x) in P2_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P2_VALUES_U[i]);
        }
    }
    None
}
