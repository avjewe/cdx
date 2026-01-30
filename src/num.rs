//! Numeric Helpers

use crate::prelude::*;

/// is target closer to num/denom than to (num-1)/denom or (num+1)/denom
#[must_use]
pub fn f64_equal(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    (lower..upper).contains(&target)
}

/// is target closer to num/denom than to (num-1)/denom
#[must_use]
pub fn f64_greater(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    target > lower
}

/// is target closer to num/denom than to (num+1)/denom
#[must_use]
pub fn f64_less(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    target < upper
}

/// amount of junk allowed in a number, before falling back to the 'error' value
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Default)]
pub enum JunkType {
    /// best effort for any input
    #[default]
    Any,
    /// trailing junk is ok, but must have a valid prefix
    Trailing,
    /// every byte must be part of a valid value
    None,
}

/// Value to assign to junk values
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Default)]
pub enum JunkVal {
    /// if value is junk, make it compare less
    Min,
    /// if value is junk, make it compare greater
    #[default]
    Max,
}

/// return appropriate value for type and `JunkVal`
pub trait JunkValue {
    /// return appropriate value for type and `JunkVal`
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
    /// return value associated with `JunkVal`
    #[must_use]
    pub fn val<T: JunkValue>(&self) -> T {
        T::val(self.junk_val)
    }
}

/// ordering for f64
#[must_use]
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
#[must_use]
pub const fn ulp_to_ulong(d: f64) -> u64 {
    let x = u64::from_ne_bytes(d.to_ne_bytes());
    if (x & 0x8000_0000_0000_0000_u64) != 0 {
        x ^ 0xffff_ffff_ffff_ffff_u64
    } else {
        x | 0x8000_0000_0000_0000_u64
    }
}

/// how to format a number
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumFormat {
    /// Plain or Float, whichever is shorter
    Short(Option<usize>),
    /// as integer
    Plain(Option<usize>),
    /// 1.2e34
    Float(Option<usize>),
    /// power of 2, e.g. 3K
    Power2,
    /// power of 10 e.g. 3k
    Power10,
}
impl Default for NumFormat {
    fn default() -> Self {
        Self::Short(None)
    }
}
impl NumFormat {
    /// new from string
    pub fn new(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("plain") {
            Ok(Self::Plain(None))
        } else if spec.eq_ignore_ascii_case("short") {
            Ok(Self::Short(None))
        } else if spec.eq_ignore_ascii_case("float") {
            Ok(Self::Float(None))
        } else if spec.eq_ignore_ascii_case("power2") || spec.eq_ignore_ascii_case("p2") {
            Ok(Self::Power2)
        } else if spec.eq_ignore_ascii_case("power10") || spec.eq_ignore_ascii_case("p10") {
            Ok(Self::Power10)
        } else if let Some((a, b)) = spec.split_once('.') {
            let p = b.to_usize_whole(spec.as_bytes(), "Num Format")?;
            if a.eq_ignore_ascii_case("plain") {
                Ok(Self::Plain(Some(p)))
            } else if a.eq_ignore_ascii_case("short") {
                Ok(Self::Short(Some(p)))
            } else if a.eq_ignore_ascii_case("float") {
                Ok(Self::Float(Some(p)))
            } else {
                err!("Number format must be short[.N], plain[.N], float[.N], power2 or power10")
            }
        } else {
            err!("Number format must be short[.N], plain[.N], float[.N], power2 or power10")
        }
    }

    fn new_format(self, num: f64) -> Self {
        match self {
            Self::Short(x) => {
                let num = num.abs();
                if (0.0001..1000.0).contains(&num) {
                    Self::Plain(x)
                } else {
                    Self::Float(x)
                }
            }
            _ => self,
        }
    }

    /// format a number
    pub fn print(self, mut num: f64, mut w: impl Write) -> Result<()> {
        let nfmt = self.new_format(num);
        if let Self::Plain(n) = nfmt {
            if let Some(prec) = n {
                write!(w, "{num:.prec$}")?;
            } else {
                write!(w, "{num}")?;
            }
            return Ok(());
        }
        if let Self::Float(n) = nfmt {
            if let Some(prec) = n {
                write!(w, "{num:.prec$e}")?;
            } else {
                write!(w, "{num:e}")?;
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
        let (exp, letters) = if nfmt == Self::Power2 {
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
const P10_VALUES_U: [usize; 9] = [
    1,
    1000,
    1000 ^ 2,
    1000 ^ 3,
    1000 ^ 4,
    1000 ^ 5,
    1000 ^ 6,
    usize::MAX,
    usize::MAX,
];
const P10_VALUES_F: [f64; 9] = [
    1.0,
    1000.0,
    1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
];

/// return value associated with suffix character, e.g. K returns 1024 and k returns 1000
#[must_use]
pub fn suffix_valf(ch: u8) -> Option<f64> {
    for (i, x) in P2_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P2_VALUES_F[i]);
        }
    }
    for (i, x) in P10_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P10_VALUES_F[i]);
        }
    }
    None
}
/// return value associated with suffix character, e.g. K returns 1024 and k returns 1000
#[must_use]
pub fn suffix_valu(ch: u8) -> Option<usize> {
    for (i, x) in P2_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P2_VALUES_U[i]);
        }
    }
    for (i, x) in P10_LETTERS.iter().enumerate() {
        if *x == ch {
            return Some(P10_VALUES_U[i]);
        }
    }
    None
}
