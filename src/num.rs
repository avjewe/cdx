//! Numeric Helpers
#![allow(dead_code)]
#![allow(clippy::unneeded_field_pattern)]

use crate::*;

trait Integer {
    const MAX: Self;
    const MIN: Self;
}
use crate::prelude::*;
// #[allow(dead_code)]
// struct Container<T : Integer, const MIN : T = {T::MIN}, const MAX : T = {T::MAX}> {
//     payload: T,
// }

struct WeatherReading {
    station_id: String,
    recorded_at: u32,
    temperature: f64,
    humidity: f64,
    pressure: f64,
}
fn is_dangerous(
    WeatherReading {
        station_id: _,
        recorded_at: _,
        temperature,
        humidity,
        pressure,
    }: &WeatherReading,
) -> bool {
    if *temperature > 40.0 {
        // assuming °C, that's 104°F or 313.15K
        return true;
    }
    if *humidity < 5.0 {
        // assuming percentage
        return true;
    }
    if *pressure < 960.0 {
        // assuming hPa, that's 28.35 inHg
        return true;
    }
    false
}

impl WeatherReading {
    fn is_dangerous(&self) -> bool {
        let Self { station_id: _, recorded_at: _, temperature, humidity, pressure } = self;
        if *temperature > 40.0 {
            // assuming °C, that's 104°F or 313.15K
            return true;
        }
        if *humidity < 5.0 {
            // assuming percentage
            return true;
        }
        if *pressure < 960.0 {
            // assuming hPa, that's 28.35 inHg
            return true;
        }
        false
    }
}

/// is target closer to num/denom than to (num-1)/denom or (num+1)/denom
#[must_use]
#[expect(clippy::cast_precision_loss)]
pub fn f64_equal(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    (lower..upper).contains(&target)
}

/// is target closer to num/denom than to (num-1)/denom
#[must_use]
#[expect(clippy::cast_precision_loss)]
pub fn f64_greater(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let lower = 2.0f64.mul_add(fnum, -1.0) / (2.0 * fdenom);
    target > lower
}

/// is target closer to num/denom than to (num+1)/denom
#[must_use]
#[expect(clippy::cast_precision_loss)]
pub fn f64_less(target: f64, num: usize, denom: usize) -> bool {
    let fnum = num as f64;
    let fdenom = denom as f64;
    let upper = 2.0f64.mul_add(fnum, 1.0) / (2.0 * fdenom);
    target < upper
}

/// amount of junk allowed in a number, before falling back to the 'error' value
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
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
                if j == JunkVal::Min { Self::MIN } else { Self::MAX }
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

/// ordering for f64. `total_cmp`, but 0.0 == -0.0
#[must_use]
pub fn fcmp(x: f64, y: f64) -> Ordering {
    let x = if x == 0.0 { 0.0 } else { x };
    let y = if y == 0.0 { 0.0 } else { y };
    x.total_cmp(&y)
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
    /// simplest rational approximation
    Rational(usize),
    /// Roman Numerals
    Roman,
    /// Dyadic fractions. Can get rather wordy.
    Dyadic(dyadic::Dyadic),
}
impl Default for NumFormat {
    fn default() -> Self {
        Self::Plain(None)
        // Self::Short(None)
    }
}
const FORMAT_CHOICES: &str = "Number format must be short[.N], plain[.N], float[.N], rational[.N], roman, dyadic, power2 or power10";

impl NumFormat {
    /// new from string
    pub fn new(spec: &str) -> Result<Self> {
        let spec = spec.to_lowercase();
        if spec == "plain" {
            Ok(Self::Plain(None))
        } else if spec == "short" {
            Ok(Self::Short(None))
        } else if spec == "float" {
            Ok(Self::Float(None))
        } else if spec == "roman" {
            Ok(Self::Roman)
        } else if spec == "dyadic" {
            Ok(Self::Dyadic(dyadic::Dyadic::default()))
        } else if let Some(suffix) = spec.strip_prefix("dyadic,") {
            Ok(Self::Dyadic(dyadic::Dyadic::from_spec(suffix)?))
        } else if spec == "power2" || spec == "p2" {
            Ok(Self::Power2)
        } else if spec == "power10" || spec == "p10" {
            Ok(Self::Power10)
        } else if spec == "rational" {
            Ok(Self::Rational(3))
        } else if let Some((a, b)) = spec.split_once('.') {
            let p = b.to_usize_whole(spec.as_bytes(), "Num Format")?;
            if a == "plain" {
                Ok(Self::Plain(Some(p)))
            } else if a == "short" {
                Ok(Self::Short(Some(p)))
            } else if a == "float" {
                Ok(Self::Float(Some(p)))
            } else if a == "rational" {
                if p < 1 {
                    err!("Number of digits of precision for rational must be positive.")
                } else {
                    Ok(Self::Rational(p))
                }
            } else {
                err!(FORMAT_CHOICES)
            }
        } else {
            err!(FORMAT_CHOICES)
        }
    }

    fn new_format(self, num: f64) -> Self {
        match self {
            Self::Short(x) => {
                let num = num.abs();
                if (0.0001..1000.0).contains(&num) { Self::Plain(x) } else { Self::Float(x) }
            }
            _ => self,
        }
    }

    /// format a number
    pub fn print(self, num: f64, w: impl Write) -> Result<()> {
        let nfmt = self.new_format(num);
        nfmt.inner_print(num, w)
    }
    fn inner_print(self, mut num: f64, mut w: impl Write) -> Result<()> {
        match self {
            Self::Short(_n) => {
                anyhow::bail!("bad");
            }

            Self::Plain(n) => {
                if let Some(prec) = n {
                    write!(w, "{num:.prec$}")?;
                } else {
                    write!(w, "{num}")?;
                }
            }
            Self::Float(n) => {
                if let Some(prec) = n {
                    write!(w, "{num:.prec$e}")?;
                } else {
                    write!(w, "{num:e}")?;
                }
            }
            Self::Rational(n) => {
                let (x, y) = format::get_rational(num, n as u32);
                write!(w, "{x}/{y}")?;
            }
            Self::Roman => {
                roman::write_roman_f(num, &mut w)?;
            }
            Self::Dyadic(d) => {
                w.write_all(d.format_frac(num).as_bytes())?;
            }
            Self::Power2 => {
                num = num.round();
                if num.abs() < 1000.0 || num.is_nan() || num.is_infinite() {
                    write!(w, "{num}")?;
                    return Ok(());
                }
                write!(w, "{}", format::format_power2f(num))?;
            }
            Self::Power10 => {
                num = num.round();
                if num.abs() < 1000.0 || num.is_nan() || num.is_infinite() {
                    write!(w, "{num}")?;
                    return Ok(());
                }
                write!(w, "{}", format::format_power10f(num))?;
            }
        }
        Ok(())
    }
}

/// Suffix letters for powers of 2^10
pub const P2_LETTERS: &[u8] = b"0KMGTPEZYRQ";
/// Suffix letters for powers of 10^3
pub const P10_LETTERS: &[u8] = b"0kmgtpezyrq";
const P2_VALUES_U: [usize; 11] = [
    1,
    1024,
    1024 * 1024,
    1024 * 1024 * 1024,
    1024 * 1024 * 1024 * 1024,
    1024 * 1024 * 1024 * 1024 * 1024,
    1024 * 1024 * 1024 * 1024 * 1024 * 1024,
    usize::MAX,
    usize::MAX,
    usize::MAX,
    usize::MAX,
];
const P2_VALUES_F: [f64; 11] = [
    1.0,
    1024.0,
    1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
    1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0,
];
const P10_VALUES_U: [usize; 11] = [
    1,
    1000,
    1000 * 1000,
    1000 * 1000 * 1000,
    1000 * 1000 * 1000 * 1000,
    1000 * 1000 * 1000 * 1000 * 1000,
    1000 * 1000 * 1000 * 1000 * 1000 * 1000,
    usize::MAX,
    usize::MAX,
    usize::MAX,
    usize::MAX,
];
const P10_VALUES_F: [f64; 11] = [
    1.0,
    1000.0,
    1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
    1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0 * 1000.0,
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
pub fn suffix_value(ch: u8) -> Option<usize> {
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
