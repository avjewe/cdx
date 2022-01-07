//! Numeric Helpers

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
