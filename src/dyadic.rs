//! convert a floating-point number to a fraction
use crate::prelude::*;
// use crate::*;
use std::fmt::Write;

/// Fraction where the denominator is a power of two
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum DyadicKind {
    /// Print lots of text
    Debug,
    /// Print best fraction not exceeding given denominator
    #[default]
    Standard,
    /// Print best fraction. If would be 32nds, write "1/2 + 1/32" or the like
    Pulled,
}

/// Fraction where the denominator is a power of two
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Dyadic {
    /// Print lots of text
    kind: DyadicKind,
    denom: usize,
}

impl Dyadic {
    /// Create new Dyadic
    pub fn new(kind: DyadicKind, denom: usize) -> Result<Self> {
        if !denom.is_power_of_two() {
            anyhow::bail!("denominator for dyadic must be a power of two 2 <= denom <= 64.");
        }
        if denom < 2 {
            anyhow::bail!("denominator for dyadic must be at least 2.");
        }
        if kind == DyadicKind::Pulled && denom < 8 {
            anyhow::bail!("denominator for dyadic::pulled must be at least 8.");
        }
        Ok(Self { kind, denom })
    }
    /// Create Dyadic from text specification
    pub fn from_spec(x: &str) -> Result<Self> {
        let mut kind = DyadicKind::Standard;
        let mut denom = 32usize;
        for p in x.split(',') {
            if p.is_empty() {
            } else if p.eq_ignore_ascii_case("debug") {
                kind = DyadicKind::Debug;
            } else if p.eq_ignore_ascii_case("pulled") {
                kind = DyadicKind::Pulled;
            } else if p.eq_ignore_ascii_case("standard") {
                kind = DyadicKind::Standard;
            } else {
                let n = x.parse::<usize>();
                match n {
                    Ok(d) => {
                        denom = d;
                    }
                    Err(_e) => {
                        return Err(anyhow::anyhow!(
                            "Dyadic parameter must be number or one of : debug, standard, pulled."
                        ));
                    }
                }
            }
        }
        Self::new(kind, denom)
    }
}

/// Remove noise from floating point value, for printing purposes.
#[must_use]
pub fn remove_noise(x: f64) -> f64 {
    // Snap the float by formatting to 14 significant digits and parsing back
    format!("{x:.14}").trim_end_matches('0').parse().unwrap()
}

#[must_use]
fn format_smart(val: f64) -> String {
    let formatted = format!("{val:.4}");
    formatted.trim_end_matches('0').trim_end_matches('.').to_string()
}

impl Dyadic {
    /// Find best carpenter approximation
    #[must_use]
    pub fn format_frac(self, f: f64) -> String {
        let s = format_smart(f);
        let prefix =
            if self.kind == DyadicKind::Debug { format!("{s} == ") } else { String::new() };
        if f < 0.0 {
            format!("{prefix}-{}", self.format_frac2(f.abs()))
        } else if f >= 0.0 {
            format!("{prefix}{}", self.format_frac2(f))
        } else {
            s
        }
    }

    #[must_use]
    fn format_frac2(self, f: f64) -> String {
        debug_assert!(f >= 0.0);
        let whole = f.trunc();
        let fractional = remove_noise(f.fract());
        if fractional == 0.0 {
            format!("{whole}")
        } else if whole == 0.0 {
            self.format_frac3(fractional)
        } else {
            format!("{whole}{}", self.format_frac3(fractional))
        }
    }

    // const fn denom(x: usize) -> usize {
    //     assert!(x < 32, "x must be between 0 and 31 inclusive");
    //     if x == 0 {
    //         return 1;
    //     }
    //     32 >> x.trailing_zeros()
    // }
    const fn denom(x: usize, y: usize) -> usize {
        assert!(x < y, "x must be between 0 and MAX inclusive");
        if x == 0 {
            return 1;
        }
        y >> x.trailing_zeros()
    }

    fn frac(numer: usize, denom: usize) -> String {
        if numer == 0 { String::new() } else { format!(" {numer}/{denom}") }
    }

    #[must_use]
    #[expect(clippy::cast_precision_loss)]
    #[expect(clippy::needless_range_loop)]
    fn format_frac3(self, f: f64) -> String {
        let f_denom = self.denom as f64;
        let mut values = vec![(0, 0.0); self.denom];

        for i in 0..self.denom {
            let x = i as f64;
            let value = x / f_denom;
            values[i] = (i, remove_noise(value - f));
        }
        values.sort_by(|a, b| a.1.abs().total_cmp(&b.1.abs()));
        let mut ret = String::new();
        let mut best_denom = self.denom + 1;
        for i in 0..self.denom {
            if best_denom < 8 {
                break;
            }
            let denom = Self::denom(values[i].0, self.denom);
            // eprintln!("{i} {denom} {best_denom}");
            // if denom >= best_denom && denom < 8 {
            //     break;
            // }
            if denom >= best_denom {
                continue;
            }
            best_denom = denom;
            let numer = values[i].0 * denom / self.denom;
            if !ret.is_empty() {
                ret.push_str(" or");
            }
            let f = Self::frac(numer, denom);
            if self.kind == DyadicKind::Debug {
                let _ = write!(ret, "{f}({})", format_smart(values[i].1));
            } else if self.kind == DyadicKind::Pulled && denom == self.denom {
                let pos = values[i].0;
                let prev_denom = Self::denom(pos - 1, self.denom);
                let next_denom = Self::denom(pos + 1, self.denom);
                if prev_denom < next_denom {
                    let numer = (pos - 1) * prev_denom / self.denom;
                    let _ = write!(ret, " {numer}/{prev_denom} + 1/{}", self.denom);
                } else {
                    let numer = (pos + 1) * next_denom / self.denom;
                    let _ = write!(ret, " {numer}/{next_denom} - 1/{}", self.denom);
                }
                break;
            } else {
                let _ = write!(ret, "{f}");
                break;
            }
        }
        ret
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_frac() {
        assert_eq!(Dyadic::new(DyadicKind::Standard, 32).unwrap().format_frac(-17.00001), "-17");
        assert_eq!(Dyadic::new(DyadicKind::Standard, 32).unwrap().format_frac(1.1), "1 3/32");
        assert_eq!(Dyadic::new(DyadicKind::Standard, 64).unwrap().format_frac(1.1), "1 3/32");
        assert_eq!(Dyadic::new(DyadicKind::Standard, 64).unwrap().format_frac(1.11), "1 7/64");
        assert_eq!(Dyadic::new(DyadicKind::Standard, 16).unwrap().format_frac(1.1), "1 1/8");
        assert_eq!(Dyadic::new(DyadicKind::Pulled, 32).unwrap().format_frac(1.16), "1 1/8 + 1/32");
        assert_eq!(Dyadic::new(DyadicKind::Pulled, 32).unwrap().format_frac(1.1), "1 1/8 - 1/32");
        assert_eq!(Dyadic::new(DyadicKind::Pulled, 64).unwrap().format_frac(1.11), "1 1/8 - 1/64");
        assert_eq!(
            Dyadic::new(DyadicKind::Pulled, 1024).unwrap().format_frac(1.11),
            "1 7/64 + 1/1024"
        );
    }

    // #[test]
    // #[allow(clippy::zero_divided_by_zero)]
    // fn print_format_frac() {
    //     let d = Dyadic::new(DyadicKind::Debug, 1024).unwrap();
    //     println!("{}", d.format_frac(1.00001));
    //     println!("{}", d.format_frac(1.1));
    //     println!("{}", d.format_frac(1.11));
    //     println!("{}", d.format_frac(1.12));
    //     println!("{}", d.format_frac(1.13));
    //     println!("{}", d.format_frac(1.14));
    //     println!("{}", d.format_frac(1.16));
    //     println!("{}", d.format_frac(1.17));
    //     println!("{}", d.format_frac(1.18));
    //     println!("{}", d.format_frac(1.19));
    //     println!("{}", d.format_frac(1.2));
    //     println!("{}", d.format_frac(1.271));
    //     println!("{}", d.format_frac(1.3));
    //     println!("{}", d.format_frac(0.0 / 0.0));
    //     println!("{}", d.format_frac(1.5));
    //     println!("{}", d.format_frac(1.6));
    //     println!("{}", d.format_frac(1.7));
    //     println!("{}", d.format_frac(1.8));
    //     println!("{}", d.format_frac(1.9));
    // }
}
