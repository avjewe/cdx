//! convert a floating-point number to a fraction

use std::fmt::Write;

/// Remove noise from floating point value, for printing purposes.
#[must_use]
pub fn remove_noise(x: f64) -> f64 {
    // Snap the float by formatting to 14 significant digits and parsing back
    format!("{x:.14}").trim_end_matches('0').parse().unwrap()
}

/// Find best carpenter approximation
#[must_use]
pub fn format_frac(f: f64) -> String {
    let s = format_smart(f);
    if f < 0.0 {
        format!("{s} == -{}", format_frac2(f.abs()))
    } else if f >= 0.0 {
        format!("{s} == {}", format_frac2(f))
    } else {
        s
    }
}

#[must_use]
fn format_frac2(f: f64) -> String {
    debug_assert!(f >= 0.0);
    let whole = f.trunc();
    let fractional = remove_noise(f.fract());
    if fractional == 0.0 {
        format!("{whole}")
    } else if whole == 0.0 {
        format_frac3(fractional)
    } else {
        format!("{whole} and {}", format_frac3(fractional))
    }
}

const DENOM: [usize; 32] = [
    2, 32, 16, 32, 8, 32, 16, 32, 4, 32, 16, 32, 8, 32, 16, 32, 2, 32, 16, 32, 8, 32, 16, 32, 4,
    32, 16, 32, 8, 32, 16, 32,
];

fn format_smart(val: f64) -> String {
    let formatted = format!("{val:.4}");
    formatted.trim_end_matches('0').trim_end_matches('.').to_string()
}

#[must_use]
#[expect(clippy::cast_precision_loss)]
#[expect(clippy::needless_range_loop)]
fn format_frac3(f: f64) -> String {
    debug_assert!(f > 0.0 && f < 1.0);
    let mut values = [(0, 0.0); 32];
    for i in 0..32 {
        let x = i as f64;
        let value = x / 32.0;
        values[i] = (i, remove_noise(value - f));
    }
    values.sort_by(|a, b| a.1.abs().total_cmp(&b.1.abs()));
    let mut ret = String::new();
    let mut best_denom = 99;
    for i in 0..32 {
        let denom = DENOM[values[i].0];
        if denom >= best_denom {
            break;
        }
        best_denom = denom;
        let numer = values[i].0 * denom / 32;
        if !ret.is_empty() {
            ret.push_str(" or ");
        }
        let _ = write!(ret, "{numer}/{denom} ({})", format_smart(values[i].1));
    }
    ret
}

// #[cfg(test)]
// mod tests {
//     use super::*;

// #[test]
// fn test_format_frac() {
//     println!("{}", format_frac(1.1));
//     println!("{}", format_frac(1.11));
//     println!("{}", format_frac(1.12));
//     println!("{}", format_frac(1.13));
//     println!("{}", format_frac(1.14));
//     println!("{}", format_frac(1.16));
//     println!("{}", format_frac(1.17));
//     println!("{}", format_frac(1.18));
//     println!("{}", format_frac(1.19));
//     println!("{}", format_frac(1.2));
//     println!("{}", format_frac(1.271));
//     println!("{}", format_frac(1.3));
//     println!("{}", format_frac(0.0 / 0.0));
//     println!("{}", format_frac(1.5));
//     println!("{}", format_frac(1.6));
//     println!("{}", format_frac(1.7));
//     println!("{}", format_frac(1.8));
//     println!("{}", format_frac(1.9));
// }
// }
