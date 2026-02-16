//! format numbers into human readable abbreviations

#[must_use]
/// Format f64 into something short and readable, like 42K or 1.5M
/// Value is rounded to the nearest integer.
///
/// Returned string is at most 5 characters, including optional `-` sign
/// 0-999 are themselves
/// The other formats are 1K, 1.2K, 12K, 123K
/// Where K == 1024, and M, G, T, P, E, Z, Y, R, Q each increase by a factor of 1024
/// The number represented by the string is never less than the original number
/// e.g 1000 = "1K", 1024="1K", 1025="1.1K"
pub fn format_power2f(num: f64) -> String {
    if num < 0.0 {
        let mut ret = "-".to_string();
        ret.push_str(&format_inner(-num, false));
        ret
    } else {
        format_inner(num, false)
    }
}

#[must_use]
/// Format f64 into something short and readable, like 42k or 1.5m
/// Value is rounded to the nearest integer.
///
/// Returned string is at most 5 characters, including optional `-` sign
/// 0-999 are themselves
/// The other formats are 1k, 1.2k, 12k, 123k
/// Where K == 1000, and m, g, t, p, e, z, y, r, q each increase by a factor of 1000
/// The number represented by the string is never less than the original number
/// e.g 1000 = "1k", 1001="1.1k"
pub fn format_power10f(num: f64) -> String {
    if num < 0.0 {
        let mut ret = "-".to_string();
        ret.push_str(&format_inner(-num, true));
        ret
    } else {
        format_inner(num, true)
    }
}

#[must_use]
/// as `format_power2f`
pub fn format_power2(num: u64) -> String {
    #[allow(clippy::cast_precision_loss, reason = "precision loss ok")]
    format_power2f(num as f64)
}

#[must_use]
/// as `format_power10f`
pub fn format_power10(num: u64) -> String {
    #[allow(clippy::cast_precision_loss, reason = "precision loss ok")]
    format_power10f(num as f64)
}

#[must_use]
fn format_inner(num: f64, is_10: bool) -> String {
    if num < 1000.0 {
        return format!("{num:.0}");
    }
    let mut curr_exp = 1f64;
    let mut exp =
        if is_10 { crate::num::P10_LETTERS.iter() } else { crate::num::P2_LETTERS.iter() };
    let bump = if is_10 { 1000.0 } else { 1024.0 };

    loop {
        let e = exp.next().unwrap();
        if num <= (999.0 * curr_exp) {
            if num > (9.9 * curr_exp) {
                return format!("{:.0}{}", (num / curr_exp).ceil(), *e as char);
            }
            if num > curr_exp {
                return format!("{:.1}{}", (num * 10.0 / curr_exp).ceil() / 10.0, *e as char);
            }
            return format!("1{}", *e as char);
        }
        if *e == b'Q' {
            return format!("{:.0}Q", num / curr_exp);
        }
        curr_exp *= bump;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_format2() {
        assert!(format_power2(0) == "0");
        assert!(format_power2(1) == "1");
        assert!(format_power2f(-1f64) == "-1");
        assert!(format_power2(999) == "999");
        assert!(format_power2(1000) == "1K");
        assert!(format_power2(1024) == "1K");
        assert!(format_power2(1025) == "1.1K");
        assert!(format_power2(10137) == "9.9K");
        assert!(format_power2(10138) == "10K");
        assert!(format_power2(97 * 1024) == "97K");
        assert!(format_power2(97 * 1024 + 1) == "98K");
        assert!(format_power2(999 * 1024) == "999K");
        assert!(format_power2(999 * 1024 + 1) == "1M");
    }
    #[test]
    fn test_format10() {
        assert!(format_power10(0) == "0");
        assert!(format_power10(1) == "1");
        assert!(format_power10f(-1f64) == "-1");
        assert!(format_power10(999) == "999");
        assert!(format_power10(1000) == "1k");
        assert!(format_power10(1001) == "1.1k");
        assert!(format_power10(9900) == "9.9k");
        assert!(format_power10(9901) == "10k");
        assert!(format_power10(10000) == "10k");
        assert!(format_power10(97000) == "97k");
        assert!(format_power10(97001) == "98k");
        assert!(format_power10(999_000) == "999k");
        assert!(format_power10(999_001) == "1m");
    }
}
