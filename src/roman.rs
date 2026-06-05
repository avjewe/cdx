//! Roman Numerals
use anyhow::{Result, bail};
use std::io::Cursor;

const ROMAN_NUMERALS: &[(u64, &str)] = &[
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

/// Write number in Roman Numerals. Fail unless 0 < value < 4000
pub fn write_roman<W: std::io::Write + ?Sized>(mut value: u64, writer: &mut W) -> Result<()> {
    if value == 0 {
        writer.write_all(b"N")?;
        return Ok(());
    }
    if value > 10000 {
        anyhow::bail!("Roman numerals must not be more than 10000, was {value}");
    }
    for &(arabic, roman) in ROMAN_NUMERALS {
        while value >= arabic {
            writer.write_all(roman.as_bytes())?;
            value -= arabic;
        }
    }

    Ok(())
}

/// Write number in Roman Numerals.
pub fn write_roman_f<W: std::io::Write + ?Sized>(value: f64, writer: &mut W) -> Result<()> {
    if !(0.0..=3999.0).contains(&value) || ((value - value.round()).abs() > 0.01) {
        write!(writer, "[Can't write {value} in roman numerals]")?;
        return Ok(());
    }
    write_roman(value.round() as u64, writer)
}

// The longest classical canonical Roman numeral is MMMDCCCLXXXVIII (3888).
const MAX_STRICT_ROMAN_LEN: usize = 15;

fn roman_digit(byte: u8) -> Result<u64> {
    match byte.to_ascii_uppercase() {
        b'I' => Ok(1),
        b'V' => Ok(5),
        b'X' => Ok(10),
        b'L' => Ok(50),
        b'C' => Ok(100),
        b'D' => Ok(500),
        b'M' => Ok(1000),
        byte if byte.is_ascii_graphic() => {
            bail!("invalid roman numeral byte: '{}' ({:#04X})", byte as char, byte)
        }
        _ => bail!("invalid roman numeral byte: {byte:#04X}"),
    }
}

fn add_to_total(total: &mut u64, value: u64) -> Result<()> {
    *total = total
        .checked_add(value)
        .ok_or_else(|| anyhow::anyhow!("roman numeral value overflows u64"))?;
    Ok(())
}

/// Converts an ASCII Roman numeral byte string into its numeric value.
///
/// Input is case-insensitive. `N` is accepted as zero when it is the whole
/// input.
///
/// This is intentionally permissive about non-canonical forms: repeated digits
/// such as `IIIIIII` are accepted. It still rejects unknown bytes and
/// interprets any single symbol before a larger symbol as subtractive, so `IM`
/// is 999 and `VL` is 45.
pub fn read_roman(data: &[u8]) -> Result<u64> {
    // `N` is a special zero spelling, but only when it is the whole input.
    if data.len() == 1 && data[0].eq_ignore_ascii_case(&b'N') {
        return Ok(0);
    }

    if data.is_empty() {
        bail!("roman numeral is empty");
    }

    let mut total: u64 = 0;
    let mut index = 0;

    while index < data.len() {
        let value = roman_digit(data[index])?;

        if index + 1 < data.len() {
            let next = roman_digit(data[index + 1])?;

            if value < next {
                // Subtraction only looks one symbol ahead; `IIM` is `I + IM`.
                add_to_total(&mut total, next - value)?;
                index += 2;
                continue;
            }
        }

        add_to_total(&mut total, value)?;
        index += 1;
    }

    Ok(total)
}

/// Converts a canonical ASCII Roman numeral byte string into its numeric value.
///
/// This uses `write_roman` as the definition of canonical formatting. Input is
/// still case-insensitive, so `mcmxciv` is accepted as the canonical spelling
/// of 1994.
///
/// Strict input is limited to 15 bytes, the length of the longest classical
/// canonical Roman numeral. This keeps the canonical comparison on a fixed-size
/// stack buffer.
pub fn read_roman_strict(data: &[u8]) -> Result<u64> {
    if data.len() > MAX_STRICT_ROMAN_LEN {
        bail!("roman numeral is too long to be canonical");
    }

    let value = read_roman(data)?;
    if value > 3999 {
        bail!("strict roman numeral value must be in the range 0 to 3999");
    }

    let mut canonical = [0; MAX_STRICT_ROMAN_LEN];
    let mut writer = Cursor::new(canonical.as_mut_slice());
    write_roman(value, &mut writer)?;
    let canonical_len = writer.position() as usize;

    if !data.iter().map(u8::to_ascii_uppercase).eq(canonical[..canonical_len].iter().copied()) {
        bail!("roman numeral is not canonical");
    }

    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::{read_roman, read_roman_strict, write_roman};

    fn roman(value: u64) -> String {
        let mut buffer = Vec::new();
        write_roman(value, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    #[test]
    fn writes_basic_roman_numerals() {
        assert_eq!(roman(1), "I");
        assert_eq!(roman(3), "III");
        assert_eq!(roman(5), "V");
        assert_eq!(roman(10), "X");
    }

    #[test]
    fn writes_subtractive_roman_numerals() {
        assert_eq!(roman(4), "IV");
        assert_eq!(roman(9), "IX");
        assert_eq!(roman(40), "XL");
        assert_eq!(roman(90), "XC");
        assert_eq!(roman(400), "CD");
        assert_eq!(roman(900), "CM");
    }

    #[test]
    fn writes_compound_values() {
        assert_eq!(roman(58), "LVIII");
        assert_eq!(roman(1994), "MCMXCIV");
        assert_eq!(roman(2026), "MMXXVI");
    }

    #[test]
    fn writes_z_for_zero() {
        assert_eq!(roman(0), "N");
    }

    fn write(value: u64) -> Vec<u8> {
        let mut buffer = Vec::new();
        write_roman(value, &mut buffer).unwrap();
        buffer
    }

    fn assert_writer_round_trip(value: u64) {
        let buffer = write(value);
        assert_eq!(read_roman(&buffer).unwrap(), value);
    }

    fn assert_strict_writer_round_trip(value: u64) {
        let buffer = write(value);
        assert_eq!(read_roman(&buffer).unwrap(), value);
        assert_eq!(read_roman_strict(&buffer).unwrap(), value);
    }

    #[test]
    fn reads_basic_roman_numerals() {
        assert_eq!(read_roman(b"I").unwrap(), 1);
        assert_eq!(read_roman(b"III").unwrap(), 3);
        assert_eq!(read_roman(b"V").unwrap(), 5);
        assert_eq!(read_roman(b"X").unwrap(), 10);
    }

    #[test]
    fn reads_subtractive_roman_numerals() {
        assert_eq!(read_roman(b"IV").unwrap(), 4);
        assert_eq!(read_roman(b"IX").unwrap(), 9);
        assert_eq!(read_roman(b"XL").unwrap(), 40);
        assert_eq!(read_roman(b"XC").unwrap(), 90);
        assert_eq!(read_roman(b"CD").unwrap(), 400);
        assert_eq!(read_roman(b"CM").unwrap(), 900);
    }

    #[test]
    fn reads_compound_values() {
        assert_eq!(read_roman(b"LVIII").unwrap(), 58);
        assert_eq!(read_roman(b"MCMXCIV").unwrap(), 1994);
        assert_eq!(read_roman(b"MMXXVI").unwrap(), 2026);
    }

    #[test]
    fn is_case_insensitive() {
        assert_eq!(read_roman(b"mCmXcIv").unwrap(), 1994);
    }

    #[test]
    fn recognizes_n_as_zero() {
        assert_eq!(read_roman(b"N").unwrap(), 0);
        assert_eq!(read_roman(b"n").unwrap(), 0);
    }

    #[test]
    fn rejects_invalid_numerals() {
        assert!(read_roman(b"").is_err());
        assert!(read_roman(b"MN").is_err());
        assert!(read_roman(b"A").is_err());
    }

    #[test]
    fn invalid_byte_errors_include_printable_character() {
        let error = read_roman(b"A").unwrap_err().to_string();
        assert!(error.contains("'A'"));
        assert!(error.contains("0x41"));
    }

    #[test]
    fn allows_more_than_three_leading_ms() {
        assert_eq!(read_roman(b"MMMM").unwrap(), 4000);
    }

    #[test]
    fn allows_non_canonical_repetition() {
        assert_eq!(read_roman(b"IIIIIII").unwrap(), 7);
        assert_eq!(read_roman(b"VV").unwrap(), 10);
    }

    #[test]
    fn allows_generalized_single_symbol_subtraction() {
        assert_eq!(read_roman(b"IM").unwrap(), 999);
        assert_eq!(read_roman(b"VL").unwrap(), 45);
        assert_eq!(read_roman(b"IC").unwrap(), 99);
        assert_eq!(read_roman(b"VX").unwrap(), 5);
    }

    #[test]
    fn documents_left_to_right_pairing() {
        assert_eq!(read_roman(b"IIM").unwrap(), 1000);
        assert_eq!(read_roman(b"VXL").unwrap(), 55);
        assert_eq!(read_roman(b"IXL").unwrap(), 59);
        assert_eq!(read_roman(b"XXM").unwrap(), 1000);
        assert_eq!(read_roman(b"MIM").unwrap(), 1999);
    }

    #[test]
    fn reads_values_written_by_write_roman() {
        assert_writer_round_trip(0);
        assert_writer_round_trip(1);
        assert_writer_round_trip(4);
        assert_writer_round_trip(9);
        assert_writer_round_trip(58);
        assert_writer_round_trip(1994);
        assert_writer_round_trip(2026);
        assert_writer_round_trip(4000);
    }

    #[test]
    fn strict_reader_round_trips_classical_range() {
        for value in 0..=3999 {
            assert_strict_writer_round_trip(value);
        }
    }

    #[test]
    fn strict_reader_accepts_canonical_input_case_insensitively() {
        assert_eq!(read_roman_strict(b"N").unwrap(), 0);
        assert_eq!(read_roman_strict(b"n").unwrap(), 0);
        assert_eq!(read_roman_strict(b"mcmxciv").unwrap(), 1994);
        assert_eq!(read_roman_strict(b"MMMDCCCLXXXVIII").unwrap(), 3888);
    }

    #[test]
    fn strict_reader_rejects_non_canonical_input() {
        assert!(read_roman_strict(b"").is_err());
        assert!(read_roman_strict(b"IIII").is_err());
        assert!(read_roman_strict(b"VV").is_err());
        assert!(read_roman_strict(b"IM").is_err());
        assert!(read_roman_strict(b"VL").is_err());
        assert!(read_roman_strict(b"IIM").is_err());
        assert!(read_roman_strict(b"MN").is_err());
        assert!(read_roman_strict(b"MMMM").is_err());
        assert!(read_roman_strict(b"MMMMMMMMMMMMMMMM").is_err());
    }

    #[test]
    fn strict_reader_range_error_names_range() {
        let error = read_roman_strict(b"MMMM").unwrap_err().to_string();
        assert!(error.contains("0 to 3999"));
    }
}
