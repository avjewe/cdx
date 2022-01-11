//! Transpose a file

use crate::util::{get_writer, Reader, Result};
use std::io::Write;

/// transpose
pub fn transpose(file: &str, head: bool, max_lines: usize) -> Result<()> {
    let mut f = Reader::new();
    f.open(file)?;
    if f.is_empty() {
        return Ok(());
    }
    let mut data = Vec::new();
    let mut lines = 0;
    while lines < max_lines {
        data.push(f.curr().clone());
        lines += 1;
        if f.getline()? {
            break;
        }
    }
    let mut w = get_writer("-")?;
    if head {
        w.write_all(b" CDX\t")?;
    }
    for i in 0..f.header().len() {
        let mut need_tab = if f.has_header() {
            w.write_all(f.header()[i].as_bytes())?;
            true
        } else {
            false
        };
        for x in &data {
            if need_tab {
                w.write_all(b"\t")?;
            }
            need_tab = true;
            w.write_all(&x[i])?;
        }
        w.write_all(b"\n")?;
    }
    Ok(())
}
