use crate::{args, arg_enum};
use crate::args::ArgSpec;
use cdx::{get_reader, get_writer, Result, HeaderChecker, HeaderMode, HEADER_MODE};
use std::io::{BufRead, Read, Write};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
enum PadMode {
    None,
    All,
    End
}

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Concatenate files.", args::FileCount::Many);
    const A: [ArgSpec; 2] = [
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
        arg_enum! {"pad", "p", "Mode", "Add trailing newline if absent.", &["Yes","No","End"]},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut pad = PadMode::All;

    for x in args {
	if x.name == "header" {
	    checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "pad" {
	    if x.value.to_ascii_lowercase() == "yes" {
		pad = PadMode::All;
	    }
	    else if x.value.to_ascii_lowercase() == "no" {
		pad = PadMode::None;
	    }
	    else if x.value.to_ascii_lowercase() == "end" {
		pad = PadMode::End;
	    }
	    else {
		unreachable!();
	    }
        } else {
            unreachable!();
        }
    }
    
    let mut w = get_writer("-")?;
    const SIZE: usize = 16 * 1024;
    let mut buffer = [0u8; SIZE];
    let mut first_line = Vec::new();
    let mut last_was_cr = true;
    
    for x in files {
        let mut f = get_reader(&x)?;
        first_line.clear();
        let n = f.read_until(b'\n', &mut first_line)?;
        if n == 0 {
            continue;
        }
	if checker.check(&first_line, &x)? {
            w.write_all(&first_line)?;
	    last_was_cr = first_line.last().unwrap() == &b'\n';
	}
        loop {
            let n = f.read(&mut buffer[..])?;
            if n == 0 {
                break;
            }
            w.write_all(&buffer[..n])?;
	    last_was_cr = first_line.last().unwrap() == &b'\n';
        }
	if pad == PadMode::All && !last_was_cr {
            w.write_all(b"\n")?;
	    last_was_cr = true;
	}
    }
    if pad == PadMode::End && !last_was_cr {
        w.write_all(b"\n")?;
    }
    Ok(())
}
