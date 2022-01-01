#![allow(dead_code)]

use crate::args::ArgSpec;
//use crate::{arg, arg_enum, arg_pos, args};
use crate::args;
use cdx::expr::calc;
use cdx::util::{Reader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Evaluate Expressions.", args::FileCount::Many);
    const A: [ArgSpec; 0] = [
//        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
//        arg! {"key", "k", "Spec", "How to compare value to lines"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    for x in args {
        if x.name == "header" {
        } else {
            unreachable!();
        }
    }

    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() || f.is_done() {
            continue;
        }
        loop {
            match calc(&String::from_utf8_lossy(f.curr_nl())) {
                Ok(v) => println!("{}", v),
                Err(e) => eprintln!("{}", e),
            }
            if f.getline()? {
                break;
            }
        }
    }
    Ok(())
}
