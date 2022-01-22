#![allow(dead_code)]

use crate::args::ArgSpec;
//use crate::{arg, arg_enum, arg_pos, args};
use crate::args;
use cdx::expr::calc;
use cdx::num;
use cdx::util::{prerr, Reader, Result};

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
                Ok(v) => {
                    let mut vec: Vec<u8> = Vec::new();
                    vec.clear();
                    num::format_hnum(v, num::NumFormat::Plain(4), &mut vec)?;
                    prerr(&[&vec]);
                    vec.clear();
                    num::format_hnum(v, num::NumFormat::Float(4), &mut vec)?;
                    prerr(&[&vec]);
                    vec.clear();
                    num::format_hnum(v, num::NumFormat::Power2, &mut vec)?;
                    prerr(&[&vec]);
                    vec.clear();
                    num::format_hnum(v, num::NumFormat::Power10, &mut vec)?;
                    prerr(&[&vec]);
                    //		    println!("{}", num::format_hnum(v as i64, num::NumFormat::Power2, ),
                }
                Err(e) => eprintln!("{}", e),
            }
            if f.getline()? {
                break;
            }
        }
    }
    Ok(())
}
