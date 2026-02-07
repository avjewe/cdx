#![allow(dead_code)]

use crate::prelude::*;
use cdx::expr::{calc, parse_fmt_expr};
use cdx::prelude::*;
use cdx::util::get_writer;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Evaluate Formatted Expressions.", args::FileCount::Many);
    const A: [ArgSpec; 1] = [arg! {"fmt", "f", "Format", "How to format values."}];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;
    let mut fmt = NumFormat::default();
    for x in args {
        if x.name == "fmt" {
            fmt = NumFormat::new(&x.value)?;
        } else {
            unreachable!();
        }
    }
    let mut w = get_writer("-")?;
    for x in &files {
        let mut f = Reader::new(&settings.text_in);
        f.open(x)?;
        if f.is_empty() || f.is_done() {
            continue;
        }
        loop {
            let exp = &String::from_utf8_lossy(f.curr_nl());
            let (f2, exp) = parse_fmt_expr(fmt, exp);
            fmt = f2;
            match calc(exp) {
                Ok(v) => {
                    fmt.print(v, &mut w.0)?;
                    w.write_all(b"\n")?;
                    w.flush()?;
                }
                Err(e) => eprintln!("{}", e),
            }
            if f.get_line()? {
                break;
            }
        }
    }
    Ok(())
}
