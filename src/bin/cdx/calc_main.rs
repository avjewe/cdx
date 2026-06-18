use crate::prelude::*;
use cdx::expr::{TrigMode, calc, parse_fmt_expr};
use cdx::prelude::*;
use cdx::util::get_writer;
use cdx::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Evaluate Formatted Expressions.", args::FileCount::Zero);
    const A: [ArgSpec; 2] = [
        arg! {"fmt", "f", "Format", "How to format values."},
        arg! {"trig", "t", "Trigonometric Mode", "Trigonometric mode for trig functions."},
    ];
    let (args, _files) = args::parse(&prog, &A, argv, settings)?;
    let mut fmt = NumFormat::default();
    let mut trig_mode = TrigMode::default();
    for x in args {
        if x.name == "fmt" {
            fmt = NumFormat::new(&x.value)?;
        } else if x.name == "trig" {
            trig_mode = match x.value.as_str() {
                "radians" => TrigMode::Radians,
                "degrees" => TrigMode::Degrees,
                _ => {
                    return Err(anyhow!("Invalid trigonometric mode: {}", x.value));
                }
            };
        } else {
            unreachable!();
        }
    }
    let mut w = get_writer("-")?;
    let mut r = rusty_line::new_reader(">> ")?;
    let r = std::io::BufReader::new(&mut r);

    for line_result in r.lines() {
        let line = line_result?; // Propagate potential I/O errors
        let (f2, exp) = parse_fmt_expr(fmt, &line);
        fmt = f2;
        match calc(exp, trig_mode) {
            Ok(v) => {
                fmt.print(v, &mut w.0)?;
                w.write_all(b"\n")?;
                w.flush()?;
            }
            Err(e) => eprintln!("{}", e),
        }
    }
    Ok(())
}
