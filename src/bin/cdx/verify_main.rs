use crate::args::ArgSpec;
use crate::{arg, args};
use cdx::expr;
use cdx::matcher::*;
use cdx::util::{Error, LookbackReader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Verify file contents.", args::FileCount::Many);
    const A: [ArgSpec; 5] = [
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-matchers", "s", "", "Print available matchers"},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut list = MultiLineMatcher::new(MultiMode::And);
    for x in args {
        if x.name == "pattern" {
            list.push_spec(&x.value)?;
        } else if x.name == "show-matchers" {
            MatchMaker::help();
            return Ok(());
        } else if x.name == "or" {
            list.multi = MultiMode::Or;
        } else if x.name == "show-const" {
            expr::show_const();
            return Ok(());
        } else if x.name == "show-func" {
            expr::show_func();
            return Ok(());
        } else {
            unreachable!();
        }
    }

    let max_fails = 5;
    let mut first_file = true;

    for x in &files {
        let mut f = LookbackReader::new(1);
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        if first_file {
            first_file = false;
            list.lookup(&f.names())?;
        }
        if f.is_done() {
            continue;
        }
        let mut fails = 0;
        loop {
            if !list.ok_verbose(f.curr_line(), f.line_number(), x) {
                fails += 1;
                if fails >= max_fails {
                    break;
                }
            }
            if f.getline()? {
                break;
            }
        }
        if fails > 0 {
            return Err(Error::Silent);
        }
    }
    Ok(())
}
