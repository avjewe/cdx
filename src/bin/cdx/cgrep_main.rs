use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::expr;
use cdx::matcher::*;
use cdx::util::{get_writer, HeaderChecker, HeaderMode, Reader, Result, HEADER_MODE};
use std::str::FromStr;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 7] = [
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-matchers", "s", "", "Print available matchers"},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
        arg! {"invert", "v", "", "Print lines that don't match."},
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut list = LineMatcherList::new(Combiner::And);
    let mut reverse = false;
    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "pattern" {
            list.push_spec(&x.value)?;
        } else if x.name == "show-matchers" {
            MatchMaker::help();
            return Ok(());
        } else if x.name == "or" {
            list.multi = Combiner::Or;
        } else if x.name == "invert" {
            reverse = true;
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

    let mut w = get_writer("-")?;

    let mut first_file = true;

    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        if first_file {
            first_file = false;
            list.lookup(&f.names())?;
        }
        if checker.check_file(&f.cont, &files[0])? {
            f.write_header(&mut w)?;
        }
        if f.is_done() {
            continue;
        }
        loop {
            if list.ok(f.curr_line()) ^ reverse {
                // write previous lines of context if necessary
                f.write_curr(&mut w)?;
            } else {
                // write more lines of context if necessary
            }
            if f.getline()? {
                break;
            }
        }
    }
    Ok(())
}
