use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::matcher::*;
use cdx::{
    get_writer, prerr, Error, HeaderChecker, HeaderMode, LookbackReader, Result, HEADER_MODE,
};
use std::str::FromStr;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-matchers", "s", "", "Print available matchers"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut checker = HeaderChecker::new();
    let mut list = LineMatchMulti::new();
    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "pattern" {
            list.push(Box::new(ColMatcher::new(&x.value)?));
        } else if x.name == "show-matchers" {
            MatchMaker::help();
            return Ok(());
        } else if x.name == "or" {
            list.multi = MultiMode::Or;
        } else {
            unreachable!();
        }
    }

    let mut w = get_writer("-")?;

    let grep_mode = true;
    let reverse = false;
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
        if checker.check_file(&f.cont, &files[0])? {
            f.write_header(&mut w)?;
        }
        if f.is_done() {
            continue;
        }
        if grep_mode {
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
        } else {
            let mut fails = 0;
            loop {
                if !list.ok(f.curr_line()) {
                    prerr(&[b"Line did not match", &f.curr_line().line]);
                    // list.explain()???
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
    }
    Ok(())
}
