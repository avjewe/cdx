use crate::arg;
use crate::args;
use cdx::check::*;
use cdx::{get_writer, Error, LookbackReader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    let a = vec![
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
    ];
    let (args, files) = args::parse(&prog, &a, argv);

    let mut list = CheckList::new();
    for x in args {
        if x.name == "pattern" {
            list.push(Box::new(ColChecker::new(&x.value)?));
        } else {
            unreachable!();
        }
    }

    assert_eq!(files.len(), 1);

    let mut f = LookbackReader::new(1);
    f.open(&files[0])?;
    if f.is_empty() {
        return Ok(());
    }
    list.lookup(&f.names())?;
    let mut w = get_writer("-")?;
    f.write_header(&mut w)?;

    if f.is_done() {
        return Ok(());
    }
    let grep_mode = true;
    let reverse = false;
    let max_fails = 5;

    if grep_mode {
        loop {
            if list.ok(f.curr_line()) ^ reverse {
                // write previous lines if necessary
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
            if !list.ok_verbose(f.curr_line()) {
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
