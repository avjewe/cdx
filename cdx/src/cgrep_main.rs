use crate::arg;
use crate::args;
use cdxlib::{get_writer, LookbackReader, Result, Error};
use cdxlib::linespec::*;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    let a = vec![arg! {"key", "k", "Spec", "How to compare adjacent lines"}];
    let (args, files) = args::parse(&prog, &a, argv);

    let mut _key = "".to_string();
    for x in args {
        if x.name == "key" {
            _key = x.value;
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

    let mut w = get_writer("-")?;
    f.write_header(&mut w)?;

    if f.is_done() {
        return Ok(());
    }
    let grep_mode = true;
    let reverse = false;
    let list = CheckList::new();
    let max_fails = 5;
    
    if grep_mode {
	loop {
	    if list.ok(f.curr_line()) ^ reverse {
		// write previous lines if necessary
		f.write_curr(&mut w)?;
	    }
	    else {
		// write more lines of context if necessary
	    }
	    if f.getline()? {
		break;
	    }
	}
    }
    else {
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
