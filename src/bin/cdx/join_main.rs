use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::{comp, err, get_writer, Error, Reader, Result};
use std::cmp::Ordering;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 1] = [arg! {"key", "k", "Spec", "How to compare adjacent lines"}];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut key = "".to_string();
    for x in args {
        if x.name == "key" {
            key = x.value;
        } else {
            unreachable!();
        }
    }

    if files.len() != 2 {
        return err!("{} files specified, exactly two required", files.len());
    }

    let mut f1 = Reader::new();
    let mut f2 = Reader::new();
    f1.open(&files[0])?;
    f2.open(&files[1])?;
    if f1.is_empty() && f2.is_empty() {
        return Ok(());
    }

    let mut w = get_writer("-")?;
    // FIXME header

    let mut comp = comp::make_comp(&key)?;
    comp.lookup(&f1.names())?;

    loop {
        if f1.is_done() || f2.is_done() {
            break;
        }
        match comp.comp_cols(f1.curr(), f2.curr()) {
            Ordering::Equal => {
                // print
                f1.write(&mut w)?;
                f1.getline()?;
                f2.getline()?;
            }
            Ordering::Less => {
                f1.getline()?;
            }
            Ordering::Greater => {
                f2.getline()?;
            }
        }
    }
    // drain f1 or f2 as needed
    Ok(())
}
