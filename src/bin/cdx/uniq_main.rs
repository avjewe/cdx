use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::comp::LineCompList;
use cdx::{get_writer, LookbackReader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    const A: [ArgSpec; 1] = [arg! {"key", "k", "Spec", "How to compare adjacent lines"}];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut comp = LineCompList::new();
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
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
    f.write_curr(&mut w)?;

    comp.lookup(&f.names())?;
    f.do_split = comp.need_split();

    loop {
        if f.getline()? {
            break;
        }
        if !comp.equal_cols(f.prev_line(1), f.curr_line()) {
            f.write_curr(&mut w)?;
        }
    }
    Ok(())
}
