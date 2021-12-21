use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::comp::{CompMaker, LineCompList};
use cdx::{get_writer, sort, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Sort lines.", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"unique", "u", "", "Print only first of equal lines"},
        arg! {"merge", "m", "", "Merge already sorted files."},
        arg! {"show-comp", "s", "", "Print available comparisons"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut unique = false;
    let mut merge = false;
    let mut comp = LineCompList::new();
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "merge" {
            merge = true;
        } else if x.name == "unique" {
            unique = true;
        } else if x.name == "help" {
            CompMaker::help();
            return Ok(());
        } else {
            unreachable!();
        }
    }
    if comp.is_empty() {
        comp.add("")?;
    }
    let mut w = get_writer("-")?;
    if merge {
        sort::merge(&files, &mut comp, &mut w, unique)?;
    } else {
        sort::sort(&files, comp, &mut w, unique)?;
    }
    Ok(())
}
