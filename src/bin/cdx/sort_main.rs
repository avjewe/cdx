use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::{comp, get_writer, sort, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Sort lines.", args::FileCount::Many);
    const A: [ArgSpec; 3] = [
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"unique", "u", "", "Print only first of equal lines"},
        arg! {"merge", "m", "", "Merge already sorted files."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut key = "".to_string();
    let mut unique = false;
    let mut merge = false;
    for x in args {
        if x.name == "key" {
            key = x.value;
        } else if x.name == "merge" {
            merge = true;
        } else if x.name == "unique" {
            unique = true;
        } else {
            unreachable!();
        }
    }
    let mut w = get_writer("-")?;
    let mut comp = comp::make_comp(&key)?;
    if merge {
        sort::merge(&files, &mut comp, &mut w, unique)?;
    } else {
        sort::sort(&files, comp, &mut w, unique)?;
    }
    Ok(())
}
