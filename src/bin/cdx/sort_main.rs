use crate::prelude::*;
use cdx::prelude::*;
use cdx::comp::{comp_check, CompMaker};
use cdx::sort;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Sort lines.", args::FileCount::Many);
    const A: [ArgSpec; 6] = [
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"unique", "u", "", "Print only first of equal lines"},
        arg! {"merge", "m", "", "Merge already sorted files."},
        arg! {"show-comp", "s", "", "Print available comparisons"},
        arg! {"check", "c", "", "Check to see if each input file is sorted."},
        arg! {"Check", "C", "Number", "Check to see if each input file is sorted. Report this many failures before exiting."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut unique = false;
    let mut merge = false;
    let mut comp = LineCompList::new();
    let mut check = false;
    let mut num_checks = 1;
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "merge" {
            merge = true;
        } else if x.name == "check" {
            check = true;
            num_checks = 1;
        } else if x.name == "Check" {
            check = true;
            num_checks = x
                .value
                .to_usize_whole(x.value.as_bytes(), "number of reports")?;
        } else if x.name == "unique" {
            unique = true;
        } else if x.name == "show-comp" {
            CompMaker::help();
            return Ok(());
        } else {
            unreachable!();
        }
    }
    if check && merge {
        return err!("Check and Merge make no sense together");
    }
    if comp.is_empty() {
        comp.add("")?;
    }
    if check {
        let mut reported = 0;
        for x in &files {
            let mut f = Reader::new_open(x)?;
            if f.is_done() {
                continue;
            }
            loop {
                if f.getline()? {
                    break;
                }
                if comp_check(&f, &mut comp, unique) {
                    reported += 1;
                    if reported >= num_checks {
                        break;
                    }
                }
            }
        }
        if reported > 0 {
            return Err(Error::Silent);
        }
    } else {
        let mut w = get_writer("-")?;
        if merge {
            sort::merge(&files, &mut comp, &mut w, unique)?;
        } else {
            sort::sort(&files, comp, &mut w, unique)?;
        }
    }
    Ok(())
}
