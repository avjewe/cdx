use crate::prelude::*;
use cdx::comp::comp_check;
use cdx::prelude::*;
use cdx::sort::{SortConfig};

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Sort lines.", args::FileCount::Many);
    const A: [ArgSpec; 7] = [
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"unique", "u", "", "Print only first of equal lines"},
        arg! {"merge", "m", "", "Merge already sorted files."},
        arg! {"check", "c", "", "Check to see if each input file is sorted."},
        arg! {"Check", "C", "Number", "Check to see if each input file is sorted. Report this many failures before exiting."},
        arg! {"alt-sort", "a", "", "Use alternate sort algorithm"},
        arg! {"alt-merge", "A", "", "Use alternate merge algorithm"},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut unique = false;
    let mut merge = false;
    let mut comp = LineCompList::new();
    let mut check = false;
    let mut num_checks = 1;
    let mut config = SortConfig::default();
    for x in args {
        if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "alt-merge" {
	    config.alt_merge = true;
        } else if x.name == "alt-sort" {
	    config.alt_sort = true;
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
            return cdx_err(CdxError::Silent);
        }
    } else {
        let mut w = get_writer("-")?;
        if merge {
            config.merge(&files, &mut comp, &mut w.0, unique)?;
        } else {
            config.sort(&files, comp, &mut w.0, unique)?;
        }
    }
    Ok(())
}
