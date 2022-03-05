use crate::prelude::*;
use cdx::comp::comp_check;
use cdx::expr;
use cdx::matcher::Combiner;
use cdx::prelude::*;
use cdx::util::CheckLine;

// output of verify should somehow input to other tools
// MORE MATCHERS
// explicit number of columns
// --part-names -- these named columns must exist, maybe with subset of matchers?
// --full-name - exact column names in order, maybe with subset of matchers?
// total lines and bytes as expr
// total lines and bytes of another file as expr
// Matcher :: a bunch of columns not equaling each other
// Matcher :: two columns compared to each other
// only check first N, or maybe also last N? or maybe that's just a script with head and tail?
// still return OK if N lines or N% of the lines are bad

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Verify file contents.", args::FileCount::Many);
    const A: [ArgSpec; 10] = [
        arg! {"report", "r", "Number", "How many failures to report before exit."},
        arg! {"first", "f", "Op,Value", "'FirstLine Op Value' must be true. E.g LT,a for first line is less than 'a'."},
        arg! {"last", "l", "Op,Value", "'LastLine Op Value' must be true."},
        arg! {"key", "k", "Spec", "How to compare adjacent lines"},
        arg! {"sort", "s", "", "Check that the file is sorted."},
        arg! {"unique", "u", "", "Check that the file is sorted, with unique lines."},
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-matchers", "", "", "Print available matchers"},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut list = LineMatcherList::new_with(Combiner::And);
    let mut comp = LineCompList::new();
    let mut do_sort = false;
    let mut do_unique = false;
    let mut max_fails = 5;
    let mut first: Option<CheckLine> = None;
    let mut last: Option<CheckLine> = None;

    for x in args {
        if x.name == "pattern" {
            list.push(&x.value)?;
        } else if x.name == "key" {
            comp.add(&x.value)?;
        } else if x.name == "or" {
            list.multi = Combiner::Or;
        } else if x.name == "fail" {
            max_fails = x.value.to_usize_whole(x.value.as_bytes(), "max fails")?;
        } else if x.name == "sort" {
            do_sort = true;
        } else if x.name == "first" {
            first = Some(CheckLine::new(&x.value)?);
        } else if x.name == "last" {
            last = Some(CheckLine::new(&x.value)?);
        } else if x.name == "unique" {
            do_sort = true;
            do_unique = true;
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
    if comp.is_empty() {
        comp.add("")?;
    }

    let mut fails = 0;
    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        list.lookup(&f.names())?;
        comp.lookup(&f.names())?;
        if f.is_done() {
            continue;
        }
        if first.is_some()
            && !first.as_ref().unwrap().line_ok_verbose(
                f.curr_line(),
                &mut comp,
                f.line_number(),
            )?
        {
            fails += 1;
        }
        let num_cols = f.names().len();
        loop {
            let mut did_fail = false;
            if f.curr().len() != num_cols {
                eprintln!(
                    "Expected {num_cols} columns, but line {} of {} had {}",
                    f.line_number() + 1,
                    x,
                    f.curr().len()
                );
                did_fail = true;
            }
            if !list.ok_verbose(f.curr_line(), f.line_number(), x) {
                did_fail = true;
            }
            if f.getline()? {
                if last.is_some()
                    && !last.as_ref().unwrap().line_ok_verbose(
                        f.prev_line(1),
                        &mut comp,
                        f.line_number() - 1,
                    )?
                {
                    fails += 1;
                }
                break;
            }
            if do_sort {
                did_fail = did_fail || comp_check(&f, &mut comp, do_unique);
            }
            if did_fail {
                fails += 1;
                if fails >= max_fails {
                    break;
                }
            }
        }
        if fails > 0 {
            return Err(Error::Silent);
        }
    }
    Ok(())
}
