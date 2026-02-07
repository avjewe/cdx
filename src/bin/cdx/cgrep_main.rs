use crate::prelude::*;
use cdx::expr;
use cdx::matcher::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 6] = [
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
        arg! {"invert", "v", "", "Print lines that don't match."},
        arg! {"location", "l", "name:what", "prefix extra columns of location context."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut list = LineMatcherList::new_with(Combiner::And);
    let mut reverse = false;
    let mut loc = FileLocList::new();
    for x in args {
        if x.name == "pattern" {
            list.push(&x.value)?;
        } else if x.name == "or" {
            list.multi = Combiner::Or;
        } else if x.name == "invert" {
            reverse = true;
        } else if x.name == "location" {
            loc.push(&x.value)?;
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

    let mut w = get_writer("-")?;
    for x in &files {
        let mut f = Reader::new_open(x, &settings.text_in)?;
        if f.is_empty() {
            continue;
        }
        list.lookup(&f.names())?;
        let mut not_header = String::new();
        let mut header = ColumnHeader::new();
        loc.add(&mut header)?;
        header.push_all(f.header())?;
        if f.has_header() {
            not_header = header.get_head(&settings.text_out());
        }
        if settings.checker.check(not_header.as_bytes(), x)? {
            w.write_all(not_header.as_bytes())?;
        }
        if f.is_done() {
            continue;
        }
        loop {
            if list.ok(f.curr_line()) ^ reverse {
                // write previous lines of context if necessary
                loc.write_data(&mut w.0, b'\t', f.loc())?;
                f.write_curr(&mut w.0)?;
            } else {
                // write more lines of context if necessary
            }
            if f.get_line()? {
                break;
            }
        }
    }
    Ok(())
}
