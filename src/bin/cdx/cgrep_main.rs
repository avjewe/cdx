use crate::prelude::*;
use cdx::expr;
use cdx::matcher::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 7] = [
        arg! {"pattern", "p", "Col,Spec,Pattern", "Select line where this col matches this pattern."},
        arg! {"show-const", "", "", "Print available constants"},
        arg! {"show-func", "", "", "Print available functions"},
        arg! {"or", "o", "", "A line matches if any of the matchers matches."},
        arg! {"invert", "v", "", "Print lines that don't match."},
        arg! {"location", "l", "name:what", "prefix extra columns of location context."},
        arg! {"no-match", "V", "FileName", "Write non-matching lines here."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut list = LineMatcherList::new_with(Combiner::And);
    let mut reverse = false;
    let mut loc = FileLocList::new();
    let mut no_match = None;
    for x in args {
        if x.name == "pattern" {
            list.push(&x.value)?;
        } else if x.name == "or" {
            list.multi = Combiner::Or;
        } else if x.name == "invert" {
            reverse = true;
        } else if x.name == "location" {
            loc.push(&x.value)?;
        } else if x.name == "no-match" {
            no_match = Some(x.value.clone());
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
    let no_match: std::result::Result<Option<cdx::util::Outfile>, anyhow::Error> =
        no_match.map(|s| get_writer(&s)).transpose();
    let mut no_match = no_match?;
    let mut need_no_match_header = true;

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
            if need_no_match_header && let Some(o) = no_match.as_mut() {
                o.write_all(not_header.as_bytes())?;
                need_no_match_header = false;
            }
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
            } else if let Some(o) = no_match.as_mut() {
                o.write_all(f.curr_line().line())?;
            }
            // write more lines of context if necessary
            if f.get_line()? {
                break;
            }
        }
    }
    Ok(())
}
