use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::agg::{AggMaker, AggWholeList};
use cdx::column::{ColumnHeader, Writer};
use cdx::num;
use cdx::util::{get_writer, Reader, Result, TextLine, Tri};
use std::io::Write;

/*

--format - usize, human 10^ human 2^x float w/precision
--columns =>treat each column as a separate named file. Merge if multiple files.
--report-every-million-input-lines

*/
pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Aggregate info on whole lines.", args::FileCount::Many);
    const A: [ArgSpec; 10] = [
        arg! {"agg", "a", "NewCol,Spec", "Merge values into new column."},
        arg! {"lines", "l", "", "Shortcut for '--agg lines,asum,count'"},
        arg! {"bytes", "b", "", "Shortcut for '--agg bytes,asum,chars'"},
        arg! {"chars", "c", "", "Shortcut for '--agg chars,asum,utf8.chars'"},
        arg! {"words", "w", "", "Shortcut for '--agg words,asum,swords'"},
        arg! {"file", "f", "Tri,ColName", "Should we add the filename as the first column?"},
        arg! {"header", "h", "Tri", "Should we write a cdx header?"},
        arg! {"total", "t", "yes,no,maybe,only", "Should we write the totals line?"},
        arg! {"format", "o", "plain,float,power2,power10", "Format for output numbers."},
        arg! {"agg-help", "", "", "Print help for aggregators"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut agg = AggWholeList::new();
    let mut file_name_col = "file".to_string();
    let mut show_file_name = Tri::Maybe;
    let mut show_header = Tri::Maybe;
    let mut show_totals = Tri::Maybe;
    let mut total_only = false;
    let mut fmt = num::NumFormat::default();
    for x in args {
        if x.name == "agg" {
            agg.push(&x.value)?;
        } else if x.name == "lines" {
            agg.push("lines,count")?;
        } else if x.name == "bytes" {
            agg.push("bytes,asum,chars")?;
        } else if x.name == "chars" {
            agg.push("chars,asum,utf8.chars")?;
        } else if x.name == "words" {
            agg.push("words,asum,swords")?;
        } else if x.name == "format" {
            fmt = num::NumFormat::new(&x.value)?;
        } else if x.name == "total" {
            if x.value.eq_ignore_ascii_case("only") {
                total_only = true;
            } else {
                show_totals = Tri::new(&x.value)?;
            }
        } else if x.name == "header" {
            show_header = Tri::new(&x.value)?;
        } else if x.name == "file" {
            if let Some((a, b)) = x.value.split_once(',') {
                show_file_name = Tri::new(a)?;
                file_name_col = b.to_string();
            } else {
                show_file_name = Tri::new(&x.value)?;
            }
        } else if x.name == "agg-help" {
            AggMaker::help();
            return Ok(());
        } else {
            unreachable!();
        }
    }
    let show_file = match show_file_name {
        Tri::Yes => true,
        Tri::No => false,
        Tri::Maybe => files.len() > 1 && !total_only,
    };
    let do_totals = match show_totals {
        Tri::Yes => true,
        Tri::No => false,
        Tri::Maybe => files.len() > 1,
    } || total_only;
    let do_header = match show_header {
        Tri::Yes => true,
        Tri::No => false,
        Tri::Maybe => agg.len() > 1 || show_file,
    };
    if agg.is_empty() {
        agg.push("lines,count")?;
    }
    agg.fmt(fmt);
    let nada = TextLine::new();
    let mut w = get_writer("-")?;
    let mut first_file = true;
    let mut totals = Vec::new();
    totals.resize(agg.len(), 0.0);
    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            break;
        }
        let mut c_write = Writer::new(f.delim());
        agg.fill(&mut c_write);
        c_write.lookup(&f.names())?;

        if do_header && first_file {
            first_file = false;
            let mut ch = ColumnHeader::new();
            if show_file {
                ch.push(&file_name_col)?;
            }
            c_write.add_names(&mut ch, f.header())?;
            w.write_all(ch.get_head(f.delim()).as_bytes())?;
        }

        if f.is_done() {
            break;
        }

        f.do_split = false;
        agg.add(&f.curr_line().line);
        loop {
            if f.getline()? {
                break;
            }
            agg.add(&f.curr_line().line);
        }
        if !total_only {
            if show_file {
                w.write_all(x.as_bytes())?;
                w.write_all(b"\t")?;
            }
            c_write.write(&mut w, &nada)?;
        }
        #[allow(clippy::needless_range_loop)]
        for i in 0..agg.len() {
            totals[i] += agg.get(i).agg.borrow().value();
        }
        agg.reset();
    }
    if do_totals {
        if show_file {
            w.write_all(b"totals\t")?;
        }
        for (i, t) in totals.iter().enumerate() {
            if i != 0 {
                w.write_all(b"\t")?;
            }
            num::format_hnum(*t, fmt, &mut w)?;
        }
        w.write_all(b"\n")?;
    }
    Ok(())
}
