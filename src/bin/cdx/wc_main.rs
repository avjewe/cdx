use crate::prelude::*;
use cdx::prelude::*;

/*

--report-every-million-input-lines

*/
struct NamedAgg {
    name: String,
    agg: AggList,
}
impl NamedAgg {
    fn new(name: &str, agg: AggList) -> Self {
        Self {
            name: name.to_string(),
            agg,
        }
    }
}

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Aggregate info on whole lines.", args::FileCount::Many);
    const A: [ArgSpec; 10] = [
        arg! {"agg", "a", "NewCol,Spec", "Merge values into new column."},
        arg! {"lines", "l", "", "Shortcut for '--agg lines,count'"},
        arg! {"bytes", "b", "", "Shortcut for '--agg bytes,asum,chars'"},
        arg! {"chars", "c", "", "Shortcut for '--agg chars,asum,utf8.chars'"},
        arg! {"words", "w", "", "Shortcut for '--agg words,asum,swords'"},
        arg! {"file", "f", "Tri,ColName", "Should we add the filename as the first column?"},
        arg! {"with-header", "h", "Tri", "Should we write a cdx header?"},
        arg! {"total", "t", "yes,no,maybe,only", "Should we write the totals line?"},
        arg! {"format", "F", "plain,float,power2,power10", "Format for output numbers."},
        arg! {"columns", "C", "", "Count each column separately."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut agg = AggList::new();
    let mut file_name_col = "file".to_string();
    let mut show_file_name = Tri::Maybe;
    let mut show_header = Tri::Maybe;
    let mut show_totals = Tri::Maybe;
    let mut total_only = false;
    let mut do_columns = false;
    let mut fmt = NumFormat::default();
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
            fmt = NumFormat::new(&x.value)?;
        } else if x.name == "columns" {
            do_columns = true;
        } else if x.name == "total" {
            if x.value.eq_ignore_ascii_case("only") {
                total_only = true;
            } else {
                show_totals = Tri::new(&x.value)?;
            }
        } else if x.name == "with-header" {
            show_header = Tri::new(&x.value)?;
        } else if x.name == "file" {
            if let Some((a, b)) = x.value.split_once(',') {
                show_file_name = Tri::new(a)?;
                file_name_col = b.to_string();
            } else {
                show_file_name = Tri::new(&x.value)?;
            }
        } else {
            unreachable!();
        }
    }
    let nada = TextLine::new();
    let mut w = get_writer("-")?;
    let mut first_file = true;
    let mut totals = Vec::new();
    let do_totals;
    let show_file;
    agg.fmt(fmt);
    if do_columns {
        if agg.is_empty() {
            agg.push("bytes,asum,chars")?;
        }
        totals.resize(agg.len(), 0.0);
        let mut aggs: Vec<NamedAgg> = Vec::new();
        let mut colmap: Vec<usize> = Vec::new();
        for x in &files {
            let mut f = Reader::new(&settings.text_in);
            f.open(x)?;
            if f.is_empty() {
                continue;
            }
            colmap.clear();
            for x in &f.names() {
                if let Some(pos) = aggs.iter().position(|agg| agg.name == *x) {
                    colmap.push(pos);
                } else {
                    colmap.push(aggs.len());
                    aggs.push(NamedAgg::new(x, agg.deep_clone()));
                }
            }
            if f.is_done() {
                break;
            }
            loop {
                for (i, x) in f.curr_line().iter().enumerate() {
                    aggs[colmap[i]].agg.add(x);
                }
                if f.getline()? {
                    break;
                }
            }
        }
        show_file = match show_file_name {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => aggs.len() > 1 && !total_only,
        };
        do_totals = match show_totals {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => aggs.len() > 1,
        } || total_only;
        let do_header = match show_header {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => agg.len() > 1 || show_file,
        };
        if do_header {
            let mut ch = ColumnHeader::new();
            if show_file_name != Tri::No {
                ch.push("column")?;
            }
            let mut c_write = Writer::new(settings.text_out());
            agg.fill(&mut c_write);
            c_write.add_names(&mut ch, &StringLine::new())?;
            w.write_all(ch.get_head(&settings.text_out()).as_bytes())?;
        }
        if !total_only {
            for x in &aggs {
                if show_file_name != Tri::No {
                    w.write_all(x.name.as_bytes())?;
                    w.write_all(b"\t")?;
                }
                for i in 0..x.agg.len() {
                    if i != 0 {
                        w.write_all(b"\t")?;
                    }
                    x.agg.get(i).agg.borrow_mut().result(&mut w.0, fmt)?;
                }
                w.write_all(b"\n")?;
            }
        }
        if do_totals {
            for x in &aggs {
                #[allow(clippy::needless_range_loop)]
                for i in 0..agg.len() {
                    totals[i] += x.agg.get(i).agg.borrow().value();
                }
            }
        }
    } else {
        if agg.is_empty() {
            agg.push("lines,count")?;
        }
        totals.resize(agg.len(), 0.0);
        show_file = match show_file_name {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => files.len() > 1 && !total_only,
        };
        do_totals = match show_totals {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => files.len() > 1,
        } || total_only;
        let do_header = match show_header {
            Tri::Yes => true,
            Tri::No => false,
            Tri::Maybe => agg.len() > 1 || show_file,
        };
        for x in &files {
            let mut f = Reader::new(&settings.text_in);
            f.open(x)?;
            if f.is_empty() {
                continue;
            }
            let mut c_write = Writer::new(settings.text_out2(f.delim()));
            agg.fill(&mut c_write);
            c_write.lookup(&f.names())?;

            if do_header && first_file {
                first_file = false;
                let mut ch = ColumnHeader::new();
                if show_file {
                    ch.push(&file_name_col)?;
                }
                c_write.add_names(&mut ch, f.header())?;
                w.write_all(ch.get_head(&settings.text_out()).as_bytes())?;
            }

            if f.is_done() {
                continue;
            }

            f.do_split(false);
            loop {
                agg.add(f.curr_line().line());
                if f.getline()? {
                    break;
                }
            }
            if !total_only {
                if show_file {
                    w.write_all(x.as_bytes())?;
                    w.write_all(b"\t")?;
                }
                c_write.write(&mut w.0, &nada)?;
            }
            #[allow(clippy::needless_range_loop)]
            for i in 0..agg.len() {
                totals[i] += agg.get(i).agg.borrow().value();
            }
            agg.reset();
        }
    }
    if do_totals {
        if show_file {
            w.write_all(b"totals\t")?;
        }
        for (i, t) in totals.iter().enumerate() {
            if i != 0 {
                w.write_all(b"\t")?;
            }
            fmt.print(*t, &mut w.0)?;
        }
        w.write_all(b"\n")?;
    }
    Ok(())
}
