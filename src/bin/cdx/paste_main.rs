use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::column::{ColumnHeader, DupColHandling, ScopedValues};
use cdx::util::{err, get_writer, Error, LookbackReader, Result};
use std::io::Write;

/*
What if different delims
 */

#[derive(PartialEq, Copy, Clone, Debug)]
enum EndMode {
    Exact,
    Early,
    Late,
}
impl EndMode {
    fn new(spec: &str) -> Result<Self> {
        if spec.eq_ignore_ascii_case("exact") {
            Ok(Self::Exact)
        } else if spec.eq_ignore_ascii_case("early") {
            Ok(Self::Early)
        } else if spec.eq_ignore_ascii_case("late") {
            Ok(Self::Late)
        } else {
            err!("End mode must be Exact, Early or Late : '{}'", spec)
        }
    }
}

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Join files on a matching column.", args::FileCount::Many);
    const A: [ArgSpec; 6] = [
        arg_enum! {"end", "e", "Mode", "When to stop. Default Exact", &["Exact", "Early", "Late"]},
        arg! {"default", "d", "ScopedValue", "Use this value for short files."},
        arg! {"last", "l", "", "Use value from last line for short files."},
        arg! {"rename", "r", "old.new,...", "Dupicate column named 'old' is renamed 'new'."},
        arg! {"rename-sloppy", "R", "", "Not an error is some renames not used."},
        arg_enum! {"dups", "D", "Mode", "Duplicate Column Handling", &["Fail", "Allow", "Numeric"]},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut end_mode = EndMode::Exact;
    let mut dflt = ScopedValues::new();
    let mut header = ColumnHeader::new();
    let mut use_last = false;

    for x in args {
        if x.name == "end" {
            end_mode = EndMode::new(&x.value)?;
        } else if x.name == "default" {
            dflt.add(&x.value, ',')?;
        } else if x.name == "dups" {
            header.set_handling(DupColHandling::new(&x.value)?);
        } else if x.name == "rename" {
            header.rename(&x.value)?;
        } else if x.name == "rename-sloppy" {
            header.rename_sloppy();
        } else if x.name == "last" {
            use_last = true;
        } else {
            unreachable!();
        }
    }
    let mut fds: Vec<LookbackReader> = Vec::with_capacity(files.len());
    let mut num_live = 0;
    let mut num_dead = 0;
    let mut do_header = true;
    let mut rngs: Vec<std::ops::Range<usize>> = Vec::new();
    let mut curr_cols = 0;
    for x in &files {
        let mut f = LookbackReader::new(1);
        f.open(x)?;
        if !f.has_header() {
            do_header = false;
        }
        if do_header {
            header.push_all(f.header())?;
        } else {
            header.push_all_unchecked(f.header());
        }
        if f.is_done() {
            num_dead += 1;
        } else {
            num_live += 1;
        }
        rngs.push(std::ops::Range {
            start: curr_cols,
            end: curr_cols + f.header().len(),
        });
        curr_cols += f.header().len();
        fds.push(f);
    }
    let mut w = get_writer("-")?;
    header.check_rename()?;
    if do_header {
        w.write_all(header.get_head(b'\t').as_bytes())?;
    }
    dflt.lookup(&header.fieldnames())?;
    while num_live > 0 {
        if end_mode == EndMode::Exact && num_dead != 0 && num_live != 0 {
            let mut s = String::new();
            if num_dead <= num_live {
                for (i, f) in files.iter().enumerate() {
                    if fds[i].is_done() {
                        if !s.is_empty() {
                            s.push_str(", ");
                        }
                        s.push_str(f);
                    }
                }
                s.push_str(" ended early");
            } else {
                for (i, f) in files.iter().enumerate() {
                    if !fds[i].is_done() {
                        if !s.is_empty() {
                            s.push_str(", ");
                        }
                        s.push_str(f);
                    }
                }
                s.push_str(" still has data");
            }
            return err!("Input files had different lengths. {}", s); // FIXME - which files were short?
        }
        if end_mode == EndMode::Early && num_dead != 0 {
            break;
        }
        let mut need_delim = false;
        for (i, f) in fds.iter_mut().enumerate() {
            if need_delim {
                w.write_all(b"\t")?;
            }
            need_delim = true;
            if f.is_done() {
                if use_last {
                    w.write_all(f.prev_nl(1))?;
                } else {
                    let mut nd = false;
                    for j in rngs[i].start..rngs[i].end {
                        if nd {
                            w.write_all(b"\t")?;
                        }
                        nd = true;
                        w.write_all(dflt.get(j).as_bytes())?;
                    }
                }
            } else {
                w.write_all(f.curr_nl())?;
                if f.getline()? {
                    num_dead += 1;
                    num_live -= 1;
                }
            }
        }
        w.write_all(b"\n")?;
    }
    Ok(())
}
