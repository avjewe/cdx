use crate::prelude::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Generate a text file.", args::FileCount::Zero);
    const A: [ArgSpec; 5] = [
        arg! {"lines", "n", "Number", "Generate this many lines of data. Default 10."},
        arg! {"column", "c", "Spec", "Generate one column of this type"},
        arg! {"multi", "m", "N,Spec", "Generate N columns of this type"},
        arg! {"full", "F", "Spec", "Generate a file with this contents."},
        arg! {"with-header", "h", "", "Write a CDX header, with columns named c1, c2, ..."},
    ];
    let (args, _files) = args::parse(&prog, &A, argv, settings)?;

    let mut num_lines = 10;
    let mut list = GenList::new();
    let mut header = false;
    let mut full = String::new();
    let mut bad_full = false;

    for x in args {
        if x.name == "lines" {
            bad_full = true;
            num_lines = x.value.to_usize_whole(x.value.as_bytes(), "number of lines")?;
        } else if x.name == "with-header" {
            header = true;
        } else if x.name == "column" {
            bad_full = true;
            list.push(&x.value)?;
        } else if x.name == "full" {
            if full.is_empty() {
                full = x.value.clone();
            } else {
                return err!("--full may not be used more than once");
            }
        } else if x.name == "multi" {
            bad_full = true;
            if let Some((a, b)) = x.value.split_once(',') {
                let a = a.to_usize_whole(x.value.as_bytes(), "multi prefix")?;
                for _ in 0..a {
                    list.push(b)?;
                }
            } else {
                return err!("Multi format is Number,Spec : '{}'", x.value);
            }
        } else {
            unreachable!();
        }
    }
    if !full.is_empty() {
        if bad_full {
            return err!("When using --full, you may not use --lines, --column or -multi.");
        }
        println!("FULL {full}");
        let mut w = get_writer("-")?;
        let mut filegen = cdx::textgen::GenMaker::full_make(&full)?;
        filegen.text_gen.write(&mut w.0)?;
        return Ok(());
    }
    if list.is_empty() {
        list.push("grid")?;
    }
    let mut w = get_writer("-")?;

    if header {
        w.write_all(b" CDX")?;
        for i in 1..=list.len() {
            write!(w, "\tc{}", i)?;
        }
        w.write_all(b"\n")?;
    }

    for _i in 0..num_lines {
        list.write(&mut w.0, b'\t')?;
    }
    Ok(())
}
