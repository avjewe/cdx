use crate::prelude::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Generate a text file.", args::FileCount::Zero);
    const A: [ArgSpec; 4] = [
        arg! {"lines", "n", "Number", "Generate this many lines of data. Default 10."},
        arg! {"column", "c", "Spec", "Generate one column of this type"},
        arg! {"multi", "m", "N,Spec", "Generate N columns of this type"},
        arg! {"with-header", "h", "", "Write a CDX header, with columns named c1, c2, ..."},
    ];
    let (args, _files) = args::parse(&prog, &A, argv, settings)?;

    let mut num_lines = 10;
    let mut list = GenList::new();
    let mut header = false;

    for x in args {
        if x.name == "lines" {
            num_lines = x
                .value
                .to_usize_whole(x.value.as_bytes(), "number of lines")?;
        } else if x.name == "with-header" {
            header = true;
        } else if x.name == "column" {
            list.push(&x.value)?;
        } else if x.name == "multi" {
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
