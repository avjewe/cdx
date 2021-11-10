use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::column::{ColumnClump, ColumnSet, CompositeColumn, ReaderColumns, Writer};
use cdx::{get_writer, Error, Reader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    const A: [ArgSpec; 3] = [
        arg! {"columns", "c", "Columns", "the columns"},
        arg! {"group", "g", "Columns", "the columns in a bunch, e.g. '.group:1-3'"},
        arg! {"composite", "C", "Spec", "new value made from parts. e.g. 'stuff:abc^{two}def'"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut v = Writer::new(b'\t');
    for x in args {
        if x.name == "columns" {
            v.push(Box::new(ReaderColumns::new(ColumnSet::from_spec(&x.value))));
        } else if x.name == "group" {
            v.push(Box::new(ColumnClump::from_spec(&x.value)?));
        } else if x.name == "composite" {
            v.push(Box::new(CompositeColumn::new(&x.value)?));
        } else {
            unreachable!();
        }
    }

    if v.is_empty() {
        return Err(Error::Error(
            "cut requires at lease one --columns or --groups".to_string(),
        ));
    }

    for x in files {
        let mut f = Reader::new();
        f.open(&x)?;
        if f.is_empty() {
            continue;
        }
        v.lookup(&f.names())?;
        let mut w = get_writer("-")?;

        v.write_names(&mut w, f.header())?;
        if f.is_done() {
            continue;
        }
        loop {
            v.write(&mut w, f.curr())?;
            if f.getline()? {
                break;
            }
        }
    }
    Ok(())
}
