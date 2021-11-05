use crate::arg;
use crate::args;
use cdxlib::column::{ColumnClump, ColumnSet, ReaderColumns, Writer};
use cdxlib::{get_writer, Error, Reader, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    let a = vec![
        arg! {"columns", "c", "Columns", "the columns"},
        arg! {"group", "g", "Columns", "the columns in a bunch"},
    ];
    let (args, files) = args::parse(&prog, &a, argv);

    let mut v = Writer::new(b'\t');
    for x in args {
        if x.name == "columns" {
            let mut s = ColumnSet::new();
            s.add_yes(&x.value);
            v.push(Box::new(ReaderColumns::new(s)));
        } else if x.name == "group" {
            let mut g = ColumnSet::new();
            g.add_yes(&x.value);
            let s = Box::new(ReaderColumns::new(g));
            v.push(Box::new(ColumnClump::new(s, b"group", b',')));
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
