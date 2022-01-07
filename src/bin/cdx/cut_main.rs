use crate::args::ArgSpec;
use crate::{arg, arg_enum, args};
use cdx::column::{
    ColumnClump, ColumnHeader, ColumnSet, CompositeColumn, DupColHandling, ReaderColumns, Writer,
};
use cdx::util::{get_writer, Error, HeaderChecker, HeaderMode, Reader, Result, HEADER_MODE};
use std::io::Write;
use std::str::FromStr;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    const A: [ArgSpec; 5] = [
        arg! {"fields", "f", "Columns", "the columns to select."},
        arg! {"group", "g", "Columns", "the columns in a bunch, e.g. '.group:1-3'"},
        arg! {"composite", "c", "Spec", "new value made from parts. e.g. 'stuff:abc^{two}def'"},
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
        arg_enum! {"dups", "D", "Mode", "Duplicate Column Handling", &["Fail","Allow","Warn", "Numeric"]},
    ];
    let (args, files) = args::parse(&prog, &A, argv);
    let mut checker = HeaderChecker::new();
    let mut header = ColumnHeader::new();
    let mut v = Writer::new(b'\t');
    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
        } else if x.name == "dups" {
            header.set_handling(DupColHandling::new(&x.value)?);
        } else if x.name == "fields" {
            v.push(Box::new(ReaderColumns::new(ColumnSet::from_spec(
                &x.value,
            )?)));
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

    let mut w = get_writer("-")?;
    let mut not_header = String::new();
    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        v.lookup(&f.names())?;
        header.clear();
        not_header.clear();
        v.add_names(&mut header, f.header())?;
        if f.cont.has_header {
            not_header = header.get_head(b'\t');
        }
        if checker.check(not_header.as_bytes(), x)? {
            w.write_all(not_header.as_bytes())?;
        }
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
