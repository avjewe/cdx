use crate::{arg, args, arg_enum};
use crate::args::ArgSpec;
use cdx::column::{ColumnClump, ColumnSet, CompositeColumn, ReaderColumns, Writer};
use cdx::{get_writer, Error, Reader, Result, HeaderChecker, HEADER_MODE, HeaderMode};
use std::str::FromStr;
use std::io::Write;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg! {"fields", "f", "Columns", "the columns to select."},
        arg! {"group", "g", "Columns", "the columns in a bunch, e.g. '.group:1-3'"},
        arg! {"composite", "c", "Spec", "new value made from parts. e.g. 'stuff:abc^{two}def'"},
        arg_enum! {"header", "h", "Mode", "header requirements", &HEADER_MODE},
    ];
    let (args, files) = args::parse(&prog, &A, argv);
    let mut checker = HeaderChecker::new();
    let mut v = Writer::new(b'\t');
    for x in args {
        if x.name == "header" {
            checker.mode = HeaderMode::from_str(&x.value)?;
	}
        else if x.name == "fields" {
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

    let mut w = get_writer("-")?;
    let mut not_header : Vec<u8> = Vec::new();
    for x in &files {
        let mut f = Reader::new();
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
	v.lookup(&f.names())?;
	not_header.clear();
	v.write_names(&mut not_header, f.header())?;
	if checker.check(&not_header, x)? && f.cont.has_header {
	    w.write_all(&not_header)?;
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
