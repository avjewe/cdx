use crate::prelude::*;
use cdx::column::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    const A: [ArgSpec; 5] = [
        arg! {"fields", "f", "Columns", "the columns to select."},
        arg! {"group", "g", "Columns", "the columns in a bunch, e.g. '.group:1-3'"},
        arg! {"expr", "e", "Name:Expr", "The result of an arithmetic expression"},
        arg! {"composite", "c", "Spec", "new value made from parts. e.g. 'stuff:abc^{two}def'"},
        arg_enum! {"dups", "D", "Mode", "Duplicate Column Handling", &["Fail", "Allow", "Numeric"]},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;
    let mut header = ColumnHeader::new();
    let mut v = Writer::new(b'\t');
    for x in args {
        if x.name == "dups" {
            header.set_handling(DupColHandling::new(&x.value)?);
        } else if x.name == "fields" {
            v.push(Box::new(ReaderColumns::new(ColumnSet::from_spec(
                &x.value,
            )?)));
        } else if x.name == "group" {
            v.push(Box::new(ColumnClump::from_spec(&x.value)?));
        } else if x.name == "expr" {
            v.push(Box::new(ColumnExpr::new(&x.value)?));
        } else if x.name == "composite" {
            v.push(Box::new(CompositeColumn::new(&x.value)?));
        } else {
            unreachable!();
        }
    }

    if v.is_empty() {
        bail!("cut requires at lease one --columns or --groups");
    }

    let mut w = get_writer("-")?;
    let mut not_header = String::new();
    for x in &files {
        let mut f = Reader::new(&settings.text_in);
        f.open(x)?;
        if f.is_empty() {
            continue;
        }
        v.lookup(&f.names())?;
        header.clear();
        not_header.clear();
        v.add_names(&mut header, f.header())?;
        if f.has_header() {
            not_header = header.get_head(&settings.text_out());
        }
        if settings.checker.check(not_header.as_bytes(), x)? {
            w.write_all(not_header.as_bytes())?;
        }
        if f.is_done() {
            continue;
        }
        loop {
            v.write(&mut w.0, f.curr())?;
            if f.getline()? {
                break;
            }
        }
    }
    Ok(())
}
