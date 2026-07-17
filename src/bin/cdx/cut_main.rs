use crate::prelude::*;
use cdx::column::*;
use cdx::prelude::*;
use cdx::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select columns", args::FileCount::Many);
    const A: [ArgSpec; 5] = [
        arg! {"fields", "f", "Columns", "the columns to select."},
        arg! {"group", "g", "Columns", "the columns in a bunch, e.g. '.group:1-3'"},
        arg! {"expr", "e", "Name:Expr", "The result of an arithmetic expression."},
        arg! {"composite", "c", "Spec", "new value made from parts. e.g. 'stuff:abc^{two}def'"},
        arg_enum! {"dups", "D", "Mode",
"Duplicate Column Handling. Values for Mode can be:
    Fail    : Fail if there are duplicate column names (default).
    Allow   : Allow duplicate column names.
    Numeric : Allow duplicate column names. Make them unique by appending a number.",
        &["Fail", "Allow", "Numeric"]},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;
    let mut header = ColumnHeader::new();
    let mut v = Writer::new();
    for x in args {
        if x.name == "dups" {
            header.set_handling(DupColHandling::new(&x.value)?);
        } else if x.name == "fields" {
            v.push(Box::new(ReaderColumns::new(ColumnSet::from_spec(&x.value)?)));
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
    let mut lw = LineWriter::default();
    for x in &files {
        let mut f = TextFile::new(x, &settings.input)?;
        if f.is_empty() {
            continue;
        }
        lw.from_spec(&f, &settings.output);
        v.lookup(f.names())?;
        header.clear();
        v.add_names(&mut header, f.names())?;
        // FIXME! We need a way to have output header but no input header
        if f.has_header() {
            header.add_header(&mut lw)?;
            if settings.checker.check_output(&lw, &f)? {
                lw.write_cdx(&mut w.0)?;
            }
            lw.clear();
        }
        if f.is_done() {
            continue;
        }
        loop {
            v.output(&mut lw, f.values())?;
            lw.write(&mut w.0)?;
            lw.clear();
            if f.get_line()? {
                break;
            }
        }
    }

    Ok(())
}
