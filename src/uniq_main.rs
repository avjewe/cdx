use crate::*;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::One);
    let a = vec![arg! {"key", "k", "Spec", "How to compare adjacent lines"}];
    let (args, files) = args::parse(&prog, &a, argv);

    let mut key = "".to_string();
    for x in args {
        if x.name == "key" {
            key = x.value;
        } else {
            unreachable!();
        }
    }

    assert_eq!(files.len(), 1);

    let mut f = LookbackReader::new(1);
    f.open(&files[0])?;
    if f.is_empty() {
        return Ok(());
    }

    let mut w = get_writer("-")?;
    f.write_header(&mut w)?;

    if f.is_done() {
        return Ok(());
    }
    f.write_curr(&mut w)?;

    let mut comp = comp::make_comp(&key)?;
    comp.lookup(&f.names())?;
    f.do_split = comp.need_split();

    loop {
        if f.getline()? {
            break;
        }
        if !comp.equal_cols(f.prev_line(1), f.curr_line()) {
            f.write_curr(&mut w)?;
        }
    }
    Ok(())
}
