use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::{comp, get_writer, sort, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Sort lines.", args::FileCount::Many);
    const A: [ArgSpec; 1] = [arg! {"key", "k", "Spec", "How to compare adjacent lines"}];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut key = "".to_string();
    for x in args {
        if x.name == "key" {
            key = x.value;
        } else {
            unreachable!();
        }
    }
    let mut w = get_writer("-")?;
    let mut comp = comp::make_comp(&key)?;
    sort::sort(&files, &mut comp, &mut w)?;
    Ok(())
}
