use crate::prelude::*;
use cdx::prelude::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Transpose a file.", args::FileCount::One);
    const A: [ArgSpec; 2] = [
        arg! {"head", "h", "",  "Add ' CDX' before first line."},
        arg! {"lines", "n", "Number",  "Maximum lines to examine."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;

    let mut head = false;
    let mut lines = usize::MAX;
    for x in args {
        if x.name == "head" {
            head = true;
        } else if x.name == "lines" {
            lines = x
                .value
                .to_usize_whole(x.value.as_bytes(), "number of lines")?;
        } else {
            unreachable!();
        }
    }
    cdx::transpose::transpose(&files[0], head, lines)?;
    Ok(())
}
