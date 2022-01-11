use crate::args::ArgSpec;
use crate::{arg, args};
use cdx::util::Result;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Transpose a file.", args::FileCount::One);
    const A: [ArgSpec; 2] = [
        arg! {"head", "h", "",  "Add ' CDX' before first line."},
        arg! {"lines", "n", "Number",  "Maximum lines to examine."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut head = false;
    let mut lines = usize::MAX;
    for x in args {
        if x.name == "head" {
            head = true;
        } else if x.name == "lines" {
            lines = x.value.parse::<usize>()?;
        } else {
            unreachable!();
        }
    }
    cdx::transpose::transpose(&files[0], head, lines)?;
    Ok(())
}
