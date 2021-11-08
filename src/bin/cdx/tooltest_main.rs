use crate::args::ArgSpec;
use crate::{arg, args};
use cdx::tooltest::*;
use cdx::Result;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 1] = [arg! {"bin", "b", "Dir", "Location of executables to test"}];
    let (args, files) = args::parse(&prog, &A, argv);
    let mut config = Config::new();
    for x in args {
        if x.name == "bin" {
            config.bin(&x.value);
        } else {
            unreachable!();
        }
    }
    for x in &files {
        config.run(x)?;
    }
    config.report()
}
