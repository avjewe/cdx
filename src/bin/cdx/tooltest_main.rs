use crate::args::ArgSpec;
use crate::{arg, args};
use cdx::tooltest::*;
use cdx::Result;

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 3] = [
        arg! {"bin", "b", "Dir", "Location of executables to test"},
        arg! {"tmp", "t", "Dir", "Use this as a persistant tmp dir, rather than creating an ephemeral one."},
        arg! {"file-format", "", "", "Describe format for '.test' files."},
    ];
    let (args, files) = args::parse(&prog, &A, argv);
    let mut config = Config::new()?;
    for x in args {
        if x.name == "bin" {
            config.bin(&x.value);
        } else if x.name == "tmp" {
            config.tmp(&x.value)?;
        } else if x.name == "file-format" {
            show_format();
            return Ok(());
        } else {
            unreachable!();
        }
    }
    for x in &files {
        config.run(x)?;
    }
    config.report()
}
