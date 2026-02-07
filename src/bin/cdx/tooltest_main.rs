use crate::prelude::*;
use cdx::prelude::*;
use cdx::tooltest::*;

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Select uniq lines.", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg! {"verbose", "v", "", "Print name of each test before execution."},
        arg! {"bin", "b", "Dir", "Location of executables to test"},
        arg! {"tmp", "t", "Dir", "Use this as a persistent tmp dir, rather than creating an ephemeral one."},
        arg! {"file-format", "", "", "Describe format for '.test' files."},
    ];
    let (args, files) = args::parse(&prog, &A, argv, settings)?;
    let mut config = Config::new()?;
    let mut verbose = false;
    for x in args {
        if x.name == "bin" {
            config.bin(&x.value);
        } else if x.name == "verbose" {
            verbose = true;
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
        if verbose {
            eprintln!("{}", x);
        }
        config.run(x);
    }
    config.report()
}
