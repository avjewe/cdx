use crate::arg;
use crate::args;
use crate::args::ArgSpec;
use cdx::join::*;
use cdx::{err, Error, Result};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Join files on a matching column.", args::FileCount::Many);
    const A: [ArgSpec; 2] = [
        arg! {"also", "a", "FileNum,FileName", "Write non-matching lines from this file to this file."},
        arg! {"key", "k", "Spec", "How to compare lines"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut config = JoinConfig::new();
    config.infiles = files;
    for x in args {
        if x.name == "key" {
            config.keys.push(x.value);
        } else if x.name == "also" {
            let parts = x.value.split_once(',');
            if let Some((a, b)) = parts {
                config
                    .unmatch_out
                    .push(NoMatch::new(a.parse::<usize>()?, b));
            } else {
                return err!("--also format is FileNum,FileName {}", x.value);
            }
        } else {
            unreachable!();
        }
    }

    config.join()
}
