use crate::prelude::*;
use cdx::prelude::*;
use cdx::join::{JoinConfig, OutColSpec, NoMatch};

pub fn main(argv: &[String]) -> Result<()> {
    let prog = args::ProgSpec::new("Join files on a matching column.", args::FileCount::Many);
    const A: [ArgSpec; 4] = [
        arg! {"also", "a", "FileNum,FileName", "Write non-matching lines from this file to this file."},
        arg! {"file", "f", "OutputFile", "Output File Name, default '-' for stdout"},
        arg! {"key", "k", "Spec", "How to compare lines"},
        arg! {"output", "o", "Spec", "Output columns : file.ColumnSet,file.ColumnSet"},
    ];
    let (args, files) = args::parse(&prog, &A, argv);

    let mut config = JoinConfig::new();
    config.infiles = files;
    for x in args {
        if x.name == "key" {
            config.keys.push(x.value);
        } else if x.name == "output" {
            let mut file: Option<usize> = None;
            let mut set = ColumnSet::new();
            for y in x.value.split(',') {
                let z = y.split_once('.');
                if let Some((num, spec)) = z {
                    if !set.is_empty() {
                        config.col_specs.push(OutColSpec::new(file.unwrap(), set));
                        set = ColumnSet::new();
                    }
                    let fnum = num.to_usize_whole(x.value.as_bytes(), "file number")?;
                    if fnum == 0 {
                        return err!("file number must be greater than zero {}", y);
                    }
                    file = Some(fnum - 1);
                    set.add_yes(spec)?;
                } else {
                    if file.is_none() {
                        return err!(
                            "First part of output spec must start with 'file.' {}",
                            x.value
                        );
                    }
                    set.add_yes(y)?;
                }
            }
            if !set.is_empty() {
                config.col_specs.push(OutColSpec::new(file.unwrap(), set));
            }
        } else if x.name == "file" {
            config.match_out = x.value;
        } else if x.name == "also" {
            let parts = x.value.split_once(',');
            if let Some((a, b)) = parts {
                config.unmatch_out.push(NoMatch::new(
                    a.to_usize_whole(x.value.as_bytes(), "file number")?,
                    b,
                ));
            } else {
                return err!("--also format is FileNum,FileName {}", x.value);
            }
        } else {
            unreachable!();
        }
    }
    config.join()
}
