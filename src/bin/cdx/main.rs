use cdx::util::{Error, Result};
use std::env;

mod args;
mod binsearch_main;
mod calc_main;
mod cat_main;
mod cdxmain;
mod cgrep_main;
mod cut_main;
mod join_main;
mod paste_main;
mod sample_main;
mod sort_main;
mod tabs_main;
mod tooltest_main;
mod transpose_main;
mod uniq_main;
mod verify_main;
mod wc_main;

fn main() {
    match inner_main(env::args().collect()) {
        Err(e) => {
            if e.suppress() {
                std::process::exit(0);
            }
            if e.silent() {
                std::process::exit(1);
            }
            eprintln!("Error\t{}", e);
            eprint!("Command\t");
            for x in env::args() {
                eprint!("{} ", x);
            }
            eprintln!();
            std::process::exit(1);
        }
        Ok(()) => {
            std::process::exit(0);
        }
    }
}

fn inner_main(mut args: Vec<String>) -> Result<()> {
    if args.len() < 2 {
        eprintln!("USAGE : cdx <command> [options...]");
        eprintln!("Type 'cdx help' for more details");
        return Err(Error::Silent);
    }
    if args[1] == "help" || args[1] == "--help" {
        println!("USAGE : cdx <command> [options...]");
        println!("Commands are :");
        for x in cdxmain::MAINLIST {
            println!("{:8} : {}", x.name, x.help);
        }
        return Ok(());
    }
    if args[1] == "version" || args[1] == "--version" {
        println!("cdx version {}", args::version());
        return Ok(());
    }
    for x in cdxmain::MAINLIST {
        if args[1] == x.name {
            let arg1 = args.remove(1);
            args[0] += " ";
            args[0] += &arg1;
            return (x.proc)(&args);
        }
    }
    eprintln!("Valid subcommands are :");
    for x in cdxmain::MAINLIST {
        eprintln!("{:8} : {}", x.name, x.help);
    }
    Err(Error::Silent)
}
