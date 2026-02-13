use cdx::prelude::*;
use cdx::util::{silent, suppress};
use std::env;

pub mod args;
mod binsearch_main;
mod calc_main;
mod cat_main;
mod cdxmain;
mod cgrep_main;
mod cut_main;
mod filegen_main;
pub mod globals;
mod join_main;
mod paste_main;
pub mod prelude;
mod sample_main;
mod sort_main;
mod tabs_main;
mod tooltest_main;
mod transpose_main;
mod uniq_main;
mod verify_main;
mod wc_main;
use crate::globals::Settings;

fn main() {
    let mut settings = Settings::new();
    match inner_main(env::args().collect(), &mut settings) {
        Err(e) => {
            if suppress(&e) {
                std::process::exit(0);
            }
            if silent(&e) {
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

pub fn inner_main(mut args: Vec<String>, settings: &mut Settings) -> Result<()> {
    if args.len() > 1 {
        for x in cdxmain::MAIN_LIST {
            if args[1] == x.name {
                let arg1 = args.remove(1);
                args[0] += " ";
                args[0] += &arg1;
                #[cfg(feature = "track")]
                let tr = cdx::memory_tracker::ResourceTracker::new();
                let result = (x.proc)(&args, settings);
                #[cfg(feature = "track")]
                tr.report("main");
                return result;
            }
        }
    }

    let mut cmd = clap::Command::new("cdx")
        .version("0.1.22")
        .author("avjewe@gmail.com")
        .about("Transform text files into other text files.")
        .disable_help_flag(true)
        .disable_version_flag(true);

    for x in cdxmain::MAIN_LIST {
        cmd = cmd.subcommand(clap::Command::new(x.name).about(x.help));
    }
    for i in 2..globals::global_args().len() {
        cmd = args::add_arg(cmd, &globals::global_args()[i], false);
    }
    cmd = globals::Settings::add_std_help(cmd);
    cmd = args::add_help(cmd);

    if args.len() < 2 {
        cmd.print_help()?;
        return Ok(());
    }
    let help = cmd.render_help().to_string();
    let matches = cmd.get_matches_from(&args);
    globals::Settings::handle_std_help(&matches, &help)?;
    for i in 2..globals::global_args().len() {
        let name = globals::global_args()[i].name;
        if matches.get_one::<String>(name).is_some() {
            globals::Settings::show_std_help(name);
        }
    }
    if matches.get_one::<String>("std-help").is_some() {
        globals::Settings::show_std_help("std-help");
    }

    Ok(())
    /*
    if args.len() < 2 {
        eprintln!("USAGE : cdx <command> [options...]");
        eprintln!("Type 'cdx help' for more details");
        return cdx_err(CdxError::Silent);
    }
    if args[1] == "help" || args[1] == "--help" {
        println!("USAGE : cdx <command> [options...]");
        println!("Commands are :");
        for x in cdxmain::MAIN_LIST {
            println!("{:8} : {}", x.name, x.help);
        }
        return Ok(());
    }
    if args[1] == "version" || args[1] == "--version" {
        println!("cdx version {}", args::version());
        return Ok(());
    }
    for x in cdxmain::MAIN_LIST {
        if args[1] == x.name {
            let arg1 = args.remove(1);
            args[0] += " ";
            args[0] += &arg1;
            return (x.proc)(&args, settings);
        }
    }
    eprintln!("Valid subcommands are :");
    for x in cdxmain::MAIN_LIST {
        eprintln!("{:8} : {}", x.name, x.help);
    }
    cdx_err(CdxError::Silent)
    */
}
