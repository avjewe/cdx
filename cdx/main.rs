use cdx::{err,cdxmain,Error};
use std::env;

fn main() {
    match inner_main(env::args().collect()) {
        Err(e) => {
            if e.suppress() {
                std::process::exit(0);
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

fn inner_main(mut args: Vec<String>) -> cdx::Result<()> {
    if args.len() < 2 {
	return err!("USAGE : cdx <command> [options...]");
    }
    for x in cdxmain::MAINLIST {
        if args[1] == x.name {
            let arg1 = args.remove(1);
            args[0] += " ";
            args[0] += &arg1;
            return (x.proc)(&args);
        }
    }
    err!("Subcommands are 'cut' and 'uniq'")
}
