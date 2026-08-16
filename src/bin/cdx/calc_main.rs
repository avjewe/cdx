use crate::prelude::*;
use cdx::expr::TrigMode;
use cdx::prelude::*;
use cdx::util::get_writer;
use cdx::*;
use rustyline::DefaultEditor;
use std::path::PathBuf;

const HISTORY_FILE: &str = "calc_history.txt`";

fn get_config_file() -> Option<PathBuf> {
    let config_dir = util::get_config_dir()?;
    Some(config_dir.join(HISTORY_FILE))
}

pub fn main(argv: &[String], settings: &mut Settings) -> Result<()> {
    let prog = args::ProgSpec::new("Evaluate Formatted Expressions.", args::FileCount::Zero);
    const A: [ArgSpec; 4] = [
        arg_old! {"format", "f", "Format", "How to format values."},
        arg_old! {"degrees", "d", "", "Use degrees instead of radians for trig functions."},
        arg_old! {"import", "i", "", "Import a file containing expressions to evaluate."},
        arg_old! {"import-silent", "s", "", "Import a file containing expressions to evaluate. Hide output."},
    ];
    let (args, _files) = args::parse(&prog, &A, argv, settings)?;
    let mut expr = Expr::default();
    let mut w = get_writer("-")?;
    for x in args {
        if x.name == "format" {
            expr.set_fmt(NumFormat::new(&x.value)?);
        } else if x.name == "degrees" {
            expr.set_trig_mode(TrigMode::Degrees);
        } else if x.name == "import" {
            expr.import_file(&x.value, &mut w.0)?;
        } else if x.name == "import-silent" {
            expr.import_file_silent(&x.value)?;
        } else {
            unreachable!();
        }
    }

    let mut rl = DefaultEditor::new()?;
    if let Some(config_file) = get_config_file() {
        drop(rl.load_history(&config_file));
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                expr.do_line(&line, &mut w.0)?;
                w.flush()?;
            }
            Err(_) => break,
        }
    }
    if let Some(config_file) = get_config_file() {
        drop(rl.save_history(&config_file));
    }
    Ok(())
}
