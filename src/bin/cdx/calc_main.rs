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

fn handle_custom(
    rl: &rustyline::Editor<(), rustyline::history::FileHistory>,
    line: &str,
    w: &mut dyn std::io::Write,
) -> Result<()> {
    if let Some(stripped) = line.strip_prefix("#history") {
        let f: &str = stripped.trim();
        if f.is_empty() {
            for h in rl.history().iter() {
                writeln!(w, "{h}")?;
            }
        } else {
            let regex = regex::RegexBuilder::new(f).build()?;
            for h in rl.history().iter() {
                if regex.is_match(h) {
                    writeln!(w, "{h}")?;
                }
            }
        }
    } else {
        bail!("Internal error: handle_custom called with non-custom line: {line}");
    }
    Ok(())
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
    let custom = ["#history"];
    for x in args {
        if x.name == "format" {
            expr.set_fmt(NumFormat::new(&x.value)?);
        } else if x.name == "degrees" {
            expr.set_trig_mode(TrigMode::Degrees);
        } else if x.name == "import" {
            expr.import_file(&x.value, &mut w.0, &custom)?;
        } else if x.name == "import-silent" {
            expr.import_file_silent(&x.value, &custom)?;
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
                if expr.do_line(&line, &mut w.0, &custom)? {
                    handle_custom(&rl, &line, &mut w.0)?;
                }
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
