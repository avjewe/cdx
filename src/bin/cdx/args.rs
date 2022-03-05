use crate::globals;
use cdx::prelude::*;

#[macro_export]
macro_rules! arg {
    ($a:expr,$b:expr,$c:expr,$d:expr) => {
        args::ArgSpec {
            name: $a,
            short: $b,
            value: $c,
            help: $d,
            values: &[],
            positional: false,
        }
    };
}

#[macro_export]
macro_rules! arg_pos {
    ($a:expr,$c:expr,$d:expr) => {
        args::ArgSpec {
            name: $a,
            short: "",
            value: $c,
            help: $d,
            values: &[],
            positional: true,
        }
    };
}

#[macro_export]
macro_rules! arg_enum {
    ($a:expr,$b:expr,$c:expr,$d:expr,$e:expr) => {
        args::ArgSpec {
            name: $a,
            short: $b,
            value: $c,
            help: $d,
            values: $e,
            positional: false,
        }
    };
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FileCount {
    Zero,
    One,
    Many,
}

#[derive(Debug)]
pub struct ProgSpec {
    pub help: String,
    pub files: FileCount,
    pub author: String,
    pub version: String,
}

/// return current version string
pub fn version() -> String {
    format!(
        "{}.{}.{}{}",
        env!("CARGO_PKG_VERSION_MAJOR"),
        env!("CARGO_PKG_VERSION_MINOR"),
        env!("CARGO_PKG_VERSION_PATCH"),
        option_env!("CARGO_PKG_VERSION_PRE").unwrap_or("")
    )
}
impl ProgSpec {
    pub fn new(help: &str, files: FileCount) -> Self {
        Self {
            help: help.to_string(),
            files,
            author: "avjewe@gmail.com".to_string(),
            version: version(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ArgSpec {
    pub name: &'static str,
    pub short: &'static str,
    pub value: &'static str,
    pub help: &'static str,
    pub values: &'static [&'static str],
    pub positional: bool,
}

#[derive(Debug)]
pub struct ArgValue {
    pub name: String,
    pub value: String,
    pub index: usize,
}

impl ArgValue {
    pub fn new(name: &str, value: &str, index: usize) -> Self {
        Self {
            name: name.to_string(),
            value: value.to_string(),
            index,
        }
    }
}
pub fn add_arg<'t>(a: clap::Command<'t>, x: &ArgSpec, hide_help: bool) -> clap::Command<'t> {
    let mut b = clap::Arg::new(x.name);
    if x.positional {
        b = b.takes_value(true).help(x.help).required(true)
    } else {
        if !x.short.is_empty() {
            b = b.short(x.short.first());
        }
        b = b.long(x.name).help(x.help).multiple_occurrences(true);
        if !x.value.is_empty() {
            b = b.value_name(x.value).number_of_values(1).takes_value(true)
        }
        if !x.values.is_empty() {
            b = b.possible_values(x.values).ignore_case(true);
        }
    }
    b = b.hide_long_help(hide_help);
    a.arg(b)
}
pub fn get_arg(m: &clap::ArgMatches, x: &ArgSpec, v: &mut Vec<ArgValue>) {
    if x.value.is_empty() {
        if m.occurrences_of(x.name) > 0 {
            let ind = m.indices_of(x.name).unwrap().collect::<Vec<_>>();
            for i in ind {
                v.push(ArgValue::new(x.name, "", i));
            }
        }
    } else if let Some(arg) = m.values_of(x.name) {
        let ind = m.indices_of(x.name).unwrap().collect::<Vec<_>>();
        assert_eq!(ind.len(), arg.len());
        for (i, val) in arg.enumerate() {
            v.push(ArgValue::new(x.name, val, ind[i]));
        }
    }
}

pub fn parse(
    prog: &ProgSpec,
    spec: &[ArgSpec],
    argv: &[String],
    glob: &mut globals::Settings,
) -> Result<(Vec<ArgValue>, Vec<String>)> {
    let mut a = clap::Command::new("cdx")
        .version(&*prog.version)
        .author(&*prog.author)
        .about(&*prog.help)
        .global_setting(clap::AppSettings::DeriveDisplayOrder);

    for x in spec {
        a = add_arg(a, x, false);
    }
    a = glob.add_std_help(a);
    for x in globals::global_args() {
        a = add_arg(a, x, true);
    }
    match prog.files {
        FileCount::Zero => {}
        FileCount::One => {
            a = a.arg(clap::Arg::new("input_files").takes_value(true));
        }
        FileCount::Many => {
            a = a.arg(
                clap::Arg::new("input_files")
                    .multiple_occurrences(true)
                    .takes_value(true),
            );
        }
    }
    let m = a.get_matches_from(argv);
    glob.handle_std_help(&m)?;
    let mut v: Vec<ArgValue> = Vec::new();
    for x in globals::global_args() {
        get_arg(&m, x, &mut v);
    }
    glob.consume(&v)?;
    v.clear();
    for x in spec {
        get_arg(&m, x, &mut v);
    }
    // values_of_os
    let mut files: Vec<String> = Vec::new();
    if prog.files != FileCount::Zero {
        if let Some(arg) = m.values_of("input_files") {
            for f in arg {
                files.push(f.to_string());
            }
        }
    }
    if prog.files != FileCount::Zero && files.is_empty() {
        files.push("-".to_string());
    }
    v.sort_by(|a, b| a.index.cmp(&b.index));
    Ok((v, files))
}
