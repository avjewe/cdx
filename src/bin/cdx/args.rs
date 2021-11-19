#[macro_export]
macro_rules! arg {
    ($a:expr,$b:expr,$c:expr,$d:expr) => {
        args::ArgSpec {
            name: $a,
            short: $b,
            value: $c,
            help: $d,
            values: &[],
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

pub fn parse(prog: &ProgSpec, spec: &[ArgSpec], argv: &[String]) -> (Vec<ArgValue>, Vec<String>) {
    let mut a = clap::App::new("cdx")
        .version(&*prog.version)
        .author(&*prog.author)
        .about(&*prog.help);

    for x in spec {
        let mut b = clap::Arg::with_name(x.name)
            .short(x.short)
            .long(x.name)
            .help(x.help)
            .multiple(true);
        if !x.value.is_empty() {
            b = b.value_name(x.value).number_of_values(1).takes_value(true);
        }
        if !x.values.is_empty() {
            b = b.possible_values(x.values).case_insensitive(true);
        }
        a = a.arg(b);
    }
    match prog.files {
        FileCount::Zero => {}
        FileCount::One => {
            a = a.arg(clap::Arg::with_name("input_files").takes_value(true));
        }
        FileCount::Many => {
            a = a.arg(
                clap::Arg::with_name("input_files")
                    .multiple(true)
                    .takes_value(true),
            );
        }
    }
    let m = a.get_matches_from(argv);
    let mut v: Vec<ArgValue> = Vec::new();
    for x in spec {
        if let Some(arg) = m.values_of_lossy(x.name) {
            let ind = m.indices_of(x.name).unwrap().collect::<Vec<_>>();
            if arg.is_empty() {
                for i in ind {
                    v.push(ArgValue::new(x.name, "", i));
                }
            } else {
                assert_eq!(ind.len(), arg.len());
                for i in 0..ind.len() {
                    v.push(ArgValue::new(x.name, &arg[i], ind[i]));
                }
            }
        }
    }

    let mut files: Vec<String> = Vec::new();
    if let Some(arg) = m.values_of_lossy("input_files") {
        for f in arg {
            files.push(f.to_string());
        }
    } else if prog.files != FileCount::Zero {
        files.push("-".to_string());
    }
    v.sort_by(|a, b| a.index.cmp(&b.index));
    (v, files)
}
