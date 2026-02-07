//! global settings and command line arguments available to all tools

use crate::args::ArgValue;
use crate::args::add_arg;
use crate::prelude::*;
use cdx::agg::AggMaker;
use cdx::comp::CompMaker;
use cdx::expr;
use cdx::matcher::MatchMaker;
use cdx::prelude::*;
use cdx::textgen::GenMaker;
use cdx::trans::TransMaker;
use cdx::util::HeaderChecker;

const A: [ArgSpec; 10] = [
    arg! {"text-in", "", "Format", "Input text file format"},
    arg! {"text-out", "", "Format", "Output text file format"},
    arg! {"std-agg", "", "", "Show aggregators"},
    arg! {"std-comp", "", "", "Show comparators"},
    arg! {"std-const", "", "", "Show constants"},
    arg! {"std-func", "", "", "Show functions"},
    arg! {"std-gen", "", "", "Show generators"},
    arg! {"std-match", "", "", "Show matchers"},
    arg! {"std-text", "", "", "Show text format help"},
    arg! {"std-trans", "", "", "Show transforms"},
];

pub fn global_args() -> &'static [ArgSpec] {
    &A
}

#[derive(Clone, Debug, Default)]
pub struct Settings {
    /// --header controls how to mix and match input files with different CDX headers
    pub checker: HeaderChecker,
    pub text_in: TextFileMode,
    pub text_out: Option<TextFileMode>,
}

impl Settings {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn text_out(&self) -> TextFileMode {
        match self.text_out {
            Some(x) => x,
            None => self.text_in,
        }
    }
    pub fn text_out2(&self, delim: u8) -> TextFileMode {
        match self.text_out {
            Some(x) => x,
            None => {
                let mut ret = self.text_in;
                ret.delim = delim;
                ret
            }
        }
    }
    pub fn add_std_help(a: clap::Command) -> clap::Command {
        add_arg(a, &arg! {"std-help", "", "", "Show help for standard args."}, false)
    }
    pub fn handle_std_help(m: &clap::ArgMatches, help: &str) -> Result<()> {
        if let Some(src) = m.value_source("std-help")
            && src == clap::parser::ValueSource::CommandLine
        {
            Self::help();
            cdx_err(CdxError::NoError)
        } else if let Some(src) = m.value_source("version")
            && src == clap::parser::ValueSource::CommandLine
        {
            println!("cdx {}", args::version());
            cdx_err(CdxError::NoError)
        } else if let Some(src) = m.value_source("help")
            && src == clap::parser::ValueSource::CommandLine
        {
            println!("{help}");
            cdx_err(CdxError::NoError)
        } else {
            Ok(())
        }
    }
    pub fn help() {
        for x in &A {
            eprintln!("{:12} {} {}", x.name, x.value, x.help);
        }
    }
    pub fn show_std_help(name: &str) {
        if name == "std-agg" {
            AggMaker::help();
        } else if name == "std-comp" {
            CompMaker::help();
        } else if name == "std-const" {
            expr::show_const();
        } else if name == "std-func" {
            expr::show_func();
        } else if name == "std-gen" {
            GenMaker::help();
        } else if name == "std-match" {
            MatchMaker::help();
        } else if name == "std-text" {
            TextFileMode::text_help();
        } else if name == "std-trans" {
            TransMaker::help();
        } else if name == "std-help" {
            Self::help();
        } else {
            unreachable!();
        }
    }
    pub fn consume(&mut self, args: &[ArgValue]) -> Result<()> {
        for x in args {
            if x.name == "text-in" {
                self.text_in = TextFileMode::new(&x.value)?;
            } else if x.name == "text-out" {
                self.text_out = Some(TextFileMode::new(&x.value)?);
            } else {
                Self::show_std_help(&x.name);
                return cdx_err(CdxError::NoError);
            }
        }
        Ok(())
    }
}
