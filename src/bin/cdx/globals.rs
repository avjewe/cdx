//! global settings and command line arguments available to all tools

use crate::args::ArgValue;
use crate::args::OPT_ARG;
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
    arg_old! {"Input", "I", "Format", "Input text file format"},
    arg_old! {"Output", "O", "Format", "Output text file format"},
    arg_old! {"std-agg", "", OPT_ARG, "Show aggregators"},
    arg_old! {"std-comp", "", OPT_ARG, "Show comparators"},
    arg_old! {"std-const", "", OPT_ARG, "Show constants"},
    arg_old! {"std-func", "", OPT_ARG, "Show functions"},
    arg_old! {"std-gen", "", OPT_ARG, "Show generators"},
    arg_old! {"std-match", "", OPT_ARG, "Show matchers"},
    arg_old! {"std-text", "", OPT_ARG, "Show text format help"},
    arg_old! {"std-trans", "", OPT_ARG, "Show transforms"},
];

pub fn global_args() -> &'static [ArgSpec] {
    &A
}

#[derive(Clone, Debug, Default)]
pub struct Settings {
    /// --header controls how to mix and match input files with different CDX headers
    pub checker: HeaderChecker,
    pub input: cdx::input_file::Config,
    pub output: cdx::output::Spec,
}

impl Settings {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn output(&self, input: &cdx::input_file::Config) -> Result<cdx::output::Config> {
        Ok(cdx::output::Config::from_input_and_spec(input, &self.output))
    }
    pub fn add_std_help(a: clap::Command) -> clap::Command {
        add_arg(a, &arg_old! {"std-help", "", "", "Show help for standard args."}, false)
    }
    pub fn handle_std_help(m: &clap::ArgMatches, help: &str) -> Result<()> {
        if let Some(src) = m.value_source("std-help")
            && src == clap::parser::ValueSource::CommandLine
        {
            // Only the list of standard types
            Self::help();
            cdx_err(CdxError::NoError)
        } else if let Some(src) = m.value_source("help")
            && src == clap::parser::ValueSource::CommandLine
        {
            // Tools, Standard types and such
            println!("{help}");
            cdx_err(CdxError::NoError)
        } else {
            Ok(())
        }
    }
    pub fn help() {
        for x in &A {
            println!("{:12} {} {}", x.name, x.value, x.help);
        }
    }
    pub fn show_std_help(name: &str, value: &str) -> Result<()> {
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
            MatchMaker::help(value);
        } else if name == "std-text" {
            // FIXME
            // TextFileMode::text_help();
        } else if name == "std-trans" {
            TransMaker::help();
        } else if name == "std-help" {
            Self::help();
        } else {
            bail!("Unknown standard help topic: {}", name);
        }
        Ok(())
    }
    pub fn consume(&mut self, args: &[ArgValue]) -> Result<()> {
        for x in args {
            if x.name == "Input" {
                self.input = cdx::input_file::Config::from_spec(&x.value)?;
            } else if x.name == "Output" {
                self.output = cdx::output::Spec::from_spec(&x.value)?;
            } else {
                Self::show_std_help(&x.name, &x.value)?;
                return cdx_err(CdxError::NoError);
            }
        }
        Ok(())
    }
}
