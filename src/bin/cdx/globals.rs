//! global settings and command line arguments available to all tools

use crate::args::add_arg;
use crate::args::ArgValue;
use crate::prelude::*;
use cdx::agg::AggMaker;
use cdx::comp::CompMaker;
use cdx::expr;
use cdx::matcher::MatchMaker;
use cdx::prelude::*;
use cdx::textgen::GenMaker;
use cdx::trans::TransMaker;
use cdx::util::{HeaderChecker, HeaderMode, HEADER_MODE};

const A: [ArgSpec; 10] = [
    arg_enum! {"header", "", "Mode", "header requirements", &HEADER_MODE},
    arg! {"text-in", "", "Format", "Input text file format"},
    arg! {"text-out", "", "Format", "Output text file format"},
    arg! {"std-agg", "", "", "Show aggregators"},
    arg! {"std-comp", "", "", "Show comparators"},
    arg! {"std-const", "", "", "Show constants"},
    arg! {"std-func", "", "", "Show functions"},
    arg! {"std-gen", "", "", "Show generators"},
    arg! {"std-match", "", "", "Show matchers"},
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
    pub fn add_std_help<'t>(&self, a: clap::Command<'t>) -> clap::Command<'t> {
        add_arg(
            a,
            &arg! {"std-help", "", "", "Show help for standard args."},
            false,
        )
    }
    pub fn handle_std_help(&self, m: &clap::ArgMatches) -> Result<()> {
        if m.occurrences_of("std-help") > 0 {
            self.help();
            cdx_err(CdxError::NoError)
        } else {
            Ok(())
        }
    }
    pub fn help(&self) {
        for x in &A {
            eprintln!("{:12} {} {}", x.name, x.value, x.help);
        }
    }
    pub fn consume(&mut self, args: &[ArgValue]) -> Result<()> {
        for x in args {
            if x.name == "header" {
                self.checker.mode = HeaderMode::from_str(&x.value)?;
            } else if x.name == "text-in" {
                self.text_in = TextFileMode::new(&x.value)?;
            } else if x.name == "text-out" {
                self.text_out = Some(TextFileMode::new(&x.value)?);
            } else if x.name == "std-agg" {
                AggMaker::help();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-comp" {
                CompMaker::help();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-const" {
                expr::show_const();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-func" {
                expr::show_func();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-gen" {
                GenMaker::help();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-match" {
                MatchMaker::help();
                return cdx_err(CdxError::NoError);
            } else if x.name == "std-trans" {
                TransMaker::help();
                return cdx_err(CdxError::NoError);
            } else if x.name == "foo" {
                eprintln!("Something with side effect");
            } else {
                unreachable!();
            }
        }
        Ok(())
    }
}
