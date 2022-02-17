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

const A: [ArgSpec; 7] = [
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
pub struct Settings {}

impl Settings {
    pub fn new() -> Self {
        Self::default()
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
            Err(Error::NoError)
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
            if x.name == "std-agg" {
                AggMaker::help();
                return Err(Error::NoError);
            } else if x.name == "std-comp" {
                CompMaker::help();
                return Err(Error::NoError);
            } else if x.name == "std-const" {
                expr::show_const();
                return Err(Error::NoError);
            } else if x.name == "std-func" {
                expr::show_func();
                return Err(Error::NoError);
            } else if x.name == "std-gen" {
                GenMaker::help();
                return Err(Error::NoError);
            } else if x.name == "std-match" {
                MatchMaker::help();
                return Err(Error::NoError);
            } else if x.name == "std-trans" {
                TransMaker::help();
                return Err(Error::NoError);
            } else if x.name == "foo" {
                eprintln!("Something with side effect");
            } else {
                unreachable!();
            }
        }
        Ok(())
    }
}
