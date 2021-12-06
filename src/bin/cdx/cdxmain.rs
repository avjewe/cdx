use crate::*;
use std::fmt;

#[derive(Copy, Clone)]
pub struct OneMain {
    pub name: &'static str,
    pub help: &'static str,
    pub proc: fn(&[String]) -> Result<()>,
}

impl fmt::Debug for OneMain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "OneMain {} : {}", self.name, self.help)
    }
}

macro_rules! proc {
    ($a:expr,$b:expr,$c:expr) => {
        OneMain {
            name: $a,
            help: $b,
            proc: $c,
        }
    };
}

pub const MAINLIST: &[OneMain] = &[
    proc! {"binsearch",  "Search sorted files.",  binsearch_main::main},
    proc! {"cat",  "Concatenate Files.",  cat_main::main},
    proc! {"cgrep",  "Search files.",  cgrep_main::main},
    proc! {"cut",  "Select colums.",  cut_main::main},
    proc! {"join",  "Join files on a matching column",  join_main::main},
    proc! {"sort", "Sort files.", sort_main::main},
    proc! {"tooltest", "Test command line tools.", tooltest_main::main},
    proc! {"uniq", "Remove duplicate lines.", uniq_main::main},
];
