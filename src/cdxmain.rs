use crate::*;

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
    proc! {"cut",  "cut stuff",  cut_main::main},
    proc! {"sort", "sort stuff", sort_main::main},
    proc! {"uniq", "uniq stuff", uniq_main::main},
];
