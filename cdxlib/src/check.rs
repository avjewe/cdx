//! Test if a line matches some criterea

use crate::column::NamedCol;
use crate::{err, Error, Result, TextLine};
use std::fmt;

/// Match with no context
pub trait Check {
    /// is the line ok?
    fn check(&self, line: &TextLine) -> bool;
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()>;
}

/// context for exprs
#[derive(Debug, Clone, Copy)]
pub struct ExprContext {}

/// Match that needs an expr context
pub trait ExprCheck {
    /// is the line ok?
    fn expr_check(&mut self, line: &TextLine, expr: &ExprContext) -> bool;
}

/// Full list of checkers
pub struct CheckList {
    checks: Vec<Box<dyn Check>>,
}
impl fmt::Debug for CheckList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CheckList")
    }
}

impl CheckList {
    /// new
    pub fn new() -> Self {
        Self { checks: Vec::new() }
    }
    /// add Check to list
    pub fn push(&mut self, item: Box<dyn Check>) {
        self.checks.push(item)
    }
    /// lookup
    pub fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        for x in self.checks.iter_mut() {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
    /// ok
    pub fn ok(&self, line: &TextLine) -> bool {
        for x in &self.checks {
            if !x.check(line) {
                return false;
            }
        }
        true
    }
    /// ok_verbose
    pub fn ok_verbose(&self, line: &TextLine) -> bool {
        for x in &self.checks {
            if !x.check(line) {
                return false;
            }
        }
        true
    }
}

impl Default for CheckList {
    fn default() -> Self {
        Self::new()
    }
}

/// Check a single column against a regex
#[derive(Debug)]
pub struct CheckRegex {
    col: NamedCol,
    regex: regex::bytes::Regex,
}

impl CheckRegex {
    /// new
    pub fn new(spec: &str) -> Result<Self> {
        let mut col = NamedCol::new();
        let rest = col.parse(spec)?;
        if rest.len() < 2 || rest.as_bytes()[0] != b',' {
            return err!("Match Spec is Column,Regex, not : {}", spec);
        }
        Ok(Self {
            col,
            regex: regex::bytes::Regex::new(&rest[1..])?,
        })
    }
}

impl Check for CheckRegex {
    /// is the line ok?
    fn check(&self, line: &TextLine) -> bool {
        self.regex.is_match(line.get(self.col.num))
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}
