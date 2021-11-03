//! Test if a line matches some criterea

use crate::column::NamedCol;
use crate::{err, Error, Result, TextLine};
use std::fmt;

/// Match against whole line
pub trait LineCheck {
    /// is the line ok?
    fn check(&self, line: &TextLine) -> bool;
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()>;
}

/// Match against buffer
pub trait BufCheck {
    /// is the buffer ok?
    fn scheck(&self, buff: &str) -> bool;
    /// is the line ok?
    fn ucheck(&self, buff: &[u8]) -> bool;
}
/*
/// types of BufCheck
pub enum CheckType {
    Regex,
    Exact,
    Prefix,
    Suffix,
    Substring,
    FileExact,
    Blob,
    Length,
    // XmlAttr
    // DelimSuffix
    // DelimPrefix
    // DelimInfix
    // Json
}

/// Spec for BufCheck
pub struct CheckSpec {
    /// general type
    tttype: CheckType,
    /// true for unicode and &str, false for bytes an &[u8]
    unicode : bool,
    /// u8 to unicode error, true for error, false for lossy
    strict : bool,
    /// true for case sensitive, false for case insensitive
    case_sensitive : bool,
    /// true to invert the match
    negate : bool,
}
*/
/// context for exprs
#[derive(Debug, Clone, Copy)]
pub struct ExprContext {}

/// Match that needs an expr context
pub trait ExprCheck {
    /// is the line ok?
    fn expr_check(&mut self, line: &TextLine, expr: &ExprContext) -> bool;
}

/// Full list of checkers
#[derive(Default)]
pub struct CheckList {
    checks: Vec<Box<dyn LineCheck>>,
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
    pub fn push(&mut self, item: Box<dyn LineCheck>) {
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

impl LineCheck for CheckRegex {
    /// is the line ok?
    fn check(&self, line: &TextLine) -> bool {
        self.regex.is_match(line.get(self.col.num))
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&[u8]]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}
