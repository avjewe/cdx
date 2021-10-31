//! Test if a line matches some criterea

use crate::TextLine;

/// Match with no context
pub trait Check {
    /// is the line ok?
    fn check(&mut self, line: &TextLine) -> bool;
}

/// context for exprs
#[derive(Debug, Clone, Copy)]
pub struct ExprContext{}

/// Match that needs an expr context
pub trait ExprCheck {
    /// is the line ok?
    fn expr_check(&mut self, line: &TextLine, expr: &ExprContext) -> bool;
}

/// Full list of checkers
#[derive(Debug, Clone, Copy)]
pub struct CheckList{}

impl CheckList {
    /// new
    pub fn new() -> Self {
	Self{}
    }
    /// ok
    pub fn ok(&self, _line: &TextLine) -> bool {
	false
    }
    /// ok_verbose
    pub fn ok_verbose(&self, _line: &TextLine) -> bool {
	false
    }
}

impl Default for CheckList {
    fn default() -> Self {
        Self::new()
    }
}
