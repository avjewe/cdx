//! The prelude

#[doc(inline)]
pub use crate::agg::{AggList, LineAggList};
#[doc(inline)]
pub use crate::column::{ColumnHeader, ColumnSet, DupColHandling, NamedCol, ScopedValues, Writer};
#[doc(inline)]
pub use crate::comp::LineCompList;
#[doc(inline)]
pub use crate::expr::Expr;
#[doc(inline)]
pub use crate::matcher::{LineMatcherList, MatcherList};
#[doc(inline)]
pub use crate::num::NumFormat;
#[doc(inline)]
pub use crate::text::{Case, Text};
#[doc(inline)]
pub use crate::textgen::GenList;
#[doc(inline)]
pub use crate::trans::TransList;
#[doc(inline)]
pub use crate::util::{
    cdx_err, err, get_writer, prerr, CdxError, Error, FileLocList, Reader, Result, StringLine,
    TextFileMode, TextLine, Tri,
};

#[doc(inline)]
pub use anyhow::anyhow;
#[doc(inline)]
pub use std::cmp::Ordering;
#[doc(inline)]
pub use std::fmt;
#[doc(inline)]
pub use std::io::{BufRead, Read, Write};
#[doc(inline)]
pub use std::str::FromStr;
