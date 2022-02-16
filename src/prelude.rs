//! The prelude

#[doc(inline)]
pub use crate::agg::{AggList, LineAggList};
#[doc(inline)]
pub use crate::column::{ColumnHeader, Writer, ColumnSet, DupColHandling, ScopedValues, NamedCol};
#[doc(inline)]
pub use crate::comp::{LineCompList};
#[doc(inline)]
pub use crate::matcher::{MatcherList, LineMatcherList};
#[doc(inline)]
pub use crate::num::NumFormat;
#[doc(inline)]
pub use crate::text::{Case,Text};
#[doc(inline)]
pub use crate::textgen::GenList;
#[doc(inline)]
pub use crate::util::{get_writer, Reader, Result, StringLine, TextLine, Tri, err, Error, prerr, FileLocList};

#[doc(inline)]
pub use std::cmp::Ordering;
#[doc(inline)]
pub use std::io::{BufRead, Read, Write};
#[doc(inline)]
pub use std::str::FromStr;
#[doc(inline)]
pub use std::fmt;
