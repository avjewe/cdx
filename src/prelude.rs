//! The prelude

#[doc(inline)]
pub use crate::agg::{AggList, LineAggList};
#[doc(inline)]
pub use crate::column::{ColumnHeader, Writer, ColumnSet, DupColHandling, ScopedValues};
#[doc(inline)]
pub use crate::comp::{LineCompList};
#[doc(inline)]
pub use crate::matcher::{Match, MatcherList};
#[doc(inline)]
pub use crate::text::Text;
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
