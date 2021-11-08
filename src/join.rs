//! Join files together by matching column values

use crate::column::ScopedValue;
use crate::comp::Comparator;
use crate::Result;
use std::fmt;

/// How to search and combine input files
#[derive(Debug, Copy, Clone)]
pub enum JoinType {
    /// 2 files, first never has repeated keys
    Quick,
    /// 2 files, either may have repeated keys up to "limit"
    Full,
    /// N files. Only first may have repeated keys. One output line per unique key.
    Poly,
    /// First file not sorted, binary search remaining files
    /// One output line per match in each file separately.
    Binsearch,
    /// No files are sorted, Make hash table from all but first
    /// One output line per match in each file separately.
    Hash,
}

/// What to do if there would be duplicate column names
pub enum DupColHandling {
    /// Fail if there are duplicate columns names
    Fail,
    /// Drop all but the leftmost of the columns
    LeftMost,
    /// Allow duplicate columns names
    Allowed,
    /// if "foo" is taken, try "foo1", then "foo2" until it's new
    Numeric,
    /// Any would-be duplicate names are change based on this spec
    /// spec is comma delimited list, first applies to first file, second to the second, ...
    /// last also applies to all following files.
    /// ':' is replaced with the original named and '#' is replaced with the file number
    MapIf(String),
    /// As MapIf, but always apply the mapping, even if not a duplicate
    MapAll(String),
}

impl fmt::Debug for DupColHandling {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DupColHandling")
    }
}

/// All the settings needed to join some files
#[derive(Debug)]
pub struct JoinConfig {
    /// the type, default Quick
    pub jtype: JoinType,
    /// input file names
    pub infiles: Vec<String>,
    /// primary output file name, default "-"
    pub match_out: String,
    /// output files for unmatched lines, use empty string to skip. Default is empty list.
    /// e.g. ["", "foo"] write unmatched things from the second file to "foo"
    pub unmatch_out: Vec<String>,
    /// input file numbers to write to primary output when non-matching. Default is empty list.
    pub keep_unmatch: Vec<usize>,
    /// output columns, e.g. 0,1.1-2,2.title-author. Default is joined cols, everything from
    /// file 1 except joined cols, everything from file 2 except joined cols, ...
    out_spec: String,
    /// skip sortedness check, normally join fails if input file is not sorted
    skip_check: bool,
    /// output delimiter, default tab
    out_delim: u8,
    /// lookback limit for JoinType::Full. Default 100.
    lookback_limit: u32,
    /// Comparator - default is "1", that is first column plain comparison
    comp: Comparator,
    /// What to do if join would generate duplicate column names. Default is Fail.
    dups: DupColHandling,
    /// if true, replace empty column with value from other file
    use_other: bool,
    /// default value for non-matching lines. Scoped wrt the output columns.
    empty: ScopedValue,
}

impl JoinConfig {
    /// perform the join
    pub fn join(&self) -> Result<()> {
        Ok(())
    }
}
