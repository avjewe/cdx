//! Test if a [`TextLine`], `str` or `&[u8]` matches a pattern
//!
//! A `Matcher` is a generalization of a Regular Expression.
//! It answers "does a pattern match a target", but you supply a
//! method, in addition to a pattern. The `regex` crate provides
//! one such method, but many other, simpler, methods are also available.
//! See <https://avjewe.github.io/cdxdoc/Matcher.html> for a list of matchers,
//! and other details about the syntax for specifying lagauges,
//!
//! Most uses will involve [Matcher]s, collected into [`MatcherList`]s, or
//!
//!```
//! use cdx::matcher::*;
//! use cdx::prelude::*;
//! let matcher = MatchMaker::make("prefix,abc")?;
//! assert!(matcher.umatch(b"abcdef"));
//! assert!(!matcher.umatch(b"bcdef"));
//! assert!(matcher.smatch("abcdef"));
//! assert!(!matcher.smatch("bcdef"));
//!
//! let mut list = LineMatcherList::new_with(Combiner::And);
//! list.push("two,glob,b*")?;
//! let input = "<< CDX\tone\ttwo\naaa\tbbb\nccc\tddd\n";
//! let mut reader = Reader::new(&TextFileMode::default());
//! reader.open(input);
//! list.lookup(&reader.names())?;
//! assert!(list.ok(reader.curr_line()));
//! reader.getline()?;
//! assert!(!list.ok(reader.curr_line()));
//! # Ok::<(), cdx::util::Error>(())
//!```

use crate::comp::{Comp, CompMaker};
use crate::num;
use crate::prelude::*;
use crate::util::{self, CheckBuff, CompareOp};
use memchr::memmem::find;
use regex::Regex;
use std::collections::HashSet;
use std::fmt::Write;
use std::sync::Mutex;

/// Match against [`TextLine`]
pub trait LineMatch {
    /// Is this line ok?
    fn ok(&mut self, line: &TextLine) -> bool;
    /// Resolve any named columns.
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()>;
    /// Human readable desription
    fn show(&self) -> String;
    /// Is this line ok? If not, write explanation to stderr
    fn ok_verbose(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        let ret = self.ok(line);
        if !ret {
            eprintln!("Line {} of {} failed to {}", line_num, fname, self.show());
        }
        ret
    }
}

const fn not_str(negate: bool) -> &'static str {
    if negate { "not-" } else { "" }
}

/// Match against `str` or `&[u8]`
pub trait Match {
    /// Are these characters ok?
    fn smatch(&self, buff: &str) -> bool;
    /// Are these bytes ok?
    fn umatch(&self, buff: &[u8]) -> bool;
    /// Human readable desription
    fn show(&self) -> String {
        format!("stuff {}", 27)
    }
    /// smatch, but print to stderr if fail
    fn verbose_smatch(&self, buff: &str, negate: bool) -> bool {
        let res = self.smatch(buff);
        if res != negate {
            eprintln!("Failed to {}match {} against {}", not_str(negate), buff, self.show());
        }
        res
    }
    /// umatch, but print to stderr if fail
    fn verbose_umatch(&self, buff: &[u8], negate: bool) -> bool {
        let res = self.umatch(buff);
        if res != negate {
            eprintln!(
                "Failed to {}match {} against {}",
                not_str(negate),
                String::from_utf8_lossy(buff),
                self.show()
            );
        }
        res
    }
}

impl fmt::Debug for dyn Match + '_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.show())
    }
}
impl fmt::Display for dyn Match + '_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.show())
    }
}
impl fmt::Debug for dyn LineMatch + '_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.show())
    }
}
impl fmt::Display for dyn LineMatch + '_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.show())
    }
}

/// a threshhold
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Thresh {
    /// number of items
    Count(usize),
    /// must be 0.0..=1.0
    /// 0/0 == 0
    Frac(f64),
}
impl Default for Thresh {
    fn default() -> Self {
        Self::Count(0)
    }
}
impl Thresh {
    /// If decimal point then frac, else count
    pub fn new(spec: &str) -> Result<Self> {
        if spec.contains('.') {
            Self::new_frac(spec.to_f64_whole(spec.as_bytes(), "Threshhold for Determiner")?)
        } else {
            Ok(Self::new_count(spec.to_usize_whole(spec.as_bytes(), "Threshhold for Determiner")?))
        }
    }
    /// new Count
    #[must_use]
    pub const fn new_count(c: usize) -> Self {
        Self::Count(c)
    }
    /// new Frac
    pub fn new_frac(c: f64) -> Result<Self> {
        if (0.0..=1.0).contains(&c) {
            Ok(Self::Frac(c))
        } else {
            err!("Value {c} must be between 0 and 1 inclusive")
        }
    }
    /// we've seen this many, with more to come. Are we done yet?
    #[must_use]
    pub const fn at_most_mid(&self, n: usize) -> Tri {
        match self {
            Self::Count(c) => Tri::no_if(n > *c),
            Self::Frac(_) => Tri::Maybe,
        }
    }
    /// we've seen this many, with more to come. Are we done yet?
    #[must_use]
    pub const fn at_least_mid(&self, n: usize) -> Tri {
        match self {
            Self::Count(c) => Tri::yes_if(n >= *c),
            Self::Frac(_) => Tri::Maybe,
        }
    }
    /// we've seen this many, is that a match?
    #[must_use]
    pub fn at_most_final(&self, n: usize, tot: usize) -> bool {
        match self {
            Self::Count(c) => n <= *c,
            Self::Frac(f) => num::f64_less(*f, n, tot),
        }
    }
    /// we've seen this many, is that a match?
    #[must_use]
    pub fn at_least_final(&self, n: usize, tot: usize) -> bool {
        match self {
            Self::Count(c) => n >= *c,
            Self::Frac(f) => num::f64_greater(*f, n, tot),
        }
    }
    /// we've seen this many, is that a match?
    #[must_use]
    pub fn exactly(&self, n: usize, tot: usize) -> bool {
        match self {
            Self::Count(c) => n == *c,
            Self::Frac(f) => num::f64_equal(*f, n, tot),
        }
    }
}

/// Given some YES's and some NO's, do we match?
#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub enum Determiner {
    /// no occurrences of NO
    All,
    /// at least 1 YES
    #[default]
    Some,
    /// no occurrences of YES
    None,
    /// at least 1 NO
    NotAll,
    /// at least 1 NO and at least 1 YES
    Mixed,
    /// either All or None
    Uniform,
    /// At most this many YES's,
    AtMost(Thresh),
    /// At least this many YES's,
    AtLeast(Thresh),
    /// Exactly this many YES's,
    Exactly(Thresh),
    /// At most this many NO's,
    AtMostNo(Thresh),
    /// At least this many NO's,
    AtLeastNo(Thresh),
    /// Exactly this many NO's,
    ExactlyNo(Thresh),
}

impl Determiner {
    /// new from spec  "all" or "atleast,42"
    pub fn new(spec: &str) -> Result<Self> {
        Ok(if spec.eq_ignore_ascii_case("all") {
            Self::All
        } else if spec.eq_ignore_ascii_case("some") {
            Self::Some
        } else if spec.eq_ignore_ascii_case("none") {
            Self::None
        } else if spec.eq_ignore_ascii_case("notall") {
            Self::NotAll
        } else if spec.eq_ignore_ascii_case("mixed") {
            Self::Mixed
        } else if spec.eq_ignore_ascii_case("uniform") {
            Self::Uniform
        } else if let Some(thresh) = spec.strip_prefix("atmost,") {
            Self::AtMost(Thresh::new(thresh)?)
        } else if let Some(thresh) = spec.strip_prefix("atleast,") {
            Self::AtLeast(Thresh::new(thresh)?)
        } else if let Some(thresh) = spec.strip_prefix("exactly,") {
            Self::Exactly(Thresh::new(thresh)?)
        } else if let Some(thresh) = spec.strip_prefix("atmostno,") {
            Self::AtMostNo(Thresh::new(thresh)?)
        } else if let Some(thresh) = spec.strip_prefix("atleastno,") {
            Self::AtLeastNo(Thresh::new(thresh)?)
        } else if let Some(thresh) = spec.strip_prefix("exactlyno,") {
            Self::ExactlyNo(Thresh::new(thresh)?)
        } else {
            return err!("Unknown Determiner {spec}");
        })
    }
    /// we've seen this many yes's and this many no's, with more to come. Are we done yet?
    #[must_use]
    pub const fn match_mid(&self, yes: usize, no: usize) -> Tri {
        use Determiner::{
            All, AtLeast, AtLeastNo, AtMost, AtMostNo, Exactly, ExactlyNo, Mixed, None, NotAll,
            Some, Uniform,
        };
        match self {
            All => Tri::no_if(no > 0),
            Some => Tri::yes_if(yes > 0),
            None => Tri::no_if(yes > 0),
            NotAll => Tri::yes_if(no > 0),
            Mixed => Tri::yes_if(yes > 0 && no > 0),
            Uniform => Tri::no_if(yes > 0 && no > 0),
            AtMost(t) => t.at_most_mid(yes),
            AtLeast(t) => t.at_least_mid(yes),
            Exactly(t) => t.at_most_mid(yes),
            AtMostNo(t) => t.at_most_mid(no),
            AtLeastNo(t) => t.at_least_mid(no),
            ExactlyNo(t) => t.at_most_mid(no),
        }
    }

    /// we've seen this many yes's and this many no's, with more to come. Is that a match?
    #[must_use]
    pub fn match_final(&self, yes: usize, no: usize) -> bool {
        use Determiner::{
            All, AtLeast, AtLeastNo, AtMost, AtMostNo, Exactly, ExactlyNo, Mixed, None, NotAll,
            Some, Uniform,
        };
        let tot = yes + no;
        match self {
            All => no == 0,
            Some => yes > 0,
            None => yes == 0,
            NotAll => no > 0,
            Mixed => yes > 0 && no > 0,
            Uniform => yes == 0 || no == 0,
            AtMost(t) => t.at_most_final(yes, tot),
            AtLeast(t) => t.at_least_final(yes, tot),
            Exactly(t) => t.exactly(yes, tot),
            AtMostNo(t) => t.at_most_final(no, tot),
            AtLeastNo(t) => t.at_least_final(no, tot),
            ExactlyNo(t) => t.exactly(no, tot),
        }
    }
}

impl Match for CheckBuff {
    fn smatch(&self, buff: &str) -> bool {
        self.umatch(buff.as_bytes())
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.buff_ok(buff)
    }
    fn show(&self) -> String {
        "Compare Match".to_string() // FIXME
    }
}

// pattern is prefix of string
#[derive(Debug, Clone)]
struct PrefixMatch {
    data: String,
}

impl PrefixMatch {
    fn new(data: &str) -> Self {
        Self { data: data.to_string() }
    }
}
impl Match for PrefixMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.starts_with(&self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.starts_with(self.data.as_bytes())
    }
    fn show(&self) -> String {
        format!("Prefix Match of {}", self.data)
    }
}

#[derive(Debug, Clone)]
/// pattern is prefix of string, case insensitive
struct PrefixMatchC {
    s_data: String,
    u_data: Vec<u8>,
}
impl PrefixMatchC {
    fn new(data: &str) -> Self {
        Self { s_data: data.to_lowercase(), u_data: data.as_bytes().to_ascii_lowercase() }
    }
}

fn scase_prefix(haystack: &str, needle: &str) -> bool {
    let mut iter1 = needle.chars();
    let mut iter2 = haystack.chars().flat_map(char::to_lowercase);
    loop {
        let c1 = iter1.next();
        let c2 = iter2.next();
        if c1.is_none() {
            return true;
        }
        if c2.is_none() {
            return false;
        }
        if c1.unwrap() != c2.unwrap() {
            return false;
        }
    }
}

fn bcase_prefix(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.len() > haystack.len() {
        return false;
    }
    for x in 0..needle.len() {
        if haystack[x].to_ascii_lowercase() != needle[x] {
            return false;
        }
    }
    true
}

fn bcase_suffix(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.len() > haystack.len() {
        return false;
    }
    let hay_len = haystack.len() - 1;
    let nee_len = needle.len() - 1;
    for x in 0..needle.len() {
        if haystack[hay_len - x].to_ascii_lowercase() != needle[nee_len - x] {
            return false;
        }
    }
    true
}

// needle is already lowercase
fn scase_suffix(haystack: &str, needle: &str) -> bool {
    let needle_len = needle.chars().count();
    let haystack_len = haystack.chars().flat_map(char::to_lowercase).count();
    if needle_len > haystack_len {
        return false;
    }
    let mut needle_it = needle.chars();
    let mut haystack_it =
        haystack.chars().flat_map(char::to_lowercase).skip(haystack_len - needle_len);

    // at this monent, needle_it.chars().count() == haystack_it.chars().count()
    for _ in 0..needle_len {
        if needle_it.next().unwrap() != haystack_it.next().unwrap() {
            return false;
        }
    }
    true
}

impl Match for PrefixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        scase_prefix(buff, &self.s_data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        bcase_prefix(buff, &self.u_data)
    }
    fn show(&self) -> String {
        format!("Case-insensitive refix Match of {}", self.s_data)
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string
struct SuffixMatch {
    data: String,
}
impl SuffixMatch {
    fn new(data: &str) -> Self {
        Self { data: data.to_string() }
    }
}
impl Match for SuffixMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.ends_with(&self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.ends_with(self.data.as_bytes())
    }
    fn show(&self) -> String {
        format!("Suffix Match of {}", self.data)
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string, case insensitive
struct SuffixMatchC {
    s_data: String,
    u_data: Vec<u8>,
}
impl SuffixMatchC {
    fn new(data: &str) -> Self {
        Self { s_data: data.to_lowercase(), u_data: data.as_bytes().to_ascii_lowercase() }
    }
}
impl Match for SuffixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        scase_suffix(buff, &self.s_data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        bcase_suffix(buff, &self.u_data)
    }
    fn show(&self) -> String {
        format!("Case-insensitive suffix Match of {}", self.s_data)
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string, case insensitive
struct InfixMatchC {
    s_data: String,
    u_data: Vec<u8>,
}
impl InfixMatchC {
    fn new(data: &str) -> Self {
        Self { s_data: data.to_lowercase(), u_data: data.as_bytes().to_ascii_lowercase() }
    }
}
impl Match for InfixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        buff.to_lowercase().contains(&self.s_data) // PERF allocation
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        find(&buff.to_ascii_lowercase(), &self.u_data).is_some() // PERF allocation
    }
    fn show(&self) -> String {
        format!("Case insensitive substring Match of {}", self.s_data)
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string
struct InfixMatch {
    needle: String,
}
impl InfixMatch {
    fn new(data: &str) -> Self {
        Self { needle: data.to_string() }
    }
}
impl Match for InfixMatch {
    fn smatch(&self, buff: &str) -> bool {
        find(buff.as_bytes(), self.needle.as_bytes()).is_some()
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        find(buff, self.needle.as_bytes()).is_some()
    }
    fn show(&self) -> String {
        format!("Substring Match of {}", self.needle)
    }
}

#[derive(Debug, Clone)]
/// bytes regex
struct RegexMatch {
    s_data: Regex,
    u_data: regex::bytes::Regex,
    pattern: String,
}
impl RegexMatch {
    fn new(data: &str, case: Case) -> Result<Self> {
        Ok(Self {
            s_data: regex::RegexBuilder::new(data)
                .case_insensitive(case == Case::Insens)
                .build()?,
            u_data: regex::bytes::RegexBuilder::new(data)
                .case_insensitive(case == Case::Insens)
                .build()?,
            pattern: data.to_string(),
        })
    }
}
impl Match for RegexMatch {
    fn smatch(&self, buff: &str) -> bool {
        self.s_data.is_match(buff)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.u_data.is_match(buff)
    }
    fn show(&self) -> String {
        format!("Regex Match of {}", self.pattern)
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern
struct ExactMatch {
    data: String,
}
impl ExactMatch {
    fn new(data: &str) -> Self {
        Self { data: data.to_string() }
    }
}
impl Match for ExactMatch {
    fn smatch(&self, buff: &str) -> bool {
        self.data == buff
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.data.as_bytes() == buff
    }
    fn show(&self) -> String {
        format!("Exact Match of {}", self.data)
    }
}

fn load_hashset(data: &mut HashSet<Vec<u8>>, fname: &str) -> Result<()> {
    let mut f = Reader::new(&TextFileMode::default());
    f.do_split(false);
    f.open(fname)?;
    if f.is_done() {
        return Ok(());
    }
    loop {
        let line = &f.curr().line();
        if line.len() > 1 {
            data.insert(line[0..line.len() - 1].to_vec());
        }
        if f.getline()? {
            break;
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
/// pattern is file name. String exactly matches one line of file.
struct FileExactMatch {
    data: HashSet<Vec<u8>>,
    file_name: String,
}
impl FileExactMatch {
    fn new(file_name: &str) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset(&mut d, file_name)?;
        Ok(Self { data: d, file_name: file_name.to_string() })
    }
}
impl Match for FileExactMatch {
    fn smatch(&self, buff: &str) -> bool {
        self.data.contains(buff.as_bytes())
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.data.contains(buff)
    }
    fn show(&self) -> String {
        format!("Exact Match of one line in file {}", self.file_name)
    }
}

fn load_hashset_c(data: &mut HashSet<Vec<u8>>, fname: &str, unicode: bool) -> Result<()> {
    let mut f = Reader::new(&TextFileMode::default());
    f.do_split(false);
    f.open(fname)?;
    if f.is_done() {
        return Ok(());
    }
    loop {
        let mut line: &[u8] = f.curr().line();
        if line.len() > 1 {
            if line.last().unwrap() == &b'\n' {
                line = &line[..line.len() - 1];
            }
            if unicode {
                data.insert(String::from_utf8(line.to_vec())?.new_lower().into_bytes());
            // PERF - 2 allocations
            } else {
                data.insert(line.new_lower());
            }
        }
        if f.getline()? {
            break;
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
/// pattern is file name. String exactly matches one line of file, case insensitive
struct FileExactMatchC {
    data: HashSet<Vec<u8>>,
    file_name: String,
}
impl FileExactMatchC {
    fn new(file_name: &str, unicode: bool) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset_c(&mut d, file_name, unicode)?;
        Ok(Self { data: d, file_name: file_name.to_string() })
    }
}
impl Match for FileExactMatchC {
    fn smatch(&self, buff: &str) -> bool {
        self.data.contains(&buff.new_lower().into_bytes()) // PERF allocation
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.data.contains(&buff.new_lower()) // PERF allocation
    }
    fn show(&self) -> String {
        format!("Case insensitive match of one line in file {}", self.file_name)
    }
}

#[derive(Debug, Clone)]
/// shell style wildcard match
struct GlobMatch {
    data: String,
    ic: Case,
}
impl GlobMatch {
    fn new(data: &str, ic: Case) -> Self {
        Self { data: data.to_string(), ic }
    }
}
impl Match for GlobMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.glob(&self.data, self.ic)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.glob(self.data.as_bytes(), self.ic)
    }
    fn show(&self) -> String {
        let prefix = if self.ic == Case::Insens { "in" } else { "" };
        format!("Case {}sensitive match of glob {}", prefix, self.data)
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern, case insensitive
struct ExactMatchC {
    s_data: String,
    u_data: Vec<u8>,
}

impl ExactMatchC {
    fn new(data: &str) -> Self {
        Self { s_data: data.new_lower(), u_data: data.as_bytes().new_lower() }
    }
}
impl Match for ExactMatchC {
    fn smatch(&self, buff: &str) -> bool {
        buff.equal_insens_quick(&self.s_data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.equal_insens_quick(&self.u_data)
    }
    fn show(&self) -> String {
        format!("Case insensitive exact match of {}", self.s_data)
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// Always Matches
struct YesMatch {}
impl Match for YesMatch {
    fn smatch(&self, _buff: &str) -> bool {
        true
    }
    fn umatch(&self, _buff: &[u8]) -> bool {
        true
    }
    fn show(&self) -> String {
        "Always Matches".to_string()
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// Never Matches
struct NoMatch {}
impl Match for NoMatch {
    fn smatch(&self, _buff: &str) -> bool {
        false
    }
    fn umatch(&self, _buff: &[u8]) -> bool {
        false
    }
    fn show(&self) -> String {
        "Never Matches".to_string()
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// match length
struct LengthMatch {
    min: usize,
    max: Option<usize>,
}
impl LengthMatch {
    /// new
    fn new(data: &str) -> Result<Self> {
        if data.is_empty() {
            return err!("Length spec can't be empty");
        }
        let mut val = Self::default();
        for (n, x) in data.split(',').enumerate() {
            if n == 0 {
                val.min = x.to_usize_whole(data.as_bytes(), "length matcher")?;
            } else if n == 1 {
                val.max = Some(x.to_usize_whole(data.as_bytes(), "length matcher")?);
            } else {
                return err!("Length spec can't have more than one comma.");
            }
        }
        Ok(val)
    }
    /*
        /// new from actual numbers
        fn with_sizes(min : usize, max : Option<usize>) -> Self {
        Self {min, max}
        }
    */
}

impl Match for LengthMatch {
    fn smatch(&self, buff: &str) -> bool {
        let len = buff.chars().count();
        if len < self.min {
            return false;
        }
        if self.max.is_none() {
            return true;
        }
        len <= self.max.unwrap()
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        let len = buff.len();
        if len < self.min {
            return false;
        }
        if self.max.is_none() {
            return true;
        }
        len <= self.max.unwrap()
    }
    fn show(&self) -> String {
        if let Some(max) = self.max {
            format!("String of at least {}, but no more than {}  bytes", self.min, max)
        } else if self.min == 0 {
            "Empty String".to_string()
        } else {
            format!("String of at least {} bytes", self.min)
        }
    }
}

/// Mode for combining parts of a multi-match
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub enum Combiner {
    /// matches if any match
    Or,
    /// matches only if all match
    #[default]
    And,
}
impl fmt::Display for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Or => Ok(write!(f, "OR")?),
            Self::And => Ok(write!(f, "AND")?),
        }
    }
}

// single column (title,foo), whole line (,foo) or column set with determiner
// [this-that],foo or [all,this-that],foo
#[derive(Debug, Default)]
struct ColGroup {
    col: ColumnSet,
    det: Determiner,
    #[allow(dead_code)]
    has_col: bool,
}
impl ColGroup {
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
    fn new_with(spec: &str) -> Result<(Self, &str)> {
        if spec.is_empty() {
            Ok((Self::default(), spec))
        } else if spec.first() == ',' {
            Ok((Self::default(), &spec[1..]))
        } else if spec.first() == '[' {
            let len = util::find_close(spec)?;
            let cols = &spec[1..len];
            let rest = &spec[(len + 1)..];
            Ok((
                Self {
                    col: ColumnSet::from_spec(cols)?,
                    det: Determiner::default(),
                    has_col: true,
                },
                rest,
            ))
        } else if let Some((a, b)) = spec.split_once(',') {
            Ok((
                Self { col: ColumnSet::from_spec(a)?, det: Determiner::default(), has_col: true },
                b,
            ))
        } else {
            Ok((
                Self {
                    col: ColumnSet::from_spec(spec)?,
                    det: Determiner::default(),
                    has_col: true,
                },
                "",
            ))
        }
    }
}

//COLUMN,comp,OPCOLGROUP,PATTERN
// title,comp,<author
// title,comp,<[all,author-date]
// title,comp,LT.author,plain
#[derive(Debug)]
struct CompMatcher {
    target: NamedCol,
    op: CompareOp,
    col: ColGroup,
    comp: Comp,
}
impl CompMatcher {
    fn new(incol: &str, mut pattern: &str) -> Result<Self> {
        let target = NamedCol::new_from(incol)?;
        if pattern.len() < 3 {
            return err!("CompMatcher format is OpColumns,Comparator, '{}'", pattern);
        }
        // FIXME - factor out this CompareOp parsing
        let op1 = pattern[0..2].parse::<CompareOp>();
        let op;
        if let Ok(o) = op1 {
            pattern = &pattern[2..];
            op = o;
        } else {
            let op1 = pattern[0..1].parse::<CompareOp>();
            if let Ok(o) = op1 {
                pattern = &pattern[1..];
                op = o;
            } else if let Some((a, b)) = pattern.split_once('.') {
                op = a.parse::<CompareOp>()?;
                pattern = b;
            } else {
                return err!("CompMatcher format is OpColumns,Comparator, '{}'", pattern);
            }
        }
        let (col, rest) = ColGroup::new_with(pattern)?;
        let comp = CompMaker::make_comp(rest)?;
        Ok(Self { target, op, col, comp })
    }
}
impl LineMatch for CompMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        let mut yes: usize = 0;
        let mut no: usize = 0;
        for x in self.col.col.get_cols() {
            let o = self.comp.comp(line.get(self.target.num), line.get(x.num));
            if self.op.ord_ok(o) {
                yes += 1;
            } else {
                no += 1;
            }
            let t = self.col.det.match_mid(yes, no);
            if t != Tri::Maybe {
                return t == Tri::Yes;
            }
        }
        self.col.det.match_final(yes, no)
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.target.lookup(fieldnames)?;
        self.col.lookup(fieldnames)
    }

    fn show(&self) -> String {
        format!("CompMatcher {:?} {:?} {:?} {:?}", self.target, self.op, self.col, self.comp)
    }
}

/// Does the whole line match a pattern. Implements `LineMatch`
#[derive(Debug)]
struct WholeMatcher {
    matcher: Matcher,
}

impl WholeMatcher {
    fn new(method: &str, pattern: &str) -> Result<Self> {
        Ok(Self { matcher: MatchMaker::make2(method, pattern)? })
    }
}

// LineMatch::matcher could return Result<bool> and then we can put strict back
impl LineMatch for WholeMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        self.matcher.negate
            ^ if self.matcher.string {
                self.matcher.smatch(&String::from_utf8_lossy(line.line_nl()))
            } else {
                self.matcher.umatch(line.line_nl())
            }
    }

    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
    fn show(&self) -> String {
        format!("match whole line against {}", self.matcher)
    }
}

/// Does the line have a certain number of columns
#[derive(Debug)]
struct CountMatcher {
    count: usize,
}

impl CountMatcher {
    fn new(pattern: &str) -> Result<Self> {
        Ok(Self { count: pattern.to_usize_whole(pattern.as_bytes(), "count matcher")? })
    }
}

// LineMatch::matcher could return Result<bool> and then we can put strict back
impl LineMatch for CountMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        line.len() == self.count
    }

    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
    fn show(&self) -> String {
        format!("Does line have {} columns", self.count)
    }
    fn ok_verbose(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        let ret = self.ok(line);
        if !ret {
            eprintln!(
                "Line {} of {} had {} columns where {} were expected.",
                line_num,
                fname,
                line.len(),
                self.count
            );
        }
        ret
    }
}

/// Does a particular column of a line match a pattern. Implements `LineMatch`
#[derive(Debug)]
struct ColSetMatcher {
    matcher: Matcher,
    col: ColumnSet,
    det: Determiner,
}

impl ColSetMatcher {
    /// new from parts
    fn new(cols: &str, method: &str, pattern: &str) -> Result<Self> {
        if let Some((a, b)) = cols.split_once(',') {
            Ok(Self {
                matcher: MatchMaker::make2(method, pattern)?,
                col: ColumnSet::from_spec(b)?,
                det: Determiner::new(a)?,
            })
        } else {
            err!("ColumnGroup format is Determiner,ColumnSet")
        }
    }
}

impl LineMatch for ColSetMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        let mut yes = 0;
        let mut no = 0;
        for x in self.col.get_cols() {
            let did_match = if self.matcher.string {
                self.matcher.smatch(&String::from_utf8_lossy(line.get(x.num)))
            } else {
                self.matcher.umatch(line.get(x.num))
            };
            if did_match {
                yes += 1;
            } else {
                no += 1;
            }
            let res = self.det.match_mid(yes, no);
            if res != Tri::Maybe {
                return self.matcher.negate ^ (res == Tri::Yes);
            }
        }
        self.matcher.negate ^ self.det.match_final(yes, no)
    }

    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
    fn show(&self) -> String {
        format!("match {:?} against {}", self.col, self.matcher)
    }
}

/// Does a particular column of a line match a pattern. Implements `LineMatch`
#[derive(Debug)]
struct ColMatcher {
    matcher: Matcher,
    col: NamedCol,
}

impl ColMatcher {
    /// Column,Spec,Pattern
    /// Pattern may have additional commas
    fn new(cols: &str, method: &str, pattern: &str) -> Result<Self> {
        let mut nc = NamedCol::new();
        nc.parse(cols)?;
        Ok(Self { matcher: MatchMaker::make2(method, pattern)?, col: nc })
    }
}

// LineMatch::matcher could return Result<bool> and then we can put strict back
impl LineMatch for ColMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        self.matcher.negate
            ^ if self.matcher.string {
                self.matcher.smatch(&String::from_utf8_lossy(line.get(self.col.num)))
            } else {
                self.matcher.umatch(line.get(self.col.num))
            }
    }

    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
    fn show(&self) -> String {
        format!("match {} against {}", self.col, self.matcher)
    }
}

/// List of [`LineMatch`], combined with AND or OR
#[derive(Default)]
pub struct LineMatcherList {
    /// the mode
    pub multi: Combiner,
    /// the matchers
    pub matchers: Vec<Box<dyn LineMatch>>,
}
impl fmt::Debug for LineMatcherList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LineMatcherList {}", self.multi)
    }
}

impl LineMatcherList {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self { multi: Combiner::Or, matchers: Vec::new() }
    }
    /// new with combiner
    #[must_use]
    pub fn new_with(multi: Combiner) -> Self {
        Self { multi, matchers: Vec::new() }
    }
    /// add Matcher to list
    pub fn push(&mut self, item: &str) -> Result<()> {
        self.matchers.push(MatchMaker::make_line(item)?);
        Ok(())
    }
    /// is empty list?
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.matchers.is_empty()
    }
    /// ok, with supplied Combiner
    pub fn ok_tagged(&mut self, line: &TextLine, multi: Combiner) -> bool {
        match multi {
            Combiner::And => self.ok_and(line),
            Combiner::Or => self.ok_or(line),
        }
    }
    /// `ok_verbose`, with supplied Combiner
    pub fn ok_verbose_tagged(
        &mut self,
        line: &TextLine,
        multi: Combiner,
        line_num: usize,
        fname: &str,
    ) -> bool {
        match multi {
            Combiner::And => self.ok_verbose_and(line, line_num, fname),
            Combiner::Or => self.ok_verbose_or(line, line_num, fname),
        }
    }
    /// ok, with AND
    pub fn ok_and(&mut self, line: &TextLine) -> bool {
        for x in &mut self.matchers {
            if !x.ok(line) {
                return false;
            }
        }
        true
    }
    /// `ok_verbose`, with AND
    pub fn ok_verbose_and(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        let mut ret = true;
        for x in &mut self.matchers {
            if !x.ok_verbose(line, line_num, fname) {
                ret = false;
            }
        }
        ret
    }
    /// ok, with OR
    pub fn ok_or(&mut self, line: &TextLine) -> bool {
        for x in &mut self.matchers {
            if x.ok(line) {
                return true;
            }
        }
        false
    }
    /// ok, with OR
    pub fn ok_verbose_or(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        for x in &mut self.matchers {
            if x.ok(line) {
                // NOT ok_verbose
                return true;
            }
        }
        // FIXME - some report of each thing?
        eprintln!("Line {line_num} of {fname} failed to match OR matcher");
        false
    }
    /// Is this line ok?
    pub fn ok(&mut self, line: &TextLine) -> bool {
        self.ok_tagged(line, self.multi)
    }
    /// Is this line ok? If not, write explanation to stderr
    pub fn ok_verbose(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        self.ok_verbose_tagged(line, self.multi, line_num, fname)
    }
    /// resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in &mut self.matchers {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
    /// Human readable desription
    fn show(&self) -> String {
        if self.is_empty() {
            format!("Empty {} LineMatcherList", self.multi)
        } else if self.matchers.len() == 1 {
            format!("{}", self.matchers[0])
        } else {
            let mut ret = format!("{}", self.matchers[0]);
            for x in self.matchers.iter().skip(1) {
                write!(ret, "{} {}", self.multi, x).unwrap();
            }
            ret
        }
    }
}

impl LineMatch for LineMatcherList {
    fn ok(&mut self, line: &TextLine) -> bool {
        self.ok(line)
    }
    fn ok_verbose(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        self.ok_verbose(line, line_num, fname)
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.lookup(fieldnames)
    }
    fn show(&self) -> String {
        self.show()
    }
}

/// List of [Matcher], combined with AND or OR
#[derive(Default)]
pub struct MatcherList {
    /// the mode
    pub multi: Combiner,
    /// the matchers
    pub matchers: Vec<Matcher>,
}
impl fmt::Debug for MatcherList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MatcherList {}", self.multi)
    }
}

impl MatcherList {
    /// new `MatcherList` with Or
    #[must_use]
    pub const fn new() -> Self {
        Self { multi: Combiner::Or, matchers: Vec::new() }
    }
    /// new `MatcherList` with given mode
    #[must_use]
    pub const fn new_with(multi: Combiner) -> Self {
        Self { multi, matchers: Vec::new() }
    }
    /// add Matcher to list
    pub fn push(&mut self, item: &str) -> Result<()> {
        self.matchers.push(MatchMaker::make(item)?);
        Ok(())
    }
    /// add Matcher to list
    fn push_obj(&mut self, item: Matcher) {
        self.matchers.push(item);
    }
    /// Are there any matchers in the list?
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.matchers.is_empty()
    }
    fn smatch_tagged(&self, buff: &str, multi: Combiner) -> bool {
        match multi {
            Combiner::And => self.smatch_and(buff),
            Combiner::Or => self.smatch_or(buff),
        }
    }
    fn smatch_and(&self, buff: &str) -> bool {
        for x in &self.matchers {
            if !x.smatch(buff) {
                return false;
            }
        }
        true
    }
    fn smatch_or(&self, buff: &str) -> bool {
        for x in &self.matchers {
            if x.smatch(buff) {
                return true;
            }
        }
        false
    }
    fn umatch_tagged(&self, buff: &[u8], multi: Combiner) -> bool {
        match multi {
            Combiner::And => self.umatch_and(buff),
            Combiner::Or => self.umatch_or(buff),
        }
    }
    fn umatch_and(&self, buff: &[u8]) -> bool {
        for x in &self.matchers {
            if !x.umatch(buff) {
                return false;
            }
        }
        true
    }
    fn umatch_or(&self, buff: &[u8]) -> bool {
        for x in &self.matchers {
            if x.umatch(buff) {
                return true;
            }
        }
        false
    }
    /// Human readable desription
    #[must_use]
    pub fn show(&self) -> String {
        if self.is_empty() {
            format!("Empty {} MatcherList", self.multi)
        } else if self.matchers.len() == 1 {
            format!("{}", self.matchers[0])
        } else {
            let mut ret = format!("{}", self.matchers[0]);
            for x in self.matchers.iter().skip(1) {
                write!(ret, "{} {}", self.multi, x).unwrap();
            }
            ret
        }
    }
    /// Are these characters ok?
    #[must_use]
    pub fn smatch(&self, buff: &str) -> bool {
        self.smatch_tagged(buff, self.multi)
    }
    /// Are these bytes ok?
    #[must_use]
    pub fn umatch(&self, buff: &[u8]) -> bool {
        self.umatch_tagged(buff, self.multi)
    }
    /// smatch, but print to stderr if fail
    #[must_use]
    pub fn verbose_smatch(&self, buff: &str, negate: bool) -> bool {
        let res = self.smatch(buff);
        if res != negate {
            eprintln!("Failed to {}match {} against {}", not_str(negate), buff, self.show());
        }
        res
    }
}

impl Match for MatcherList {
    fn smatch(&self, buff: &str) -> bool {
        self.smatch(buff)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.umatch(buff)
    }
    fn show(&self) -> String {
        self.show()
    }
    fn verbose_smatch(&self, buff: &str, negate: bool) -> bool {
        self.verbose_smatch(buff, negate)
    }
}

/// Match a pattern against a target, i.e. a Match with some context.
#[derive(Debug)]
#[allow(clippy::struct_field_names)]
#[allow(clippy::struct_excessive_bools)]
pub struct Matcher {
    /// general type, e.g. "regex"
    ctype: String,
    /// true for unicode and &str, false for bytes an &[u8]
    string: bool,
    // u8 to unicode error, true for error, false for lossy
    // strict : bool,
    /// should match be case sensitive
    case: Case,
    /// true to invert the match
    negate: bool,
    /// true to remove leading and trailing whitespace before matching
    trim: bool,
    /// true if an empty buffer should match too
    empty: bool,
    /// for multi-matches, combine with AND or OR
    multi_mode: Option<Combiner>,
    /// the matching object
    matcher: Box<dyn Match>,
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.trim {
            write!(f, "trimmed ")?;
        }
        if self.negate {
            write!(f, "negated ")?;
        }
        if self.empty {
            write!(f, "empty-accepting  ")?;
        }
        write!(f, "{}", self.matcher)
    }
}

impl Default for Matcher {
    fn default() -> Self {
        Self {
            ctype: "regex".to_string(),
            string: false,
            case: Case::Sens,
            negate: false,
            trim: false,
            empty: false,
            multi_mode: None,
            matcher: Box::new(YesMatch {}),
        }
    }
}

impl Clone for Matcher {
    fn clone(&self) -> Self {
        Self {
            ctype: self.ctype.clone(),
            string: self.string,
            case: self.case,
            negate: self.negate,
            trim: self.trim,
            empty: self.empty,
            multi_mode: self.multi_mode,
            matcher: Box::new(YesMatch {}),
        }
    }
}

impl Matcher {
    /// Are these characters ok?
    #[must_use]
    pub fn smatch(&self, mut buff: &str) -> bool {
        if self.trim {
            buff = buff.trimw();
        }
        if self.empty && buff.is_empty() { true } else { self.matcher.smatch(buff) }
    }
    /// Are these bytes ok?
    #[must_use]
    pub fn umatch(&self, mut buff: &[u8]) -> bool {
        if self.trim {
            buff = buff.trimw();
        }
        if self.empty && buff.is_empty() { true } else { self.matcher.umatch(buff) }
    }
    /// umatch or smatch, depending on self.string
    pub fn do_match(&self, buff: &[u8]) -> Result<bool> {
        if self.string {
            Ok(self.smatch(std::str::from_utf8(buff)?))
        } else {
            Ok(self.umatch(buff))
        }
    }

    /// umatch or smatch, depending on self.string ;
    /// Failure to convert to utf8 is simply 'does not match'
    #[must_use]
    pub fn do_match_safe(&self, buff: &[u8]) -> bool {
        if self.string {
            if let Ok(x) = std::str::from_utf8(buff) { self.smatch(x) } else { false }
        } else {
            self.umatch(buff)
        }
    }
}

type MakerBox = Box<dyn Fn(&mut Matcher, &str) -> Result<Box<dyn Match>> + Send>;
/// A named constructor for a [Match], used by [`MatchMaker`]
struct MatchMakerItem {
    /// matched against `Matcher::ctype`
    tag: &'static str,
    /// what this matcher does
    help: &'static str,
    /// Create a dyn Match from a pattern
    maker: MakerBox,
}

struct MatchMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

static MATCH_MAKER: Mutex<Vec<MatchMakerItem>> = Mutex::new(Vec::new());
static MATCH_ALIAS: Mutex<Vec<MatchMakerAlias>> = Mutex::new(Vec::new());
const MODIFIERS: &[&str] = &["utf8", "not", "trim", "null", "case", "and", "or"];

/// Makes a [Matcher]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
pub struct MatchMaker {}

impl MatchMaker {
    /// add standard match makers
    fn init() -> Result<()> {
        if !MATCH_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("infix", "substr")?;
        Self::do_add_alias("infix", "substring")?;
        Self::do_add_alias("file-exact", "fileexact")?;
        Self::do_push("prefix", "Is the pattern a prefix of the target?", |m, p| {
            Ok(if m.case == Case::Insens {
                Box::new(PrefixMatchC::new(p))
            } else {
                Box::new(PrefixMatch::new(p))
            })
        })?;
        Self::do_push("solve", "Does the target match the given solver pattern?", |_m, p| {
            Ok(Box::new(crate::solve::SolverMatch::new(p)?))
        })?;
        Self::do_push("float", "Valid floating point number", |_m, _p| {
            Ok(Box::new(RegexMatch::new("^[-]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", Case::Sens)?))
        })?;
        Self::do_push("integer", "Valid integer", |_m, _p| {
            Ok(Box::new(RegexMatch::new("^[-]?[0-9]+$", Case::Sens)?))
        })?;
        Self::do_push(
            "number",
            "Valid number, with optional decimal, e.g. 42 or 1.23",
            |_m, _p| Ok(Box::new(RegexMatch::new("^[-]?[0-9]+(\\.[0-9]+)?$", Case::Sens)?)),
        )?;
        Self::do_push("ip", "Valid IPv4 address", |_m, _p| {
            Ok(Box::new(RegexMatch::new(
                "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$",
                Case::Sens,
            )?))
        })?;
        Self::do_push(
            "regex",
            "Interpret the pattern as a regex, as per the eponymous crate",
            |m, p| Ok(Box::new(RegexMatch::new(p, m.case)?)),
        )?;
        Self::do_push("range", "Match using a CompareOp and a Comparator", |_m, p| {
            Ok(Box::new(CheckBuff::new(p)?))
        })?;
        Self::do_push(
            "file-exact",
            "Is the target exactly one one the lines in this file?",
            |m, p| {
                Ok(if m.case == Case::Insens {
                    Box::new(FileExactMatchC::new(p, m.string)?)
                } else {
                    Box::new(FileExactMatch::new(p)?)
                })
            },
        )?;
        Self::do_push("exact", "Does the target exactly match the pattern", |m, p| {
            Ok(if m.case == Case::Insens {
                Box::new(ExactMatchC::new(p))
            } else {
                Box::new(ExactMatch::new(p))
            })
        })?;
        Self::do_push(
            "length",
            "For the pattern X,Y, length is between X and Y inclusive",
            |_m, p| Ok(Box::new(LengthMatch::new(p)?)),
        )?;
        Self::do_push("yes", "always matches", |_m, _p| Ok(Box::new(YesMatch {})))?;
        Self::do_push("suffix", "Is the pattern a suffix of the target?", |m, p| {
            Ok(if m.case == Case::Insens {
                Box::new(SuffixMatchC::new(p))
            } else {
                Box::new(SuffixMatch::new(p))
            })
        })?;
        Self::do_push("infix", "Is the pattern a substring of the target?", |m, p| {
            Ok(if m.case == Case::Insens {
                Box::new(InfixMatchC::new(p))
            } else {
                Box::new(InfixMatch::new(p))
            })
        })?;
        Self::do_push("glob", "Treat the pattern as a shell glob", |m, p| {
            Ok(Box::new(GlobMatch::new(p, m.case)))
        })?;
        Self::do_push("empty", "Is the target empty, i.e. zero bytes", |_m, _p| {
            Ok(Box::new(LengthMatch::new("0,0")?))
        })?;
        Self::do_push("blank", "Is the target made entirely of white space", |m, _p| {
            m.trim = true;
            m.empty = true;
            Ok(Box::new(NoMatch {}))
        })?;
        Self::do_push("comment", "Is the target white space followed by the pattern?", |m, p| {
            m.trim = true;
            m.empty = true;
            Ok(Box::new(PrefixMatch::new(p)))
        })?;
        Self::do_push("hash", "Is the target white space folled by the # character?", |m, _p| {
            m.trim = true;
            m.empty = true;
            Ok(Box::new(PrefixMatch::new("#")))
        })?;
        Self::do_push("slash", "Is the target white space followed by //", |m, _p| {
            m.trim = true;
            m.empty = true;
            Ok(Box::new(PrefixMatch::new("//")))
        })
    }
    /// Add a new matcher. If a Matcher already exists by that name, replace it.
    pub fn push<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&mut Matcher, &str) -> Result<Box<dyn Match>> + Send + 'static,
    {
        Self::init()?;
        Self::do_push(tag, help, maker)
    }
    /// Add a new alias. If an alias already exists by that name, replace it.
    pub fn add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        Self::init()?;
        Self::do_add_alias(old_name, new_name)
    }
    /// Return name, replaced by its alias, if any.
    fn resolve_alias(name: String) -> String {
        for x in MATCH_ALIAS.lock().unwrap().iter_mut() {
            if x.new_name == name {
                return x.old_name.to_string();
            }
        }
        name
    }
    fn do_add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        if MODIFIERS.contains(&new_name) {
            return err!(
                "You can't add an alias named {new_name} because that is reserved for a modifier"
            );
        }
        let m = MatchMakerAlias { old_name, new_name };
        let mut mm = MATCH_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == m.new_name {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        drop(mm);
        Ok(())
    }
    fn do_push<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&mut Matcher, &str) -> Result<Box<dyn Match>> + Send + 'static,
    {
        if MODIFIERS.contains(&tag) {
            return err!(
                "You can't add a matcher named {tag} because that is reserved for a modifier"
            );
        }
        let m = MatchMakerItem { tag, help, maker: Box::new(maker) };
        let mut mm = MATCH_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        drop(mm);
        Ok(())
    }
    /// Print all available Matchers to stdout.
    pub fn help() {
        println!("Modifers :");
        println!("utf8  Operations are on utf8 strings, rather than the default u8 bytes.");
        println!("not   Treat a match as a non-match, and vice versa.");
        println!("case  Ignore case. Exact behavior depends on 'utf8' setting.");
        println!("trim  Remove leading and trailing whitespce before checking.");
        println!("null  An empty string also matches. This check happens after trimming.");
        println!("and   Interpret pattern as a multi-pattern, Match with AND.");
        println!("or    Interpret pattern as a multi-pattern, Match with OR.\n");
        println!("Methods :");
        Self::init().unwrap();
        let mut results = Vec::new();
        for x in &*MATCH_MAKER.lock().unwrap() {
            results.push(format!("{:12}{}", x.tag, x.help));
        }
        results.sort();
        for x in results {
            println!("{x}");
        }
        println!("See also https://avjewe.github.io/cdxdoc/Matcher.html.");
    }
    /// Create a Matcher from a matcher spec and a pattern
    pub fn make2(matcher: &str, pattern: &str) -> Result<Matcher> {
        Self::init()?;
        let mut m = Matcher::default();
        if !matcher.is_empty() {
            for x in matcher.split('.') {
                if x.eq_ignore_ascii_case("utf8") {
                    m.string = true;
                } else if x.eq_ignore_ascii_case("not") {
                    m.negate = true;
                } else if x.eq_ignore_ascii_case("trim") {
                    m.trim = true;
                } else if x.eq_ignore_ascii_case("null") {
                    m.negate = true;
                } else if x.eq_ignore_ascii_case("case") {
                    m.case = Case::Insens;
                } else if x.eq_ignore_ascii_case("and") {
                    m.multi_mode = Some(Combiner::And);
                } else if x.eq_ignore_ascii_case("or") {
                    m.multi_mode = Some(Combiner::Or);
                } else {
                    m.ctype = x.to_string();
                }
            }
            m.ctype = Self::resolve_alias(m.ctype);
        }
        if let Some(mm) = m.multi_mode {
            let mut outer = Box::new(MatcherList::new_with(mm));
            let mut pattern = pattern;
            let delim = pattern.first();
            pattern = pattern.skip_first();
            for x in pattern.split(delim) {
                let mut m2 = m.clone();
                m2.multi_mode = None; // not really necessary
                Self::remake(&mut m2, x)?;
                outer.push_obj(m2);
            }
            m.matcher = outer;
        } else {
            Self::remake(&mut m, pattern)?;
        }
        Ok(m)
    }
    /// Create a matcher from a full spec, i.e. "Matcher,Pattern"
    pub fn make(spec: &str) -> Result<Matcher> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::make2(a, b)
        } else {
            Self::make2(spec, "")
        }
    }
    /// Create a matcher from a full spec, i.e. "Matcher,Pattern"
    pub fn make_line3(cols: &str, method: &str, pattern: &str) -> Result<Box<dyn LineMatch>> {
        if method == "expr" {
            if cols.is_empty() {
                Ok(Box::new(ExprMatcher::new(pattern)?))
            } else {
                err!("'expr' matcher spec only works on whole lines")
            }
        } else if method == "comp" {
            Ok(Box::new(CompMatcher::new(cols, pattern)?))
        } else if method == "count" {
            if cols.is_empty() {
                Ok(Box::new(CountMatcher::new(pattern)?))
            } else {
                err!("'count' matcher spec only works on whole lines")
            }
        } else if cols.is_empty() {
            Ok(Box::new(WholeMatcher::new(method, pattern)?))
        } else if !cols.is_empty() && cols.first() == '[' {
            Ok(Box::new(ColSetMatcher::new(&cols[1..cols.len() - 1], method, pattern)?))
        } else {
            Ok(Box::new(ColMatcher::new(cols, method, pattern)?))
        }
    }
    /// Create a matcher from a full spec, i.e. "Matcher,Pattern"
    pub fn make_line(spec: &str) -> Result<Box<dyn LineMatch>> {
        if !spec.is_empty() && spec.first() == '[' {
            let len = util::find_close(spec)?;
            let cols = &spec[1..len];
            let rest = &spec[(len + 1)..];
            if let Some((c, d)) = rest.split_once(',') {
                Self::make_line3(cols, c, d)
            } else {
                Self::make_line3(cols, rest, "")
            }
        } else if let Some((a, b)) = spec.split_once(',') {
            if let Some((c, d)) = b.split_once(',') {
                Self::make_line3(a, c, d)
            } else {
                Self::make_line3(a, b, "")
            }
        } else {
            Self::make_line3(spec, "", "")
        }
    }
    /// Remake the dyn Match based on current contents.
    pub fn remake(m: &mut Matcher, pattern: &str) -> Result<()> {
        Self::init()?;
        for x in &*MATCH_MAKER.lock().unwrap() {
            if x.tag == m.ctype {
                m.matcher = (x.maker)(m, pattern)?;
                return Ok(());
            }
        }
        err!("Unknown matcher : {}", m.ctype)
    }
}

/// macth if arithmetic expression is non-zero
#[derive(Debug)]
struct ExprMatcher {
    con: Expr,
}

impl ExprMatcher {
    /// new from expression
    fn new(ex: &str) -> Result<Self> {
        Ok(Self { con: Expr::new(ex)? })
    }
}

impl LineMatch for ExprMatcher {
    fn ok(&mut self, line: &TextLine) -> bool {
        self.con.eval(line) != 0.0
    }
    fn ok_verbose(&mut self, line: &TextLine, line_num: usize, fname: &str) -> bool {
        if self.con.eval(line) == 0.0 {
            eprintln!(
                "For line {} of {} the value of {} was zero",
                line_num,
                fname,
                self.con.expr()
            );
            false
        } else {
            true
        }
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.con.lookup(fieldnames)
    }

    fn show(&self) -> String {
        format!("Floating Point Expression must be non-zero : {}", self.con.expr())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn range() -> Result<()> {
        let c = MatchMaker::make("range,plain,<=dog>cat")?;
        assert!(c.smatch("ccc"));
        assert!(!c.smatch("cat"));
        assert!(c.smatch("dog"));
        assert!(!c.smatch("doh"));
        assert!(c.smatch("dof"));
        let c = MatchMaker::make("range,<dog>=cat")?;
        assert!(c.smatch("cat"));
        let c = MatchMaker::make("range,!=cat")?;
        assert!(!c.smatch("cat"));
        assert!(c.smatch("dog"));
        let c = MatchMaker::make("range,GT,cat,LE,dog")?;
        assert!(!c.smatch("cat"));
        assert!(c.smatch("dog"));
        let c = MatchMaker::make("range,LE,dog")?;
        assert!(c.smatch("cat"));
        assert!(c.smatch("dog"));
        assert!(!c.smatch("doh"));
        let c = MatchMaker::make("range,lower,LE,dog")?;
        assert!(c.smatch("Cat"));
        assert!(c.smatch("Dog"));
        assert!(!c.smatch("Doh"));
        Ok(())
    }
}
