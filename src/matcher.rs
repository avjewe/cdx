//! Test if a line matches some criterea

use crate::column::NamedCol;
use crate::comp::skip_leading_white_str;
use crate::text::{Case, Text};
use crate::{err, Error, Reader, Result, TextLine};
use memchr::memmem::find;
use std::collections::HashSet;
use std::fmt;

/// Match against whole line
pub trait LineCheck {
    /// is the line ok?
    fn check(&self, line: &TextLine) -> bool;
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()>;
}

/// Match against buffer
pub trait BufCheck {
    /// is the buffer ok?
    fn scheck(&self, buff: &str) -> bool;
    /// is the line ok?
    fn ucheck(&self, buff: &[u8]) -> bool;
    /// ucheck or scheck
    fn check(&self, buff: &[u8], use_str: bool) -> Result<bool> {
        if use_str {
            Ok(self.scheck(std::str::from_utf8(buff)?))
        } else {
            Ok(self.ucheck(buff))
        }
    }
}

/// types of BufCheck
#[derive(Debug, Copy, Clone)]
pub enum CheckType {
    /// as per regex crate
    Regex,
    /// exact match
    Exact,
    /// starts_with
    Prefix,
    /// ends_with
    Suffix,
    /// contains
    Substring,
    /// exactly match one of the lines in this file
    FileExact,
    /// shell-style with * and ?
    Glob,
    /// if pattern is one number, a string a least that long
    /// if two numbers, length between the two numbers, inclusive
    Length,
    /// length zero
    Empty,
    /// all whitespace
    Blank,
    /// all whitespace, up to a #
    HashCmt,
    /// all whitespace, up to a //
    SlashCmt,
    /// all whitespace, up to a literal pattern
    Comment,
    // XmlAttr
    // DelimSuffix
    // DelimPrefix
    // DelimInfix
    // Json
}
impl Default for CheckType {
    fn default() -> Self {
        Self::Regex
    }
}

/// pattern is prefix of string
#[derive(Debug, Clone)]
struct PrefixCheck {
    data: String,
}
impl PrefixCheck {
    fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
        }
    }
}
impl BufCheck for PrefixCheck {
    fn scheck(&self, buff: &str) -> bool {
        buff.starts_with(&self.data)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        buff.starts_with(self.data.as_bytes())
    }
}

#[derive(Debug, Clone, Copy)]
/// check all whitespace
struct BlankCheck;

fn is_blank(data: &[u8]) -> bool {
    for x in data {
        if *x > b' ' {
            return false;
        }
    }
    true
}

impl BufCheck for BlankCheck {
    fn scheck(&self, buff: &str) -> bool {
        is_blank(buff.as_bytes()) // FIXME unicode blanks?
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        is_blank(buff)
    }
}

#[derive(Debug, Clone)]
/// check all whitespace, followed by literal
struct CmtCheck {
    tag: String,
}
impl CmtCheck {
    fn new(tag: &str) -> Self {
        Self {
            tag: tag.to_string(),
        }
    }
    /*
        Replaced by Text::is_cmt, which might be slower
        fn is_ucmt(&self, data: &[u8]) -> bool {
            let mut pos: usize = 0;
            let mut found = false;
        let tag : &[u8] = self.tag.as_bytes();
            for x in data {
                if found {
                    if pos >= tag.len() {
                        return true;
                    }
                    if tag[pos] != *x {
                        return false;
                    }
                    pos += 1;
                } else if *x > b' ' {
                    if tag.is_empty() {
                        return false;
                    }
                    found = true;
                    if tag[0] != *x {
                        return false;
                    }
                    pos = 1;
                }
            }
            true
        }

        fn is_scmt(&self, data: &str) -> bool {
            let mut found = false;
        let mut tag : &str = &self.tag;
            for x in data.chars() {
                if found {
            if tag.is_empty() {
                        return true;
                    }
                    if tag.first() != x {
                        return false;
                    }
            tag = tag.skip_first();
                } else if x > ' ' && !x.is_whitespace() {
                    if tag.is_empty() {
                        return false;
                    }
                    found = true;
                    if tag.first() != x {
                        return false;
                    }
            tag = tag.skip_first();
                }
            }
            true
        }
    */
}

impl BufCheck for CmtCheck {
    fn scheck(&self, buff: &str) -> bool {
        buff.is_cmt(&self.tag)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        buff.is_cmt(self.tag.as_bytes())
    }
}

fn str_to_lower(data: &str, unicode: bool) -> String {
    if unicode {
        data.to_lowercase()
    } else {
        String::from_utf8(data.as_bytes().to_ascii_lowercase()).unwrap()
    }
}

#[derive(Debug, Clone)]
/// pattern is prefix of string, case insensitive
struct PrefixCheckC {
    data: String,
}
impl PrefixCheckC {
    fn new(data: &str, unicode: bool) -> Self {
        Self {
            data: str_to_lower(data, unicode),
        }
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
    let mut haystack_it = haystack
        .chars()
        .flat_map(char::to_lowercase)
        .skip(haystack_len - needle_len);

    // at this monent, needle_it.chars().count() == haystack_it.chars().count()
    for _ in 0..needle_len {
        if needle_it.next().unwrap() != haystack_it.next().unwrap() {
            return false;
        }
    }
    true
}

impl BufCheck for PrefixCheckC {
    fn scheck(&self, buff: &str) -> bool {
        scase_prefix(buff, &self.data)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        bcase_prefix(buff, self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string
struct SuffixCheck {
    data: String,
}
impl SuffixCheck {
    fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
        }
    }
}
impl BufCheck for SuffixCheck {
    fn scheck(&self, buff: &str) -> bool {
        buff.ends_with(&self.data)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        buff.ends_with(self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string, case insensitive
struct SuffixCheckC {
    data: String,
}
impl SuffixCheckC {
    fn new(data: &str, unicode: bool) -> Self {
        Self {
            data: str_to_lower(data, unicode),
        }
    }
}
impl BufCheck for SuffixCheckC {
    fn scheck(&self, buff: &str) -> bool {
        scase_suffix(buff, &self.data)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        bcase_suffix(buff, self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string, case insensitive
struct InfixCheckC {
    data: String,
}
impl InfixCheckC {
    fn new(data: &str, unicode: bool) -> Self {
        Self {
            data: str_to_lower(data, unicode),
        }
    }
}
impl BufCheck for InfixCheckC {
    fn scheck(&self, buff: &str) -> bool {
        buff.to_lowercase().contains(&self.data) // PERF allocation
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        find(&buff.to_ascii_lowercase(), self.data.as_bytes()).is_some() // PERF allocation
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string
struct InfixCheck {
    needle: Vec<u8>,
}
impl InfixCheck {
    fn new(data: &str) -> Self {
        Self {
            needle: data.as_bytes().to_vec(),
        }
    }
}
impl BufCheck for InfixCheck {
    fn scheck(&self, haystack: &str) -> bool {
        find(haystack.as_bytes(), &self.needle).is_some()
    }
    fn ucheck(&self, haystack: &[u8]) -> bool {
        find(haystack, &self.needle).is_some()
    }
}

#[derive(Debug, Clone)]
/// bytes regex
struct RegexCheckB {
    data: regex::bytes::Regex,
}
impl RegexCheckB {
    fn new(data: &str) -> Result<Self> {
        Ok(Self {
            data: regex::bytes::Regex::new(data)?,
        })
    }
}
impl BufCheck for RegexCheckB {
    fn scheck(&self, _buff: &str) -> bool {
        unimplemented!()
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        self.data.is_match(buff)
    }
}

#[derive(Debug, Clone)]
/// utf8 regex
struct RegexCheckS {
    data: regex::Regex,
}
impl RegexCheckS {
    fn new(data: &str) -> Result<Self> {
        Ok(Self {
            data: regex::Regex::new(data)?,
        })
    }
}
impl BufCheck for RegexCheckS {
    fn scheck(&self, buff: &str) -> bool {
        self.data.is_match(buff)
    }
    fn ucheck(&self, _buff: &[u8]) -> bool {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern
struct ExactCheck {
    data: Vec<u8>,
}
impl ExactCheck {
    fn new(data: &str) -> Self {
        Self {
            data: data.as_bytes().to_vec(),
        }
    }
}
impl BufCheck for ExactCheck {
    fn scheck(&self, buff: &str) -> bool {
        self.data == buff.as_bytes()
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        self.data == buff
    }
}

fn load_hashset(data: &mut HashSet<Vec<u8>>, fname: &str) -> Result<()> {
    let mut f = Reader::new();
    f.do_split = false;
    f.open(fname)?;
    if f.is_done() {
        return Ok(());
    }
    loop {
        let line = &f.curr().line;
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
struct FileExactCheck {
    data: HashSet<Vec<u8>>,
}
impl FileExactCheck {
    fn new(data: &str) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset(&mut d, data)?;
        Ok(Self { data: d })
    }
}
impl BufCheck for FileExactCheck {
    fn scheck(&self, buff: &str) -> bool {
        self.data.contains(buff.as_bytes())
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        self.data.contains(buff)
    }
}

fn load_hashset_c(data: &mut HashSet<Vec<u8>>, fname: &str, unicode: bool) -> Result<()> {
    let mut f = Reader::new();
    f.do_split = false;
    f.open(fname)?;
    if f.is_done() {
        return Ok(());
    }
    loop {
        let mut line: &[u8] = &f.curr().line;
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
struct FileExactCheckC {
    data: HashSet<Vec<u8>>,
}
impl FileExactCheckC {
    fn new(data: &str, unicode: bool) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset_c(&mut d, data, unicode)?;
        Ok(Self { data: d })
    }
}
impl BufCheck for FileExactCheckC {
    fn scheck(&self, buff: &str) -> bool {
        self.data.contains(&buff.new_lower().into_bytes()) // PERF allocation
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        self.data.contains(&buff.new_lower()) // PERF allocation
    }
}

#[derive(Debug, Clone)]
/// shell style wildcard match
struct GlobCheck {
    data: String,
    ic: Case,
}
impl GlobCheck {
    fn new(data: &str, ic: Case) -> Self {
        Self {
            data: data.to_string(),
            ic,
        }
    }
}
impl BufCheck for GlobCheck {
    fn scheck(&self, buff: &str) -> bool {
        buff.glob(&self.data, self.ic)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        buff.glob(self.data.as_bytes(), self.ic)
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern, case insensitive
struct ExactCheckC {
    data: String,
}

impl ExactCheckC {
    fn new(data: &str, unicode: bool) -> Result<Self> {
        Ok(Self {
            data: if unicode {
                data.new_lower()
            } else {
                String::from_utf8(data.as_bytes().new_lower())?
            },
        })
    }
}
impl BufCheck for ExactCheckC {
    fn scheck(&self, buff: &str) -> bool {
        buff.equal_insens_quick(&self.data)
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        buff.equal_insens_quick(self.data.as_bytes())
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// check length
struct LengthCheck {
    min: usize,
    max: Option<usize>,
}
impl LengthCheck {
    /// new
    fn new(data: &str) -> Result<Self> {
        if data.is_empty() {
            return err!("Length spec can't be empty");
        }
        let mut val = Self::default();
        for (n, x) in data.split(',').enumerate() {
            if n == 0 {
                val.min = x.parse::<usize>()?;
            } else if n == 1 {
                val.max = Some(x.parse::<usize>()?);
            } else {
                return err!("Length spec can't have more than one comma.");
            }
        }
        Ok(val)
    }
    /*
        /// new from actual numbers
        pub fn with_sizes(min : usize, max : Option<usize>) -> Self {
        Self {min, max}
        }
    */
}

impl BufCheck for LengthCheck {
    fn scheck(&self, buff: &str) -> bool {
        let len = buff.chars().count();
        if len < self.min {
            return false;
        }
        if self.max.is_none() {
            return true;
        }
        len <= self.max.unwrap()
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        let len = buff.len();
        if len < self.min {
            return false;
        }
        if self.max.is_none() {
            return true;
        }
        len <= self.max.unwrap()
    }
}

/// mode for combining parts of a multi-match
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MultiMode {
    /// Regular non-multi mode
    Single,
    /// matches if any match
    Or,
    /// matches only if all match
    And,
}
impl Default for MultiMode {
    fn default() -> Self {
        Self::Single
    }
}
impl MultiMode {
    /// is this multi mode
    pub fn is_multi(&self) -> bool {
        *self != Self::Single
    }
}

/// return a new BufCheck based on spec
pub fn make_match(spec: &str) -> Result<Box<dyn BufCheck>> {
    let (_, matcher) = make_match2(spec)?;
    Ok(matcher)
}

/// return a new BufCheck based on spec
pub fn make_match2(spec: &str) -> Result<(CheckSpec, Box<dyn BufCheck>)> {
    let spec = skip_leading_white_str(spec);
    if spec.is_empty() {
        return err!("Can't make a match from an empty string");
    }
    let (s, mut p) = if let Some((x, y)) = spec.split_once(',') {
        (x, y)
    } else {
        (spec, "")
    };
    let mspec = CheckSpec::new(s)?;
    let b: Box<dyn BufCheck> = if mspec.is_multi() {
        if p.is_empty() {
            return err!("Can't make a multi pattern from an empty string");
        }
        let delim = p.first();
        if delim.is_alphanumeric() {
            return err!("First character of a Multi-Pattern is the delimiter, which must not be alphanumeric. {}", p);
        }
        p = p.skip_first();
        if mspec.multi_mode == MultiMode::And {
            let mut outer = Box::new(CheckAnd::new());
            for x in p.split(delim) {
                outer.push(mspec.make_box(x)?);
            }
            outer
        } else {
            let mut outer = Box::new(CheckOr::new());
            for x in p.split(delim) {
                outer.push(mspec.make_box(x)?);
            }
            outer
        }
    } else {
        mspec.make_box(p)?
    };
    Ok((mspec, b))
    /*
        if !spec.first().is_alphanumeric() {
        make_multi_match(spec)
        }
    */
}

/// Spec for BufCheck
#[derive(Debug, Clone, Default)]
pub struct CheckSpec {
    /// general type
    ctype: CheckType,
    /// true for unicode and &str, false for bytes an &[u8]
    string: bool,
    // u8 to unicode error, true for error, false for lossy
    // strict : bool,
    /// should match be case sensitive
    case: Case,
    /// true to invert the match
    negate: bool,
    /// for multi-matches, combine with AND or OR
    multi_mode: MultiMode,
}

impl CheckSpec {
    /// is this a multi-match
    pub fn is_multi(&self) -> bool {
        self.multi_mode.is_multi()
    }
    /// period delimited parts, which can be
    /// S for string vs default of bytes
    /// N for negate vs default of normal
    /// C for case-insensitive vs default of case-sensitive
    // F for fail-on-utf8-error vs default of lossy
    /// type which is one of : regex, exact, prefix, suffix, substring, fileexact, glob, length. Default is regex.
    pub fn new(spec: &str) -> Result<Self> {
        let mut c = Self::default();
        for x in spec.split('.') {
            if x.eq_ignore_ascii_case("") {
            } else if x.eq_ignore_ascii_case("S") {
                c.string = true;
            } else if x.eq_ignore_ascii_case("N") {
                c.negate = true;
            } else if x.eq_ignore_ascii_case("C") {
                c.case = Case::Insens;
            } else if x.eq_ignore_ascii_case("and") {
                c.multi_mode = MultiMode::And;
            } else if x.eq_ignore_ascii_case("or") {
                c.multi_mode = MultiMode::Or;
            } else if x.eq_ignore_ascii_case("regex") {
                c.ctype = CheckType::Regex;
            } else if x.eq_ignore_ascii_case("exact") {
                c.ctype = CheckType::Exact;
            } else if x.eq_ignore_ascii_case("prefix") {
                c.ctype = CheckType::Prefix;
            } else if x.eq_ignore_ascii_case("suffix") {
                c.ctype = CheckType::Suffix;
            } else if x.eq_ignore_ascii_case("substr")
                || x.eq_ignore_ascii_case("substring")
                || x.eq_ignore_ascii_case("infix")
            {
                c.ctype = CheckType::Substring;
            } else if x.eq_ignore_ascii_case("file-exact") || x.eq_ignore_ascii_case("fileexact") {
                c.ctype = CheckType::FileExact;
            } else if x.eq_ignore_ascii_case("glob") {
                c.ctype = CheckType::Glob;
            } else if x.eq_ignore_ascii_case("length") {
                c.ctype = CheckType::Length;
            } else if x.eq_ignore_ascii_case("blank") {
                c.ctype = CheckType::Blank;
            } else if x.eq_ignore_ascii_case("empty") {
                c.ctype = CheckType::Empty;
            } else if x.eq_ignore_ascii_case("hash") {
                c.ctype = CheckType::HashCmt;
            } else if x.eq_ignore_ascii_case("slash") {
                c.ctype = CheckType::SlashCmt;
            } else if x.eq_ignore_ascii_case("comment") {
                c.ctype = CheckType::Comment;
            } else {
                return err!(
                    "Invalid Checker Item {}. Should be period delimited list :\n\
			     optionally including some of these :\n\
			     'S' for string vs default of bytes\n\
			     'N' for negate vs default of normal\n\
			     'C' for case-insensitive vs default of case-sensitive\n\
			     plus optionally one of these, to override the default 'regex'\n\
			     'regex' field must match pattern as per the regex crate\n\
			     'exact' field must match pattern exactly\n\
			     'prefix' pattern must be a prefix of the field\n\
			     'suffix' pattern must be a suffix of the field\n\
			     'infix' pattern must be a substring of the field\n\
			     'file-exact' field must exactly match one of the lines in this file\n\
			     'glob' field must match pattern interprested as shell glob, i.e. with * and ?\n\
			     'length' if a single number, field must be at least that long
         if two comma delimited number, length must be between the two lengths, inclusive",
                    x
                );
            }
        }
        Ok(c)
    }
    /// Create the appropriate dyn BufCheck from the spec
    pub fn make_box(&self, pattern: &str) -> Result<Box<dyn BufCheck>> {
        Ok(match self.ctype {
            CheckType::Regex => {
                if self.string {
                    Box::new(RegexCheckS::new(pattern)?)
                } else {
                    Box::new(RegexCheckB::new(pattern)?)
                }
            }
            CheckType::FileExact => {
                if self.case == Case::Insens {
                    Box::new(FileExactCheckC::new(pattern, self.string)?)
                } else {
                    Box::new(FileExactCheck::new(pattern)?)
                }
            }
            CheckType::Exact => {
                if self.case == Case::Insens {
                    Box::new(ExactCheckC::new(pattern, self.string)?)
                } else {
                    Box::new(ExactCheck::new(pattern))
                }
            }
            CheckType::Length => Box::new(LengthCheck::new(pattern)?),
            CheckType::Prefix => {
                if self.case == Case::Insens {
                    Box::new(PrefixCheckC::new(pattern, self.string))
                } else {
                    Box::new(PrefixCheck::new(pattern))
                }
            }
            CheckType::Suffix => {
                if self.case == Case::Insens {
                    Box::new(SuffixCheckC::new(pattern, self.string))
                } else {
                    Box::new(SuffixCheck::new(pattern))
                }
            }
            CheckType::Substring => {
                if self.case == Case::Insens {
                    Box::new(InfixCheckC::new(pattern, self.string))
                } else {
                    Box::new(InfixCheck::new(pattern))
                }
            }
            CheckType::Glob => Box::new(GlobCheck::new(pattern, self.case)),
            CheckType::Empty => Box::new(LengthCheck::new("0,0")?),
            CheckType::Blank => Box::new(CmtCheck::new("")),
            CheckType::Comment => Box::new(CmtCheck::new(pattern)),
            CheckType::HashCmt => Box::new(CmtCheck::new("#")),
            CheckType::SlashCmt => Box::new(CmtCheck::new("//")),
        })
    }
}

/// to check a buffer
pub struct Checker {
    spec: CheckSpec,
    check: Box<dyn BufCheck>,
}

impl Checker {
    /// new
    pub fn new(spec: CheckSpec, check: Box<dyn BufCheck>) -> Self {
        Self { check, spec }
    }
}

impl fmt::Debug for Checker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Checker {:?}", &self.spec)
    }
}

/// to check a field
#[derive(Debug)]
pub struct ColChecker {
    check: Checker,
    col: NamedCol,
}

impl ColChecker {
    /// Column,Spec,Pattern
    /// Pattern may have additional commas
    pub fn new(spec: &str) -> Result<Self> {
        let parts = spec.split_once(',');
        if parts.is_none() {
            return err!(
                "Column Pattern has no commas : should be Column,Spec,Pattern : {}",
                spec
            );
        }
        let parts = parts.unwrap();
        let mut nc = NamedCol::new();
        nc.parse(parts.0)?;
        let (s, b) = make_match2(parts.1)?;
        Ok(Self {
            check: Checker::new(s, b),
            col: nc,
        })
    }
}

// LineCheck::check could return Result<bool> and then we can put strict back
impl LineCheck for ColChecker {
    fn check(&self, line: &TextLine) -> bool {
        self.check.spec.negate
            ^ if self.check.spec.string {
                self.check
                    .check
                    .scheck(&String::from_utf8_lossy(line.get(self.col.num)))
            } else {
                self.check.check.ucheck(line.get(self.col.num))
            }
    }

    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}

/// Full list of checkers, AND'ed
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
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
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
/// Full list of checkers, OR'ed
#[derive(Default)]
pub struct CheckListOr {
    checks: Vec<Box<dyn LineCheck>>,
}
impl fmt::Debug for CheckListOr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CheckListOr")
    }
}

impl CheckListOr {
    /// new
    pub fn new() -> Self {
        Self { checks: Vec::new() }
    }
    /// is empty?
    pub fn is_empty(&self) -> bool {
        self.checks.is_empty()
    }
    /// add Check to list
    pub fn push(&mut self, item: Box<dyn LineCheck>) {
        self.checks.push(item)
    }
    /// lookup
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in self.checks.iter_mut() {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
    /// ok
    pub fn ok(&self, line: &TextLine) -> bool {
        for x in &self.checks {
            if x.check(line) {
                return true;
            }
        }
        false
    }
    /// ok_verbose
    pub fn ok_verbose(&self, line: &TextLine) -> bool {
        for x in &self.checks {
            if x.check(line) {
                return true;
            }
        }
        false
    }
}

/// Full list of checkers, OR'ed
#[derive(Default)]
pub struct CheckOr {
    checks: Vec<Box<dyn BufCheck>>,
}
impl fmt::Debug for CheckOr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CheckOr")
    }
}

impl CheckOr {
    /// new
    pub fn new() -> Self {
        Self { checks: Vec::new() }
    }
    /// add Check to list
    pub fn push(&mut self, item: Box<dyn BufCheck>) {
        self.checks.push(item)
    }
    /// is empty?
    pub fn is_empty(&self) -> bool {
        self.checks.is_empty()
    }
}

impl BufCheck for CheckOr {
    fn scheck(&self, buff: &str) -> bool {
        for x in &self.checks {
            if x.scheck(buff) {
                return true;
            }
        }
        false
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        for x in &self.checks {
            if x.ucheck(buff) {
                return true;
            }
        }
        false
    }
}

/// Full list of checkers, OR'ed
#[derive(Default)]
pub struct CheckAnd {
    checks: Vec<Box<dyn BufCheck>>,
}
impl fmt::Debug for CheckAnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CheckAnd")
    }
}

impl CheckAnd {
    /// new
    pub fn new() -> Self {
        Self { checks: Vec::new() }
    }
    /// add Check to list
    pub fn push(&mut self, item: Box<dyn BufCheck>) {
        self.checks.push(item)
    }
    /// is empty?
    pub fn is_empty(&self) -> bool {
        self.checks.is_empty()
    }
}

impl BufCheck for CheckAnd {
    fn scheck(&self, buff: &str) -> bool {
        for x in &self.checks {
            if !x.scheck(buff) {
                return false;
            }
        }
        true
    }
    fn ucheck(&self, buff: &[u8]) -> bool {
        for x in &self.checks {
            if !x.ucheck(buff) {
                return false;
            }
        }
        true
    }
}