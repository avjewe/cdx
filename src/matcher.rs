//! Test if a [TextLine], `str` or `&[u8]` matches a pattern

use crate::column::NamedCol;
use crate::text::{Case, Text};
use crate::{err, Error, Reader, Result, TextLine};
use lazy_static::lazy_static;
use memchr::memmem::find;
use std::collections::HashSet;
use std::fmt;
use std::sync::Mutex;

/// Match against [TextLine]
pub trait LineMatch {
    /// Is this line ok?
    fn ok(&self, line: &TextLine) -> bool;
    /// Resolve any named columns.
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()>;
}

/// Match against `str` or `&[u8]`
pub trait BufMatch {
    /// Are these characters ok?
    fn smatch(&self, buff: &str) -> bool;
    /// Are these bytes ok?
    fn umatch(&self, buff: &[u8]) -> bool;
}

/// pattern is prefix of string
#[derive(Debug, Clone)]
struct PrefixMatch {
    data: String,
}
impl PrefixMatch {
    fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
        }
    }
}
impl BufMatch for PrefixMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.starts_with(&self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.starts_with(self.data.as_bytes())
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
struct PrefixMatchC {
    data: String,
}
impl PrefixMatchC {
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

impl BufMatch for PrefixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        scase_prefix(buff, &self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        bcase_prefix(buff, self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string
struct SuffixMatch {
    data: String,
}
impl SuffixMatch {
    fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
        }
    }
}
impl BufMatch for SuffixMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.ends_with(&self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.ends_with(self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is suffix of string, case insensitive
struct SuffixMatchC {
    data: String,
}
impl SuffixMatchC {
    fn new(data: &str, unicode: bool) -> Self {
        Self {
            data: str_to_lower(data, unicode),
        }
    }
}
impl BufMatch for SuffixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        scase_suffix(buff, &self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        bcase_suffix(buff, self.data.as_bytes())
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string, case insensitive
struct InfixMatchC {
    data: String,
}
impl InfixMatchC {
    fn new(data: &str, unicode: bool) -> Self {
        Self {
            data: str_to_lower(data, unicode),
        }
    }
}
impl BufMatch for InfixMatchC {
    fn smatch(&self, buff: &str) -> bool {
        buff.to_lowercase().contains(&self.data) // PERF allocation
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        find(&buff.to_ascii_lowercase(), self.data.as_bytes()).is_some() // PERF allocation
    }
}

#[derive(Debug, Clone)]
/// pattern is substring of string
struct InfixMatch {
    needle: Vec<u8>,
}
impl InfixMatch {
    fn new(data: &str) -> Self {
        Self {
            needle: data.as_bytes().to_vec(),
        }
    }
}
impl BufMatch for InfixMatch {
    fn smatch(&self, haystack: &str) -> bool {
        find(haystack.as_bytes(), &self.needle).is_some()
    }
    fn umatch(&self, haystack: &[u8]) -> bool {
        find(haystack, &self.needle).is_some()
    }
}

#[derive(Debug, Clone)]
/// bytes regex
struct RegexMatchB {
    data: regex::bytes::Regex,
}
impl RegexMatchB {
    fn new(data: &str, case: Case) -> Result<Self> {
        Ok(Self {
            data: regex::bytes::RegexBuilder::new(data)
                .case_insensitive(case == Case::Insens)
                .build()?,
        })
    }
}
impl BufMatch for RegexMatchB {
    fn smatch(&self, _buff: &str) -> bool {
        unimplemented!()
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.data.is_match(buff)
    }
}

#[derive(Debug, Clone)]
/// utf8 regex
struct RegexMatchS {
    data: regex::Regex,
}
impl RegexMatchS {
    fn new(data: &str, case: Case) -> Result<Self> {
        Ok(Self {
            data: regex::RegexBuilder::new(data)
                .case_insensitive(case == Case::Insens)
                .build()?,
        })
    }
}
impl BufMatch for RegexMatchS {
    fn smatch(&self, buff: &str) -> bool {
        self.data.is_match(buff)
    }
    fn umatch(&self, _buff: &[u8]) -> bool {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern
struct ExactMatch {
    data: Vec<u8>,
}
impl ExactMatch {
    fn new(data: &str) -> Self {
        Self {
            data: data.as_bytes().to_vec(),
        }
    }
}
impl BufMatch for ExactMatch {
    fn smatch(&self, buff: &str) -> bool {
        self.data == buff.as_bytes()
    }
    fn umatch(&self, buff: &[u8]) -> bool {
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
struct FileExactMatch {
    data: HashSet<Vec<u8>>,
}
impl FileExactMatch {
    fn new(data: &str) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset(&mut d, data)?;
        Ok(Self { data: d })
    }
}
impl BufMatch for FileExactMatch {
    fn smatch(&self, buff: &str) -> bool {
        self.data.contains(buff.as_bytes())
    }
    fn umatch(&self, buff: &[u8]) -> bool {
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
struct FileExactMatchC {
    data: HashSet<Vec<u8>>,
}
impl FileExactMatchC {
    fn new(data: &str, unicode: bool) -> Result<Self> {
        let mut d = HashSet::new();
        load_hashset_c(&mut d, data, unicode)?;
        Ok(Self { data: d })
    }
}
impl BufMatch for FileExactMatchC {
    fn smatch(&self, buff: &str) -> bool {
        self.data.contains(&buff.new_lower().into_bytes()) // PERF allocation
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.data.contains(&buff.new_lower()) // PERF allocation
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
        Self {
            data: data.to_string(),
            ic,
        }
    }
}
impl BufMatch for GlobMatch {
    fn smatch(&self, buff: &str) -> bool {
        buff.glob(&self.data, self.ic)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.glob(self.data.as_bytes(), self.ic)
    }
}

#[derive(Debug, Clone)]
/// string exactly matches pattern, case insensitive
struct ExactMatchC {
    data: String,
}

impl ExactMatchC {
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
impl BufMatch for ExactMatchC {
    fn smatch(&self, buff: &str) -> bool {
        buff.equal_insens_quick(&self.data)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        buff.equal_insens_quick(self.data.as_bytes())
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// Always Matches
struct YesMatch {}
impl BufMatch for YesMatch {
    fn smatch(&self, _buff: &str) -> bool {
        true
    }
    fn umatch(&self, _buff: &[u8]) -> bool {
        true
    }
}

#[derive(Debug, Clone, Default, Copy)]
/// Never Matches
struct NoMatch {}
impl BufMatch for NoMatch {
    fn smatch(&self, _buff: &str) -> bool {
        false
    }
    fn umatch(&self, _buff: &[u8]) -> bool {
        false
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
        fn with_sizes(min : usize, max : Option<usize>) -> Self {
        Self {min, max}
        }
    */
}

impl BufMatch for LengthMatch {
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
}

/// Mode for combining parts of a multi-match
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MultiMode {
    /// matches if any match
    Or,
    /// matches only if all match
    And,
}

impl Default for MultiMode {
    fn default() -> Self {
        Self::And
    }
}

/// Does a particular column of a line match a pattern
#[allow(missing_debug_implementations)]
pub struct ColMatcher {
    matcher: Matcher,
    col: NamedCol,
}

impl ColMatcher {
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
        Ok(Self {
            matcher: MatchMaker::make(parts.1)?,
            col: nc,
        })
    }
}

// LineMatch::matcher could return Result<bool> and then we can put strict back
impl LineMatch for ColMatcher {
    fn ok(&self, line: &TextLine) -> bool {
        self.matcher.negate
            ^ if self.matcher.string {
                self.matcher
                    .smatch(&String::from_utf8_lossy(line.get(self.col.num)))
            } else {
                self.matcher.umatch(line.get(self.col.num))
            }
    }

    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.col.lookup(fieldnames)
    }
}

/// List of [LineMatch], combined with AND or OR
#[derive(Default)]
pub struct LineMatchMulti {
    /// the mode
    pub multi: MultiMode,
    /// the matchers
    pub matchers: Vec<Box<dyn LineMatch>>,
}
impl fmt::Debug for LineMatchMulti {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LineMatchMulti {:?}", self.multi)
    }
}

impl LineMatchMulti {
    /// new
    pub fn new() -> Self {
        Self {
            multi: MultiMode::And,
            matchers: Vec::new(),
        }
    }
    /// add Matcher to list
    pub fn push(&mut self, item: Box<dyn LineMatch>) {
        self.matchers.push(item)
    }
    /// ok, with supplied MultiMode
    pub fn ok_tagged(&self, line: &TextLine, multi: MultiMode) -> bool {
        match multi {
            MultiMode::And => self.ok_and(line),
            MultiMode::Or => self.ok_or(line),
        }
    }
    /// ok, with AND
    pub fn ok_and(&self, line: &TextLine) -> bool {
        for x in &self.matchers {
            if !x.ok(line) {
                return false;
            }
        }
        true
    }
    /// ok, with OR
    pub fn ok_or(&self, line: &TextLine) -> bool {
        for x in &self.matchers {
            if x.ok(line) {
                return true;
            }
        }
        false
    }
}

impl LineMatch for LineMatchMulti {
    fn ok(&self, line: &TextLine) -> bool {
        self.ok_tagged(line, self.multi)
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in self.matchers.iter_mut() {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
}

/// List of [Matcher], combined with AND or OR
#[derive(Default)]
pub struct MatchMulti {
    /// the mode
    pub multi: MultiMode,
    /// the matchers
    pub matchers: Vec<Matcher>,
}
impl fmt::Debug for MatchMulti {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MatchOr {:?}", self.multi)
    }
}

impl MatchMulti {
    /// new, default mode
    pub fn new() -> Self {
        Self::default()
    }
    /// new, explicit mode
    pub const fn with_tag(multi: MultiMode) -> Self {
        Self {
            multi,
            matchers: Vec::new(),
        }
    }
    /// add Matcher to list
    pub fn push(&mut self, item: Matcher) {
        self.matchers.push(item)
    }
    /// Are there any matchers in the list?
    pub fn is_empty(&self) -> bool {
        self.matchers.is_empty()
    }
    fn smatch_tagged(&self, buff: &str, multi: MultiMode) -> bool {
        match multi {
            MultiMode::And => self.smatch_and(buff),
            MultiMode::Or => self.smatch_or(buff),
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
    fn umatch_tagged(&self, buff: &[u8], multi: MultiMode) -> bool {
        match multi {
            MultiMode::And => self.umatch_and(buff),
            MultiMode::Or => self.umatch_or(buff),
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
}

impl BufMatch for MatchMulti {
    fn smatch(&self, buff: &str) -> bool {
        self.smatch_tagged(buff, self.multi)
    }
    fn umatch(&self, buff: &[u8]) -> bool {
        self.umatch_tagged(buff, self.multi)
    }
}

/// Match a pattern against a target
//#[derive(Debug, Clone, Default)]
#[allow(missing_debug_implementations)]
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
    multi_mode: Option<MultiMode>,
    /// the matching object
    matcher: Box<dyn BufMatch>,
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
    pub fn smatch(&self, mut buff: &str) -> bool {
        if self.trim {
            buff = buff.trimw();
        }
        if self.empty && buff.is_empty() {
            true
        } else {
            self.matcher.smatch(buff)
        }
    }
    /// Are these bytes ok?
    pub fn umatch(&self, mut buff: &[u8]) -> bool {
        if self.trim {
            buff = buff.trimw();
        }
        if self.empty && buff.is_empty() {
            true
        } else {
            self.matcher.umatch(buff)
        }
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
    pub fn do_match_safe(&self, buff: &[u8]) -> bool {
        if self.string {
            if let Ok(x) = std::str::from_utf8(buff) {
                self.smatch(x)
            } else {
                false
            }
        } else {
            self.umatch(buff)
        }
    }
}

type MakerBox = Box<dyn Fn(&mut Matcher, &str) -> Result<Box<dyn BufMatch>> + Send>;
/// A named constructor for a [BufMatch], used by [MatchMaker]
#[allow(missing_debug_implementations)]
pub struct MatchMakerItem {
    /// matched against Matcher::ctype
    pub tag: &'static str,
    /// what this matcher does
    pub help: &'static str,
    /// Create a dyn BufMatch from a pattern
    pub maker: MakerBox,
}

struct MatchMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

macro_rules! mm {
    ($a:expr,$b:expr,$c:expr) => {
        MatchMakerItem {
            tag: $a,
            help: $b,
            maker: Box::new($c),
        }
    };
}

lazy_static! {
    static ref MATCH_MAKER: Mutex<Vec<MatchMakerItem>> = Mutex::new(Vec::new());
    static ref MATCH_ALIAS: Mutex<Vec<MatchMakerAlias>> = Mutex::new(Vec::new());
    static ref MODIFIERS: Vec<&'static str> =
        vec!["utf8", "not", "trim", "null", "case", "and", "or"];
}

/// Makes a [Matcher]
#[derive(Debug, Clone, Default)]
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
        Self::do_push(mm!(
            "prefix",
            "Is the pattern a prefix of the target?",
            |m, p| Ok(if m.case == Case::Insens {
                Box::new(PrefixMatchC::new(p, m.string))
            } else {
                Box::new(PrefixMatch::new(p))
            })
        ))?;
        Self::do_push(mm!(
            "regex",
            "Interpret the pattern as a regex, as per the eponymous crate",
            |m, p| Ok(if m.string {
                Box::new(RegexMatchS::new(p, m.case)?)
            } else {
                Box::new(RegexMatchB::new(p, m.case)?)
            })
        ))?;
        Self::do_push(mm!(
            "file-exact",
            "Is the target exactly one one the lines in this file?",
            |m, p| Ok(if m.case == Case::Insens {
                Box::new(FileExactMatchC::new(p, m.string)?)
            } else {
                Box::new(FileExactMatch::new(p)?)
            })
        ))?;
        Self::do_push(mm!(
            "exact",
            "Does the target exactly match the pattern",
            |m, p| Ok(if m.case == Case::Insens {
                Box::new(ExactMatchC::new(p, m.string)?)
            } else {
                Box::new(ExactMatch::new(p))
            })
        ))?;
        Self::do_push(mm!(
            "length",
            "For the pattern X,Y, length is between X and Y inclusive",
            |_m, p| Ok(Box::new(LengthMatch::new(p)?))
        ))?;
        Self::do_push(mm!("yes", "always matches", |_m, _p| Ok(Box::new(
            YesMatch {}
        ))))?;
        Self::do_push(mm!(
            "suffix",
            "Is the pattern a suffix of the target?",
            |m, p| Ok(if m.case == Case::Insens {
                Box::new(SuffixMatchC::new(p, m.string))
            } else {
                Box::new(SuffixMatch::new(p))
            })
        ))?;
        Self::do_push(mm!(
            "infix",
            "Is the pattern a substring of the target?",
            |m, p| Ok(if m.case == Case::Insens {
                Box::new(InfixMatchC::new(p, m.string))
            } else {
                Box::new(InfixMatch::new(p))
            })
        ))?;
        Self::do_push(mm!("glob", "Treat the pattern as a shell glob", |m, p| Ok(
            Box::new(GlobMatch::new(p, m.case))
        )))?;
        Self::do_push(mm!(
            "empty",
            "Is the target empty, i.e. zero bytes",
            |_m, _p| Ok(Box::new(LengthMatch::new("0,0")?))
        ))?;
        Self::do_push(mm!(
            "blank",
            "Is the target made entirely of white space",
            |m, _p| {
                m.trim = true;
                m.empty = true;
                Ok(Box::new(NoMatch {}))
            }
        ))?;
        Self::do_push(mm!(
            "comment",
            "Is the target white space followed by the pattern?",
            |m, p| {
                m.trim = true;
                m.empty = true;
                Ok(Box::new(PrefixMatch::new(p)))
            }
        ))?;
        Self::do_push(mm!(
            "hash",
            "Is the target white space folled by the # character?",
            |m, _p| {
                m.trim = true;
                m.empty = true;
                Ok(Box::new(PrefixMatch::new("#")))
            }
        ))?;
        Self::do_push(mm!(
            "slash",
            "Is the target white space followed by //",
            |m, _p| {
                m.trim = true;
                m.empty = true;
                Ok(Box::new(PrefixMatch::new("//")))
            }
        ))
    }
    /// Add a new matcher. If a Matcher already exists by that name, replace it.
    pub fn push(m: MatchMakerItem) -> Result<()> {
        Self::init()?;
        Self::do_push(m)
    }
    /// Add a new alias. If an alias already exists by that name, replace it.
    pub fn add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        Self::init()?;
        Self::do_add_alias(old_name, new_name)
    }
    /// Return name, replaced by its alias, if any.
    fn resolve_alias(name: String) -> String {
        let mut mm = MATCH_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == name {
                return x.old_name.to_string();
            }
        }
        name
    }
    fn do_add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        if MODIFIERS.contains(&new_name) {
            return err!("You can't add an alias named {} because that is reserved for a modifier");
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
        Ok(())
    }
    fn do_push(m: MatchMakerItem) -> Result<()> {
        if MODIFIERS.contains(&m.tag) {
            return err!(
                "You can't add a matcher named {} because that is reserved for a modifier"
            );
        }
        let mut mm = MATCH_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        Ok(())
    }
    /// Print all available Matchers to stdout.
    pub fn help() {
        Self::init().unwrap();
        let mm = MATCH_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
        }
    }
    /// Call pr for every Matcher.
    pub fn help1(pr: &dyn Fn(&str, &str)) {
        Self::init().unwrap();
        let mm = MATCH_MAKER.lock().unwrap();
        for x in &*mm {
            pr(x.tag, x.help);
        }
    }
    /// Create a Matcher from a matcher spec and a pattern
    pub fn make2(matcher: &str, pattern: &str) -> Result<Matcher> {
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
                    m.multi_mode = Some(MultiMode::And);
                } else if x.eq_ignore_ascii_case("or") {
                    m.multi_mode = Some(MultiMode::Or);
                } else {
                    m.ctype = x.to_string();
                }
            }
            m.ctype = Self::resolve_alias(m.ctype);
        }
        if let Some(mm) = m.multi_mode {
            let mut outer = Box::new(MatchMulti::new());
            outer.multi = mm;
            let mut pattern = pattern;
            let delim = pattern.first();
            pattern = pattern.skip_first();
            for x in pattern.split(delim) {
                let mut m2 = m.clone();
                m2.multi_mode = None; // not really necessary
                Self::remake(&mut m2, x)?;
                outer.push(m2);
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
    /// Remake the dyn BufMatch based on current contents.
    pub fn remake(m: &mut Matcher, pattern: &str) -> Result<()> {
        Self::init()?;
        let mm = MATCH_MAKER.lock().unwrap();
        for x in &*mm {
            if x.tag == m.ctype {
                m.matcher = (x.maker)(m, pattern)?;
                return Ok(());
            }
        }
        err!("Unknown matcher : {}", m.ctype)
    }
}
