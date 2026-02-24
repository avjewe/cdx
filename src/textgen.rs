//! Generate random text

use crate::expr;
use crate::prelude::*;
use rand_distr::{Distribution, Normal};
use std::sync::Mutex;

/// location context for Gen
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
pub struct Where {
    /// 1-based column number
    pub col: usize,
    /// 1-based line number
    pub line: usize,
}

/// generate some text as a column value
pub trait Gen {
    /// write one column value
    fn write(&mut self, w: &mut dyn Write, loc: &Where) -> Result<()>;
}

/// generate a whole file
pub trait FullGen {
    /// write one column value
    fn write(&mut self, w: &mut dyn Write) -> Result<()>;
}
struct BytesGen {
    size: usize,
}
impl BytesGen {
    fn new(p: &str) -> Result<Self> {
        let size = p.parse::<usize>()?;
        Ok(Self { size })
    }
}
impl FullGen for BytesGen {
    fn write(&mut self, w: &mut dyn Write) -> Result<()> {
        for _ in 0..self.size {
            w.write_all(&[fastrand::u8(..255)])?;
        }
        Ok(())
    }
}
pub(crate) struct RankGen {
    candidates: String,
    vocab: String,
}
impl RankGen {
    fn new(p: &str) -> Result<Self> {
        let parts: Vec<_> = p.split(',').collect();
        if parts.len() != 2 {
            return err!("RankGen requires two comma delimited file names: candidates,vocab.");
        }
        Ok(Self { candidates: parts[0].to_string(), vocab: parts[1].to_string() })
    }
    fn contains(c: &[u8], char: u8, cnt: u8) -> bool {
        let mut count = cnt;
        for ch in c {
            if *ch == char {
                if count == 0 {
                    return true;
                }
                count -= 1;
            }
        }
        false
    }

    pub(crate) fn validate(word: &[u8]) -> Result<()> {
        for ch in word {
            if !ch.is_ascii_lowercase() {
                return err!(
                    "All words must be lower case ascii. {}",
                    String::from_utf8_lossy(word)
                );
            }
        }
        Ok(())
    }

    #[expect(clippy::cast_possible_wrap)]
    fn get_score(candidates: &[Vec<u8>], word: &[u8]) -> i64 {
        let mut used = [0u8; 26];
        let mut score = 0i64;
        let half = candidates.len() as i64 / 2;
        for ch in word {
            let pos = ch - b'a';
            let cnt = used[pos as usize];
            used[pos as usize] += 1;
            let mut count = 0i64;
            for c in candidates {
                if Self::contains(c, *ch, cnt) {
                    count += 1;
                }
            }
            score += half - (count - half).abs();
        }
        for pos in 0..word.len() {
            let mut count = 0i64;
            for c in candidates {
                if word[pos] == c[pos] {
                    count += 1;
                }
            }
            score += half - (count - half).abs();
        }

        score
    }
}
impl FullGen for RankGen {
    fn write(&mut self, w: &mut dyn Write) -> Result<()> {
        let mode = TextFileMode::default();
        let mut cand = Reader::new_open(&self.candidates, &mode)?;
        let mut vocab = Reader::new_open(&self.vocab, &mode)?;

        if cand.is_done() {
            return err!("Candidate file must not be empty.");
        }
        let mut candidates = Vec::new();
        let len = cand.curr_line().get(0).len();
        loop {
            let word = cand.curr_line().get(0);
            if word.len() != len {
                return err!(
                    "All candidate words must have same length. Saw {len} and {}",
                    word.len()
                );
            }
            Self::validate(word)?;
            candidates.push(word.to_vec());
            if cand.get_line()? {
                break;
            }
        }

        if vocab.is_done() {
            return Ok(());
        }
        loop {
            let word = vocab.curr_line().get(0);
            if word.len() == len {
                Self::validate(word)?;
                let score = Self::get_score(&candidates, word);
                w.write_all(word)?;
                w.write_all(b"\t")?;
                writeln!(w, "{score}")?;
            }
            if vocab.get_line()? {
                break;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct DecimalGen {
    sign: bool,
    pre: usize,
    post: usize,
    post_size: usize,
}
impl DecimalGen {
    // [-]NNN[.NNNN]
    fn new(spec: &str) -> Self {
        let mut sign = false;
        let mut pre = 10000;
        let mut post = 0;
        let mut post_size = 0;
        let mut spec = spec;
        if !spec.is_empty() {
            if spec.first() == '-' {
                sign = true;
                spec = &spec[1..];
            }
            if !spec.is_empty() {
                if let Some((a, b)) = spec.split_once('.') {
                    pre = 10_usize.pow(a.len() as u32);
                    post_size = b.len();
                    post = 10_usize.pow(post_size as u32);
                } else {
                    pre = 10_usize.pow(spec.len() as u32);
                }
            }
        }
        // eprintln!("{:?}", Self { sign, pre, post, post_size });
        Self { sign, pre, post, post_size }
    }
}
impl Gen for DecimalGen {
    fn write(&mut self, w: &mut dyn Write, _loc: &Where) -> Result<()> {
        let sign = if self.sign && fastrand::bool() { "-" } else { "" };
        let post = self.post_size;
        if post == 0 {
            write!(w, "{sign}{}", fastrand::usize(..self.pre))?;
        } else {
            write!(
                w,
                "{sign}{}.{:0post$}",
                fastrand::usize(..self.pre),
                fastrand::usize(..self.post)
            )?;
        }
        Ok(())
    }
}

struct ExprGen {
    expr: Expr,
    fmt: NumFormat,
    col_pos: usize,
    line_pos: usize,
}

impl ExprGen {
    fn new(spec: &str) -> Result<Self> {
        let (fmt, expr_spec) = expr::parse_fmt_expr(NumFormat::default(), spec);
        let mut expr = Expr::new(expr_spec)?;
        let col_pos = expr.find_var("col");
        let line_pos = expr.find_var("line");
        Ok(Self { expr, fmt, col_pos, line_pos })
    }
}
impl Gen for ExprGen {
    #[allow(clippy::cast_precision_loss)]
    fn write(&mut self, w: &mut dyn Write, loc: &Where) -> Result<()> {
        self.expr.set_var(self.col_pos, loc.col as f64);
        self.expr.set_var(self.line_pos, loc.line as f64);
        self.fmt.print(self.expr.eval_plain()?, w)
    }
}

struct NormalDistGen {
    fmt: NumFormat,
    norm: Normal<f64>,
}
impl NormalDistGen {
    fn new(spec: &str) -> Result<Self> {
        let mut mean = 0.0;
        let mut dev = 1.0;
        let mut fmt = NumFormat::default();
        if !spec.is_empty() {
            for (i, x) in spec.split(',').enumerate() {
                match i {
                    0 => mean = x.to_f64_whole(spec.as_bytes(), "normal distribution")?,
                    1 => dev = x.to_f64_whole(spec.as_bytes(), "normal distribution")?,
                    2 => fmt = NumFormat::new(x)?,
                    _ => {
                        return err!(
                            "Normal Dist Spec must be no more than three comma delimited pieces"
                        );
                    }
                }
            }
        }
        Ok(Self { fmt, norm: Normal::new(mean, dev).unwrap() })
    }
}
impl Gen for NormalDistGen {
    fn write(&mut self, w: &mut dyn Write, _loc: &Where) -> Result<()> {
        let v = self.norm.sample(&mut rand::rng());
        self.fmt.print(v, w)
    }
}

struct NullGen {}
impl Gen for NullGen {
    fn write(&mut self, _w: &mut dyn Write, _loc: &Where) -> Result<()> {
        Ok(())
    }
}

struct GridGen {}
impl Gen for GridGen {
    fn write(&mut self, w: &mut dyn Write, loc: &Where) -> Result<()> {
        write!(w, "{}_{}", loc.line, loc.col)?;
        Ok(())
    }
}

struct CountGen {
    start: isize,
}
impl CountGen {
    fn new(spec: &str) -> Result<Self> {
        Ok(Self {
            start: if spec.is_empty() {
                0
            } else {
                spec.to_isize_whole(spec.as_bytes(), "count generator")?
            },
        })
    }
}
impl Gen for CountGen {
    fn write(&mut self, w: &mut dyn Write, loc: &Where) -> Result<()> {
        write!(w, "{}", loc.line.cast_signed() + self.start - 1)?;
        Ok(())
    }
}

/// A dyn Gen with some context
pub struct TextGen {
    /// the spec
    pub spec: String,
    /// the Gen
    pub text_gen: Box<dyn Gen>,
}
/// A dyn Gen with some context
pub struct FullTextGen {
    /// the spec
    pub spec: String,
    /// the Gen
    pub text_gen: Box<dyn FullGen>,
}
impl Default for TextGen {
    fn default() -> Self {
        Self { spec: String::new(), text_gen: Box::new(NullGen {}) }
    }
}
impl Default for FullTextGen {
    fn default() -> Self {
        Self { spec: String::new(), text_gen: Box::new(BytesGen::new("1").unwrap()) }
    }
}
impl fmt::Debug for TextGen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.spec)
    }
}
impl fmt::Debug for FullTextGen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.spec)
    }
}
impl Clone for TextGen {
    fn clone(&self) -> Self {
        GenMaker::make(&self.spec).unwrap()
    }
}
impl Clone for FullTextGen {
    fn clone(&self) -> Self {
        GenMaker::full_make(&self.spec).unwrap()
    }
}

/// A List of Gen
#[derive(Debug, Default)]
pub struct GenList {
    v: Vec<TextGen>,
    tmp: Vec<u8>,
    line: usize,
}
impl GenList {
    /// new
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    /// add a Gen
    pub fn push(&mut self, spec: &str) -> Result<()> {
        self.v.push(GenMaker::make(spec)?);
        Ok(())
    }
    /// is empty?
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    /// length?
    #[must_use]
    pub const fn len(&self) -> usize {
        self.v.len()
    }
    /// Write full line of Gens
    pub fn write(&mut self, w: &mut impl Write, delim: u8) -> Result<()> {
        self.line += 1;
        let mut loc = Where { col: 1, line: self.line };
        let mut need_delim = false;
        for x in &mut self.v {
            if need_delim {
                w.write_all(&[delim])?;
            }
            need_delim = true;
            self.tmp.clear();
            x.text_gen.write(&mut self.tmp, &loc)?;
            w.write_all(&self.tmp)?;
            loc.col += 1;
        }
        w.write_all(b"\n")?;
        Ok(())
    }
}

type MakerBox = Box<dyn Fn(&str) -> Result<Box<dyn Gen>> + Send>;
type FullMakerBox = Box<dyn Fn(&str) -> Result<Box<dyn FullGen>> + Send>;
/// A named constructor for a [Gen], used by [`GenMaker`]
struct GenMakerItem {
    /// name of Gen
    tag: &'static str,
    /// what this Gen does
    help: &'static str,
    /// Create a dyn Gen from a pattern
    maker: MakerBox,
}

struct FullGenMakerItem {
    /// name of Gen
    tag: &'static str,
    /// what this Gen does
    help: &'static str,
    /// Create a dyn Gen from a pattern
    maker: FullMakerBox,
}

struct GenMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

static FULL_GEN_MAKER: Mutex<Vec<FullGenMakerItem>> = Mutex::new(Vec::new());
static GEN_MAKER: Mutex<Vec<GenMakerItem>> = Mutex::new(Vec::new());
static GEN_ALIAS: Mutex<Vec<GenMakerAlias>> = Mutex::new(Vec::new());
const MODIFIERS: Vec<&'static str> = vec![];

/// Makes an [`TextGen`]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default, Hash)]
pub struct GenMaker {}

impl GenMaker {
    pub(crate) fn init() -> Result<()> {
        Self::init_gen()?;
        Self::init_full_gen()
    }
    pub(crate) fn init_full_gen() -> Result<()> {
        if !FULL_GEN_MAKER.lock().unwrap().is_empty() {
            return err!("Double init of GenMaker not allowed");
        }
        Self::do_push_full("bytes", "Write a file of N random bytes. E.g. `bytes,42`", |p| {
            Ok(Box::new(BytesGen::new(p)?))
        })?;
        Self::do_push_full(
            "rank",
            "return vocab ranked by best match to candidates. E.g. `rank,candidates,vocab`",
            |p| Ok(Box::new(RankGen::new(p)?)),
        )?;
        Ok(())
    }
    pub(crate) fn init_gen() -> Result<()> {
        if !GEN_MAKER.lock().unwrap().is_empty() {
            return err!("Double init of GenMaker not allowed");
        }
        Self::do_add_alias("min", "minimum")?;
        Self::do_push("null", "Produce empty column value", |_p| Ok(Box::new(NullGen {})))?;
        Self::do_push("expr", "'fmt,expr' e.g. plain,line*col OR just 'expr'", |p| {
            Ok(Box::new(ExprGen::new(p)?))
        })?;
        Self::do_push("normal", "Normal Distribution Mean,Dev,Fmt", |p| {
            Ok(Box::new(NormalDistGen::new(p)?))
        })?;
        Self::do_push("decimal", "Decimal number. Pattern is [-]NNN[.NNN]", |p| {
            Ok(Box::new(DecimalGen::new(p)))
        })?;
        Self::do_push("grid", "Produce line_col", |_p| Ok(Box::new(GridGen {})))?;
        Self::do_push("count", "Count up from starting place", |p| {
            Ok(Box::new(CountGen::new(p)?))
        })?;
        Ok(())
    }
    /// Add a new Gen. If an Gen already exists by that name, replace it.
    pub fn push<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&str) -> Result<Box<dyn Gen>> + Send + 'static,
    {
        Self::do_push(tag, help, maker)
    }
    /// Add a new alias. If an alias already exists by that name, replace it.
    pub fn add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        Self::do_add_alias(old_name, new_name)
    }
    /// Return name, replaced by its alias, if any.
    fn resolve_alias(name: &str) -> &str {
        for x in GEN_ALIAS.lock().unwrap().iter_mut() {
            if x.new_name == name {
                return x.old_name;
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
        let m = GenMakerAlias { old_name, new_name };
        let mut mm = GEN_ALIAS.lock().unwrap();
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
        F: Fn(&str) -> Result<Box<dyn Gen>> + Send + 'static,
    {
        if MODIFIERS.contains(&tag) {
            return err!("You can't add a agg named {tag} because that is reserved for a modifier");
        }
        let m = GenMakerItem { tag, help, maker: Box::new(maker) };
        let mut mm = GEN_MAKER.lock().unwrap();
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
    fn do_push_full<F>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&str) -> Result<Box<dyn FullGen>> + Send + 'static,
    {
        if MODIFIERS.contains(&tag) {
            return err!("You can't add a agg named {tag} because that is reserved for a modifier");
        }
        let m = FullGenMakerItem { tag, help, maker: Box::new(maker) };
        let mut mm = FULL_GEN_MAKER.lock().unwrap();
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
        //        println!("Modifiers :");
        //        println!("utf8 : do the unicode thing, rather than the ascii thing.");
        println!("Methods :");
        let mut results = Vec::new();
        for x in &*GEN_MAKER.lock().unwrap() {
            results.push(format!("{:12}{}", x.tag, x.help));
        }
        results.sort();
        for x in &results {
            println!("{x}");
        }
        println!("\nFull Methods :");
        results.clear();
        for x in &*FULL_GEN_MAKER.lock().unwrap() {
            results.push(format!("{:12}{}", x.tag, x.help));
        }
        results.sort();
        for x in results {
            println!("{x}");
        }
        println!();
        println!("See also https://avjewe.github.io/cdxdoc/TextGen.html.");
    }
    /// Create a `TextGen` from a name and a pattern
    pub fn make2(spec: &str, pattern: &str, orig: &str) -> Result<TextGen> {
        let mut name = "";
        if !spec.is_empty() {
            for x in spec.split('.') {
                //                if x.eq_ignore_ascii_case("utf8") {
                //                } else {
                name = x;
            }
            name = Self::resolve_alias(name);
        }
        for x in &*GEN_MAKER.lock().unwrap() {
            if x.tag == name {
                return Ok(TextGen { spec: orig.to_string(), text_gen: (x.maker)(pattern)? });
            }
        }
        err!("No Gen found with name '{}'", name)
    }
    /// Create a `FullGen` from a name and a pattern
    pub fn full_make2(spec: &str, pattern: &str, orig: &str) -> Result<FullTextGen> {
        let mut name = "";
        if !spec.is_empty() {
            for x in spec.split('.') {
                name = x;
            }
        }
        for x in &*FULL_GEN_MAKER.lock().unwrap() {
            if x.tag == name {
                return Ok(FullTextGen { spec: orig.to_string(), text_gen: (x.maker)(pattern)? });
            }
        }
        err!("No FullGen found with name '{}'", name)
    }
    /// Create a `TextGen` from a full spec, i.e. "Gen,Pattern"
    pub fn make(spec: &str) -> Result<TextGen> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::make2(a, b, spec)
        } else {
            Self::make2(spec, "", spec)
        }
    }
    /// Create a `FullGen` from a full spec, i.e. "FullGen,Pattern"
    pub fn full_make(spec: &str) -> Result<FullTextGen> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::full_make2(a, b, spec)
        } else {
            Self::full_make2(spec, "", spec)
        }
    }
}
