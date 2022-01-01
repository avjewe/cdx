//! Aggregate Values

use crate::column::{ColumnFun, ColumnHeader, ColumnSingle, NamedCol, Writer};
use crate::comp::{Comp, CompMaker};
use crate::num::str_to_d_lossy;
use crate::util::{err, Error, Result, StringLine, TextLine};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::io::Write;
use std::rc::Rc;
use std::sync::Mutex;

// uniq says : Column,Min,Method,Pattern
// make Writer from Column or AggColumn, option to add agg as new column or to replace
// error if multuple agg for same column,
// ColumnFun from Agg and Column

/// Aggregate Values
pub trait Agg {
    /// add one more value to the aggregate
    fn add(&mut self, data: &[u8]);
    /// print result of aggregation
    fn result(&mut self, w: &mut dyn Write) -> Result<()>;
    /// reset to empty or zero state
    fn reset(&mut self);
}

type AggRef = Rc<RefCell<dyn Agg>>;

#[derive(Clone, Debug)]
/// Does this AggCol replace a column, or add a new column
pub enum AggType {
    /// replace a column
    Replace,
    /// add a new column at the beginning
    Prefix(String),
    /// add a new column
    Append(String),
}

impl Default for AggType {
    fn default() -> Self {
        Self::Replace
    }
}

/// An Agg with a source column and an output designation
#[derive(Clone)]
pub struct AggCol {
    /// source column
    pub src: NamedCol,
    /// Aggregator
    pub agg: AggRef,
    /// output style
    pub mode: AggType,
}
impl std::fmt::Debug for AggCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("AggCol")
    }
}

impl AggCol {
    /// new replace
    pub fn new_replace(spec: &str) -> Result<Self> {
        Self::new2(AggType::Replace, spec)
    }
    /// new prefix
    pub fn new_prefix(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::new2(AggType::Prefix(a.to_string()), b)
        } else {
            err!("Prefix format is NewName,Column,Spec : {}", spec)
        }
    }
    /// new append
    pub fn new_append(spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::new2(AggType::Append(a.to_string()), b)
        } else {
            err!("Append format is NewName,Column,Spec : {}", spec)
        }
    }
    /// new from AggType and spec
    pub fn new2(mode: AggType, spec: &str) -> Result<Self> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::new3(mode, a, b)
        } else {
            Self::new3(mode, spec, "")
        }
    }
    /// new from AggType and spec
    pub fn new3(mode: AggType, src: &str, spec: &str) -> Result<Self> {
        Ok(Self {
            src: NamedCol::new_from(src)?,
            agg: AggMaker::make(spec)?,
            mode,
        })
    }
}

impl ColumnFun for AggCol {
    /// write the column names (called once)
    fn add_names(&self, w: &mut ColumnHeader, head: &StringLine) -> Result<()> {
        match &self.mode {
            AggType::Replace => w.push(head.get(self.src.num)),
            AggType::Prefix(s) => w.push(s),
            AggType::Append(s) => w.push(s),
        }
    }
    /// write the column values (called many times)
    fn write(&mut self, w: &mut dyn Write, _line: &TextLine, _delim: u8) -> Result<()> {
        self.agg.borrow_mut().result(w)
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.src.lookup(fieldnames)
    }
}

/// A list of AggCol
#[derive(Clone, Default, Debug)]
pub struct AggList {
    /// The aggregators
    v: Vec<AggCol>,
}

impl AggList {
    /// new
    pub fn new() -> Self {
        Self::default()
    }
    /// reset to empty or zero
    pub fn reset(&mut self) {
        for x in &mut self.v {
            x.agg.borrow_mut().reset();
        }
    }
    /// is empty?
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    /// Column,Agg,Pattern
    pub fn push_replace(&mut self, spec: &str) -> Result<()> {
        self.push(AggCol::new_replace(spec)?)
    }
    /// OutName,Column,Agg,Pattern
    pub fn push_prefix(&mut self, spec: &str) -> Result<()> {
        self.push(AggCol::new_prefix(spec)?)
    }
    /// OutName,Column,Agg,Pattern
    pub fn push_first_prefix(&mut self, spec: &str) -> Result<()> {
        self.v.insert(0, AggCol::new_prefix(spec)?);
	Ok(())
    }
    /// OutName,Column,Agg,Pattern
    pub fn push_append(&mut self, spec: &str) -> Result<()> {
        self.push(AggCol::new_append(spec)?)
    }
    /// add item
    pub fn push(&mut self, item: AggCol) -> Result<()> {
        self.v.push(item);
        Ok(()) // fail if replace and duplicate column
    }
    /// update all aggregators with new data
    pub fn add(&mut self, data: &TextLine) {
        for x in &mut self.v {
            x.agg.borrow_mut().add(data.get(x.src.num));
        }
    }
    /// resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in &mut self.v {
            x.src.lookup(fieldnames)?;
        }
        Ok(())
    }
    /// fill Writer with aggregators
    pub fn fill(&self, w: &mut Writer, head: &StringLine) {
        for x in &self.v {
            if let AggType::Prefix(_s) = &x.mode {
                w.push(Box::new(x.clone()));
            }
        }
        'outer: for (i, x) in head.iter().enumerate() {
            for y in &self.v {
                if let AggType::Replace = &y.mode {
                    if y.src.num == i {
                        w.push(Box::new(y.clone()));
                        continue 'outer;
                    }
                }
            }
            w.push(Box::new(ColumnSingle::with_name(x).unwrap()));
        }
        for x in &self.v {
            if let AggType::Append(_s) = &x.mode {
                w.push(Box::new(x.clone()));
            }
        }
    }
}

#[derive(Debug)]
struct Merge {
    // input sub-delimiter
    delim : u8,
    // output sub-delimiter
    out_delim : u8,
    // output only this many parts
    max_parts : usize,
    // Comparator for Min and Max
    comp: Comp,
    // Sort the parts first
    do_sort : bool,
    // remove adjacent duplicates (according to Comp). Happens after sort.
    do_uniq : bool,
    // do not write parts shorter than this
    min_len : usize,
    // do not write parts longer than this
    max_len : usize,
    // write the number of parts, rather than the parts themselves
    do_count : bool,
    // internal temp space
    data : TextLine,
}

impl Default for Merge {
    fn default() -> Self {
	Self {
	    delim: b',',
	    out_delim: b',',
	    max_parts: usize::MAX,
	    comp: Comp::default(),
	    do_sort : false,
	    do_uniq : false,
	    min_len : 0,
	    max_len : usize::MAX,
	    do_count : false,
	    data : TextLine::default()
	}
    }
}

impl Merge {
    fn new(spec : &str) -> Result<Self> {
	let mut m = Self::default();
	let mut sp = spec;
	loop {
	    if sp.is_empty() {
		break;
	    }
	    if sp.starts_with("comp:") {
		m.add_one(sp)?;
		break;
	    }
	    if sp.as_bytes()[0] == b'D' {
		if sp.len() < 3 {
		    return err!("Merge Delim Spec must be three bytes, e.g. 'D.,'");
		}
		m.add_one(&sp[0..3])?;
		sp = &sp[3..];
		if !sp.is_empty() {
		    if sp.as_bytes()[0] != b'.' {
			return err!("Merge Delim Spec must be three bytes, e.g. 'D.,'");
		    }
		    sp = &sp[1..];
		}
	    }
	    if let Some((a, b)) = sp.split_once('.') {
		m.add_one(a)?;
		sp = b;
	    }
	    else {
		m.add_one(sp)?;
		break;
	    }
	}
	Ok(m)
    }
    fn add_one(&mut self, spec : &str) -> Result<()> {
	if spec.is_empty() {
	    return err!("Invalid empty Merge Part");
	}
	else if spec.as_bytes()[0] == b'D' {
	    if spec.len() != 3 {
		return err!("Merge Delim Spec must be three bytes, e.g. 'D.,'");
	    }
	    self.delim = spec.as_bytes()[1];
	    self.out_delim = spec.as_bytes()[2];
	}
	else if spec.eq_ignore_ascii_case("sort") {
	    self.do_sort = true;
	}
	else if spec.eq_ignore_ascii_case("uniq") {
	    self.do_uniq = true;
	}
	else if spec.eq_ignore_ascii_case("count") {
	    self.do_count = true;
	}
	else if let Some(val) = spec.strip_prefix("comp:") {
	    self.comp = CompMaker::make_comp(val)?;
	}
	else if let Some(val) = spec.strip_prefix("min_len:") {
	    self.min_len = val.parse::<usize>()?;
	}
	else if let Some(val) = spec.strip_prefix("max_len:") {
	    self.max_len = val.parse::<usize>()?;
	}
	else if let Some(val) = spec.strip_prefix("max_parts:") {
	    self.max_parts = val.parse::<usize>()?;
	}
	else {
	    return err!("Unrecognized Merge Part '{}'", spec);
	}
	Ok(())
    }
}

impl Agg for Merge {
    fn add(&mut self, data: &[u8]) {
	if !data.is_empty() {
	    if !self.data.line.is_empty() {
		self.data.line.push(self.delim);
	    }
	    self.data.line.extend_from_slice(data);
	}
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
	self.data.split(self.delim);
	if self.do_sort {
	    self.data.parts.sort_by(|a, b| self.comp.comp(a.get(&self.data.line), b.get(&self.data.line)));
	}
	if self.do_uniq {
            self.data.parts.dedup_by(|a, b| self.comp.equal(a.get(&self.data.line), b.get(&self.data.line)));
	}
	if self.do_count {
	    write!(w, "{}", self.data.parts.len())?;
	}
	else {
	    let mut num_written = 0;
	    for x in &self.data.parts {
		if x.len() >= self.min_len && x.len() <= self.max_len {
		    if num_written > 0 {
			w.write_all(&[self.out_delim])?;
		    }
		    w.write_all(x.get(&self.data.line))?;
		    num_written += 1;
		    if num_written >= self.max_parts {
			break;
		    }
		}
	    }
	}
	Ok(())
    }
    fn reset(&mut self) {
	self.data.line.clear();
    }
}

struct Min {
    comp: Comp,
    val: Vec<u8>,
    empty: bool,
}

impl Min {
    fn new(spec: &str) -> Result<Self> {
        Ok(Self {
            comp: CompMaker::make_comp(spec)?,
            val: Vec::new(),
            empty: true,
        })
    }
}

impl Agg for Min {
    fn add(&mut self, data: &[u8]) {
        if self.empty {
            self.empty = false;
            self.val.extend_from_slice(data);
            return;
        }
        if self.comp.comp.comp(&self.val, data) == Ordering::Greater {
            self.val.clear();
            self.val.extend_from_slice(data);
        }
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        w.write_all(&self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val.clear();
        self.empty = true;
    }
}

struct Mean {
    val: f64,
    cnt: f64,
}

impl Mean {
    fn new(spec: &str) -> Result<Self> {
        if spec.is_empty() {
            Ok(Self { val: 0.0, cnt: 0.0 })
        } else {
            err!("Unexpected pattern with 'Mean' aggregator : '{}'", spec)
        }
    }
}

impl Agg for Mean {
    fn add(&mut self, data: &[u8]) {
        self.val += str_to_d_lossy(data);
        self.cnt += 1.0;
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        write!(w, "{}", self.val / self.cnt)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val = 0.0;
        self.cnt = 0.0;
    }
}

struct Sum {
    val: f64,
}

impl Sum {
    fn new(spec: &str) -> Result<Self> {
        if spec.is_empty() {
            Ok(Self { val: 0.0 })
        } else {
            err!("Unexpected pattern with 'Sum' aggregator : '{}'", spec)
        }
    }
}

impl Agg for Sum {
    fn add(&mut self, data: &[u8]) {
        self.val += str_to_d_lossy(data);
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        write!(w, "{}", self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val = 0.0;
    }
}

struct Max {
    comp: Comp,
    val: Vec<u8>,
}

impl Max {
    fn new(spec: &str) -> Result<Self> {
        Ok(Self {
            comp: CompMaker::make_comp(spec)?,
            val: Vec::new(),
        })
    }
}

impl Agg for Max {
    fn add(&mut self, data: &[u8]) {
        if self.comp.comp.comp(&self.val, data) == Ordering::Less {
            self.val.clear();
            self.val.extend_from_slice(data);
        }
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        w.write_all(&self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val.clear();
    }
}

struct Prefix {
    val: Vec<u8>,
    empty: bool,
}

impl Prefix {
    fn new(spec: &str) -> Result<Self> {
	if spec.is_empty() {
	    Ok(Self{val : Vec::new(), empty : true})
	}
	else {
	    err!("Unexpected non-empty pattern passed to Prefix agregator : '{}'", spec)
	}
    }
}

fn common_prefix(val : &mut Vec<u8>, data : &[u8]) {
    if val.len() > data.len() {
	val.truncate(data.len());
    }
    #[allow(clippy::needless_range_loop)] // cleaner this way
    for x in 0..val.len() {
	if val[x] != data[x] {
	    val.truncate(x);		    
	    break;
	}
    }
}

impl Agg for Prefix {
    fn add(&mut self, data: &[u8]) {
	if self.empty {
	    self.empty = false;
            self.val.extend_from_slice(data);
	}
	else {
	    common_prefix(&mut self.val, data);
	}
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        w.write_all(&self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val.clear();
	self.empty = true;
    }
}

struct Suffix {
    val: Vec<u8>,
    empty: bool,
}

impl Suffix {
    fn new(spec: &str) -> Result<Self> {
	if spec.is_empty() {
	    Ok(Self{val : Vec::new(), empty : true})
	}
	else {
	    err!("Unexpected non-empty pattern passed to Sufffix agregator : '{}'", spec)
	}
    }
}

fn common_suffix(val : &mut Vec<u8>, data : &[u8]) {
    let mut ok_bytes = 0;
    for x in val.iter().rev().zip(data.iter().rev()) {
	if x.0 == x.1 {
	    ok_bytes += 1;
	}
	else {
	    break;
	}
    }
    if ok_bytes < val.len() {
	val.drain(0..(val.len()-ok_bytes));
    }
}

impl Agg for Suffix {
    fn add(&mut self, data: &[u8]) {
	if self.empty {
	    self.empty = false;
            self.val.extend_from_slice(data);
	}
	else {
	    common_suffix(&mut self.val, data);
	}
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
        w.write_all(&self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val.clear();
	self.empty = true;
    }
}

struct Count {
    val: usize
}

impl Count {
    fn new(spec: &str) -> Result<Self> {
	if spec.is_empty() {
	    Ok(Self{val:0})
	}
	else {
	    err!("Unexpected pattern passed to Count aggregator : '{}'", spec)
	}
    }
}

impl Agg for Count {
    fn add(&mut self, _data: &[u8]) {
	self.val += 1;
    }
    fn result(&mut self, w: &mut dyn Write) -> Result<()> {
	write!(w, "{}", self.val)?;
        Ok(())
    }
    fn reset(&mut self) {
        self.val = 0;
    }
}

type MakerBox = Box<dyn Fn(&str) -> Result<Rc<RefCell<dyn Agg>>> + Send>;
/// A named constructor for a [Agg], used by [AggMaker]
struct AggMakerItem {
    /// name or Agg
    tag: &'static str,
    /// what this Agg does
    help: &'static str,
    /// Create a dyn Agg from a pattern
    maker: MakerBox,
}

struct AggMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

lazy_static! {
    static ref AGG_MAKER: Mutex<Vec<AggMakerItem>> = Mutex::new(Vec::new());
    static ref AGG_ALIAS: Mutex<Vec<AggMakerAlias>> = Mutex::new(Vec::new());
    static ref MODIFIERS: Vec<&'static str> = vec![];
}

/// Makes an [Agg]
#[derive(Debug, Clone, Default)]
pub struct AggMaker {}

impl AggMaker {
    fn init() -> Result<()> {
        if !AGG_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("min", "minimum")?;
        Self::do_add_alias("max", "maximum")?;
        Self::do_add_alias("mean", "avg")?;
        Self::do_push(
            "merge",
            "Merge into a delimited list of values",
            |p| Ok(Rc::new(RefCell::new(Merge::new(p)?))),
        )?;
        Self::do_push(
            "count",
            "The number of things aggregated",
            |p| Ok(Rc::new(RefCell::new(Count::new(p)?))),
        )?;
        Self::do_push(
            "prefix",
            "The longest common prefix of the input values",
            |p| Ok(Rc::new(RefCell::new(Prefix::new(p)?))),
        )?;
        Self::do_push(
            "suffix",
            "The longest common suffix of the input values",
            |p| Ok(Rc::new(RefCell::new(Suffix::new(p)?))),
        )?;
        Self::do_push(
            "min",
            "Keep the minimum value, Pattern is the associated Comparator",
            |p| Ok(Rc::new(RefCell::new(Min::new(p)?))),
        )?;
        Self::do_push(
            "max",
            "Keep the maximum value, Pattern is the associated Comparator",
            |p| Ok(Rc::new(RefCell::new(Max::new(p)?))),
        )?;
        Self::do_push("mean", "The arithmetic mean of the values", |p| {
            Ok(Rc::new(RefCell::new(Mean::new(p)?)))
        })?;
        Self::do_push("sum", "The sum of the values", |p| {
            Ok(Rc::new(RefCell::new(Sum::new(p)?)))
        })?;
        Ok(())
    }
    /// Add a new Agg. If an Agg already exists by that name, replace it.
    pub fn push<F: 'static>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&str) -> Result<Rc<RefCell<dyn Agg>>> + Send,
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
    fn resolve_alias(name: &str) -> &str {
        let mut mm = AGG_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == name {
                return x.old_name;
            }
        }
        name
    }
    fn do_add_alias(old_name: &'static str, new_name: &'static str) -> Result<()> {
        if MODIFIERS.contains(&new_name) {
            return err!("You can't add an alias named {} because that is reserved for a modifier");
        }
        let m = AggMakerAlias { old_name, new_name };
        let mut mm = AGG_ALIAS.lock().unwrap();
        for x in mm.iter_mut() {
            if x.new_name == m.new_name {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        Ok(())
    }
    fn do_push<F: 'static>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&str) -> Result<Rc<RefCell<dyn Agg>>> + Send,
    {
        if MODIFIERS.contains(&tag) {
            return err!("You can't add a agg named {} because that is reserved for a modifier");
        }
        let m = AggMakerItem {
            tag,
            help,
            maker: Box::new(maker),
        };
        let mut mm = AGG_MAKER.lock().unwrap();
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
        //        println!("Modifers :");
        Self::init().unwrap();
        println!("Methods :");
        Self::init().unwrap();
        let mm = AGG_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
        }
	println!();
	println!("'merge' pattern is a period delimited set of any of the following :");
	println!("sort        --  sort the parts");
	println!("uniq        -- Remove any adjacent equal parts. Happens after sorting.");
	println!("count       -- Display the count of parts, not the parts themselves.");
	println!("max_len:N   -- Discard any parts longer than N bytes.");
	println!("min_len:N   -- Discard any parts shorter than N bytes.");
	println!("max_parts:N -- Display only the first N parts.");
	println!("Dxy         -- 'x' is the input delimiter and 'y' is the output delimiter. Default is comma for both.");
	println!("comp:spec   -- Spec for comparator for sort or uniq. Must be last piece.");
	println!();
        println!("See also https://avjewe.github.io/cdxdoc/Aggregator.html.");
    }
    /// Create a Agg from a name and a pattern
    pub fn make2(spec: &str, pattern: &str) -> Result<AggRef> {
        Self::init()?;
        let mut name = "";
        if !spec.is_empty() {
            for x in spec.split('.') {
                //                if x.eq_ignore_ascii_case("utf8") {
                //                } else {
                name = x;
            }
            name = Self::resolve_alias(name);
        }
        let mm = AGG_MAKER.lock().unwrap();
        for x in &*mm {
            if x.tag == name {
                return (x.maker)(pattern);
            }
        }
        err!("No Agg found with name '{}'", name)
    }
    /// Create a matcher from a full spec, i.e. "Agg,Pattern"
    pub fn make(spec: &str) -> Result<AggRef> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::make2(a, b)
        } else {
            Self::make2(spec, "")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn suffix() {
	let mut v = b"123456789".to_vec();
	common_suffix(&mut v,  b"123456789");
	assert_eq!(&v, b"123456789");
	common_suffix(&mut v,  b"23456789");
	assert_eq!(&v, b"23456789");
	common_suffix(&mut v,  b"dsjfldkjsfaslkjfaslkfjadflkj3456789");
	assert_eq!(&v, b"3456789");
	common_suffix(&mut v,  b"");
	assert_eq!(&v, b"");
	common_suffix(&mut v,  b"");
	assert_eq!(&v, b"");
	common_suffix(&mut v,  b"sadfddsafasdgfg");
	assert_eq!(&v, b"");
    }
    #[test]
    fn prefix() {
	let mut v = b"123456789".to_vec();
	common_prefix(&mut v,  b"123456789");
	assert_eq!(&v, b"123456789");	
	common_prefix(&mut v,  b"12345678");
	assert_eq!(&v, b"12345678");	
	common_prefix(&mut v,  b"1234567sdfhasflhasflasflaksjfasdkhj");
	assert_eq!(&v, b"1234567");	
	common_prefix(&mut v,  b"sdkjadflkjafdakjdsf");
	assert_eq!(&v, b"");	
	common_prefix(&mut v,  b"sdkjadflkjafdakjdsf");
	assert_eq!(&v, b"");	
	common_prefix(&mut v,  b"");
	assert_eq!(&v, b"");	
    }
}
