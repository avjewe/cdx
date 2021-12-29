//! Aggregate Values

use crate::column::{ColumnFun, ColumnHeader, ColumnSingle, NamedCol, Writer};
use crate::comp::{Comp, CompMaker};
use crate::{err, Error, Result, StringLine, TextLine};
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
    fn result(&self, w: &mut dyn Write) -> Result<()>;
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
        self.agg.borrow().result(w)
    }
    /// resolve any named columns
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.src.lookup(fieldnames)
    }
}

/// A list of AggCol
#[derive(Clone, Default, Debug)]
pub struct AggColList {
    /// The aggregators
    pub v: Vec<AggCol>,
}

impl AggColList {
    /// new
    pub fn new() -> Self {
        Self::default()
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

struct Min {
    comp: Comp,
    val: Vec<u8>,
}

impl Min {
    fn new(spec: &str) -> Result<Self> {
        Ok(Self {
            comp: CompMaker::make_comp(spec)?,
            val: Vec::new(),
        })
    }
}

impl Agg for Min {
    fn add(&mut self, data: &[u8]) {
        if self.comp.comp.comp(&self.val, data) == Ordering::Greater {
            self.val.clear();
            self.val.extend_from_slice(data);
        }
    }
    fn result(&self, w: &mut dyn Write) -> Result<()> {
        w.write_all(&self.val)?;
        Ok(())
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

/// Makes a [Agger]
#[derive(Debug, Clone, Default)]
pub struct AggMaker {}

impl AggMaker {
    fn init() -> Result<()> {
        if !AGG_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("min", "minimum")?;
        Self::do_push(
            "min",
            "Keep the minimum value, as defined by a Comparator",
            |p| Ok(Rc::new(RefCell::new(Min::new(p)?))),
        )?;
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
        println!("Methods :");
        Self::init().unwrap();
        let mm = AGG_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
        }
        println!("See also https://avjewe.github.io/cdxdoc/Aggregator.html.");
    }
    /// Create a Agg from a name and a pattern
    pub fn make2(spec: &str, pattern: &str) -> Result<AggRef> {
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
