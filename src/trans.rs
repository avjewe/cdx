//! Transforms that turn values into other values

use crate::text::Text;
use crate::util::{err, Error, FakeSlice, Result, TextLine};
use lazy_static::lazy_static;
use std::fmt;
use std::sync::Mutex;

//use crate::column::{NamedCol};

/// Transform a value to another value, in the context of a (typically unused) TextLine
pub trait Trans {
    /// Transform `src` into a new value, and append to `dst`
    fn trans(&mut self, src: &[u8], cont: &TextLine, dst: &mut Vec<u8>) -> Result<()>;
    /// Resolve any named columns, typically needed only if TextLine is used in `trans`
    fn lookup(&mut self, _fieldnames: &[&str]) -> Result<()> {
        Ok(())
    }
}

struct BytesTrans {
    v: Vec<FakeSlice>,
}
impl BytesTrans {
    fn new(spec: &str) -> Result<Self> {
        let mut v = Vec::new();
        for x in spec.split(',') {
            v.push(FakeSlice::new(x)?);
        }
        Ok(Self { v })
    }
}
impl Trans for BytesTrans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        for x in &self.v {
            dst.extend(x.get_safe(src));
        }
        Ok(())
    }
}
struct Base64Trans {
    encode: bool,
    config: base64::Config,
}
impl Base64Trans {
    fn get_config(x: &str) -> Result<base64::Config> {
        if x.eq_ignore_ascii_case("std") || x.eq_ignore_ascii_case("standard") || x.is_empty() {
            Ok(base64::STANDARD)
        } else if x.eq_ignore_ascii_case("std_np") || x.eq_ignore_ascii_case("standard_np") {
            Ok(base64::STANDARD_NO_PAD)
        } else if x.eq_ignore_ascii_case("bcrypt") {
            Ok(base64::BCRYPT)
        } else if x.eq_ignore_ascii_case("binhex") {
            Ok(base64::BINHEX)
        } else if x.eq_ignore_ascii_case("crypt") {
            Ok(base64::CRYPT)
        } else if x.eq_ignore_ascii_case("imap") {
            Ok(base64::IMAP_MUTF7)
        } else if x.eq_ignore_ascii_case("url") {
            Ok(base64::URL_SAFE)
        } else if x.eq_ignore_ascii_case("url_np") {
            Ok(base64::URL_SAFE_NO_PAD)
        } else {
            err!("Unrecognized base64 config, must be one of : standard, standard_np, bcrypt, binhex, crypt, imap, url, or url_np.\nSee https://docs.rs/base64/ for details.")
        }
    }
    fn new(encode: bool, config: &str) -> Result<Self> {
        Ok(Self {
            encode,
            config: Self::get_config(config)?,
        })
    }
}
impl Trans for Base64Trans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        if self.encode {
            dst.resize(src.len() * 4 / 3 + 4, 0);
            let len = base64::encode_config_slice(src, self.config, dst);
            dst.resize(len, 0);
        } else {
            dst.resize((src.len() + 3) / 4 * 3, 0);
            let len = base64::decode_config_slice(src, self.config, dst)?;
            dst.resize(len, 0);
        }
        Ok(())
    }
}

struct LowerTrans {}
impl Trans for LowerTrans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        for x in src {
            dst.push(x.to_ascii_lowercase());
        }
        Ok(())
    }
}

#[derive(Default)]
struct LowerUtfTrans {
    tmp: String,
}
impl Trans for LowerUtfTrans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        String::from_utf8_lossy(src)
            .as_ref()
            .assign_lower(&mut self.tmp);
        dst.extend(self.tmp.as_bytes());
        Ok(())
    }
}
#[derive(Default)]
struct UpperUtfTrans {
    tmp: String,
}
impl Trans for UpperUtfTrans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        String::from_utf8_lossy(src)
            .as_ref()
            .assign_upper(&mut self.tmp);
        dst.extend(self.tmp.as_bytes());
        Ok(())
    }
}

struct UpperTrans {}
impl Trans for UpperTrans {
    fn trans(&mut self, src: &[u8], _cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        for x in src {
            dst.push(x.to_ascii_uppercase());
        }
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone)]
/// Transform Modifiers
pub struct TransSettings {
    utf8: bool,
}

/// A Trans with some context
pub struct Transform {
    /// original spec
    pub spec: String,
    /// utf8 (vs ascii) version if available
    pub conf: TransSettings,
    /// The Trans
    pub trans: Box<dyn Trans>,
}
impl fmt::Debug for Transform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Transform {}", self.spec)
    }
}
impl Clone for Transform {
    fn clone(&self) -> Self {
        TransMaker::make(&self.spec).unwrap()
    }
}

impl Transform {
    fn trans(&mut self, src: &[u8], cont: &TextLine, dst: &mut Vec<u8>) -> Result<()> {
        self.trans.trans(src, cont, dst)
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        self.trans.lookup(fieldnames)
    }
}

#[derive(Default, Clone, Debug)]
/// a chain of transforms
pub struct TransList {
    v: Vec<Transform>,
    tmp1: Vec<u8>,
    tmp2: Vec<u8>,
}

impl TransList {
    /// new
    pub fn new(spec: &str) -> Result<Self> {
        let mut s = Self::default();
        for x in spec.split('+') {
            s.push(x)?;
        }
        Ok(s)
    }
    /// add new trans
    pub fn push(&mut self, spec: &str) -> Result<()> {
        self.v.push(TransMaker::make(spec)?);
        Ok(())
    }
    /// Transform `src` into a new value, and append to `dst`
    pub fn trans(&mut self, src: &[u8], cont: &TextLine) -> Result<&[u8]> {
        let mut use_1 = true;
        for (i, x) in self.v.iter_mut().enumerate() {
            if i == 0 {
                self.tmp1.clear();
                x.trans(src, cont, &mut self.tmp1)?;
            } else if i % 2 == 0 {
                self.tmp1.clear();
                x.trans(&self.tmp2, cont, &mut self.tmp1)?;
                use_1 = true;
            } else {
                self.tmp2.clear();
                x.trans(&self.tmp1, cont, &mut self.tmp2)?;
                use_1 = false;
            }
        }
        if use_1 {
            Ok(&self.tmp1)
        } else {
            Ok(&self.tmp2)
        }
    }

    /// Resolve any named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        for x in &mut self.v {
            x.lookup(fieldnames)?;
        }
        Ok(())
    }
}

type MakerBox = Box<dyn Fn(&TransSettings, &str) -> Result<Box<dyn Trans>> + Send>;
/// A named constructor for a [Trans], used by [TransMaker]
struct TransMakerItem {
    /// name of Trans
    tag: &'static str,
    /// what this matcher does
    help: &'static str,
    /// Create a dyn Trans from a pattern
    maker: MakerBox,
}

struct TransMakerAlias {
    old_name: &'static str,
    new_name: &'static str,
}

lazy_static! {
    static ref TRANS_MAKER: Mutex<Vec<TransMakerItem>> = Mutex::new(Vec::new());
    static ref TRANS_ALIAS: Mutex<Vec<TransMakerAlias>> = Mutex::new(Vec::new());
    static ref MODIFIERS: Vec<&'static str> = vec!["utf8"];
}

/// Makes a [Trans]
#[derive(Debug, Clone, Default)]
pub struct TransMaker {}

impl TransMaker {
    /// add standard trans makers
    fn init() -> Result<()> {
        if !TRANS_MAKER.lock().unwrap().is_empty() {
            return Ok(());
        }
        Self::do_add_alias("lower", "lowercase")?;
        Self::do_add_alias("upper", "uppercase")?;
        Self::do_push("lower", "make lower case", |c, _p| {
            if c.utf8 {
                Ok(Box::new(LowerUtfTrans::default()))
            } else {
                Ok(Box::new(LowerTrans {}))
            }
        })?;
        Self::do_push("upper", "make upper case", |c, _p| {
            if c.utf8 {
                Ok(Box::new(UpperUtfTrans::default()))
            } else {
                Ok(Box::new(UpperTrans {}))
            }
        })?;
        Self::do_push("from_base64", "decode base64 encoding", |_c, p| {
            Ok(Box::new(Base64Trans::new(false, p)?))
        })?;
        Self::do_push("to_base64", "encode base64 encoding", |_c, p| {
            Ok(Box::new(Base64Trans::new(true, p)?))
        })?;
        Self::do_push("bytes", "Select bytes from value", |_c, p| {
            Ok(Box::new(BytesTrans::new(p)?))
        })?;
        Ok(())
    }
    /// Add a new trans. If a Trans already exists by that name, replace it.
    pub fn push<F: 'static>(tag: &'static str, help: &'static str, maker: F) -> Result<()>
    where
        F: Fn(&TransSettings, &str) -> Result<Box<dyn Trans>> + Send,
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
        let mut mm = TRANS_ALIAS.lock().unwrap();
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
        let m = TransMakerAlias { old_name, new_name };
        let mut mm = TRANS_ALIAS.lock().unwrap();
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
        F: Fn(&TransSettings, &str) -> Result<Box<dyn Trans>> + Send,
    {
        if MODIFIERS.contains(&tag) {
            return err!("You can't add a trans named {} because that is reserved for a modifier");
        }
        let m = TransMakerItem {
            tag,
            help,
            maker: Box::new(maker),
        };
        let mut mm = TRANS_MAKER.lock().unwrap();
        for x in mm.iter_mut() {
            if x.tag == m.tag {
                *x = m;
                return Ok(());
            }
        }
        mm.push(m);
        Ok(())
    }
    /// Print all available Transs to stdout.
    pub fn help() {
        println!("Modifers :");
        Self::init().unwrap();
        let mm = TRANS_MAKER.lock().unwrap();
        for x in &*mm {
            println!("{:12}{}", x.tag, x.help);
        }
        println!("See also https://avjewe.github.io/cdxdoc/Trans.html.");
    }
    /// Create a Trans from a trans spec and a pattern
    pub fn make2(trans: &str, pattern: &str) -> Result<Transform> {
        let mut spec = trans.to_string();
        if !pattern.is_empty() {
            spec.push(',');
            spec.push_str(pattern);
        }
        let mut conf = TransSettings::default();
        let mut kind = "";
        if !trans.is_empty() {
            for x in trans.split('.') {
                if x.eq_ignore_ascii_case("utf8") {
                    conf.utf8 = true;
                } else {
                    kind = x;
                }
            }
            kind = Self::resolve_alias(kind);
        }
        Ok(Transform {
            spec,
            conf,
            trans: Self::make_box(kind, &conf, pattern)?,
        })
    }
    /// make a dyn Trans from a named trans and a pattern
    pub fn make_box(kind: &str, conf: &TransSettings, pattern: &str) -> Result<Box<dyn Trans>> {
        Self::init()?;
        let mm = TRANS_MAKER.lock().unwrap();
        for x in &*mm {
            if x.tag == kind {
                return (x.maker)(conf, pattern);
            }
        }
        err!("Unknown trans : {}", kind)
    }

    /// Create a trans from a full spec, i.e. "Trans,Pattern"
    pub fn make(spec: &str) -> Result<Transform> {
        if let Some((a, b)) = spec.split_once(',') {
            Self::make2(a, b)
        } else {
            Self::make2(spec, "")
        }
    }
}
