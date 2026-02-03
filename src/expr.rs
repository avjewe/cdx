//! Floating Point expressions

use crate::column::get_col;
use crate::prelude::*;
use crate::shunting_yard::to_rpn;
use crate::tok2::{BinaryOp, Token, UnaryOp, tokenize};
use crate::util::find_close;
use regex::Regex;
use std::f64::consts;
use std::fmt::Write;
use std::sync::LazyLock;

/// evaluate a constant arithmetic expression
pub fn calc(expr: &str) -> Result<f64> {
    let mut c = Expr::new(expr)?;
    c.eval_plain()
}

/// If input is 'Format,Expression' return that format and expression
/// else return supplied default format, and whole input slice
#[must_use]
pub fn parse_fmt_expr(dflt: NumFormat, spec: &str) -> (NumFormat, &str) {
    if let Some((a, b)) = spec.split_once(',') {
        let x = NumFormat::new(a);
        if let Ok(f) = x { (f, b) } else { (dflt, spec) }
    } else {
        (dflt, spec)
    }
}

fn find_const(x: &str) -> Option<f64> {
    for c in &CONSTS {
        if x == c.name {
            return Some(c.val);
        }
    }
    None
}

fn find_func(x: &str) -> Result<FuncOp> {
    for f in &FUNCS {
        if f.name == x {
            return Ok(f.val);
        }
    }
    err!("No such function as {}", x)
}

#[derive(Debug, Copy, Clone)]
enum FuncOp {
    Acos,
    Acosh,
    Asin,
    Asinh,
    Atan,
    Atan2,
    Atanh,
    Cbrt,
    Ceil,
    Copysign,
    Cos,
    Cosh,
    Erf,
    Exp,
    Exp2,
    Exp10,
    Expm1,
    Fabs,
    Fdim,
    Floor,
    Fma,
    Fmax,
    Fmin,
    Fmod,
    Hypot,
    J0,
    J1,
    Jn,
    Ldexp,
    Lgamma,
    Log,
    Log1p,
    Log2,
    Log10,
    Nextafter,
    Pow,
    Remainder,
    Round,
    Scalbn,
    Sin,
    Sinh,
    Sqrt,
    Tan,
    Tanh,
    Tgamma,
    Trunc,
    Y0,
    Y1,
    Yn,
    Abs,
    If,
    Max,
    Min,
    Avg,
}

#[derive(Debug, Copy, Clone)]
struct ConstDef {
    name: &'static str,
    val: f64,
}

macro_rules! con {
    ($a:expr,$b:expr) => {
        ConstDef { name: $a, val: $b }
    };
}

const CONSTS: [ConstDef; 20] = [
    con!("pi", consts::PI),
    con!("e", consts::E),
    con!("M_E", consts::E),
    con!("M_LOG2E", consts::LOG2_E),
    con!("M_LOG10E", consts::LOG10_E),
    con!("M_LN2", consts::LN_2),
    con!("M_LN10", consts::LN_10),
    con!("M_PI", consts::PI),
    con!("M_PI_2", consts::FRAC_PI_2),
    con!("M_PI_4", consts::FRAC_PI_4),
    con!("M_1_PI", consts::FRAC_1_PI),
    con!("M_2_PI", consts::FRAC_2_PI),
    con!("M_2_SQRTPI", consts::FRAC_2_SQRT_PI),
    con!("M_SQRT2", consts::SQRT_2),
    con!("M_SQRT1_2", consts::FRAC_1_SQRT_2),
    con!("M_INF", f64::INFINITY),
    con!("M_NEG_INF", f64::NEG_INFINITY),
    con!("M_MAX", f64::MAX),
    con!("M_MIN", f64::MIN),
    con!("M_MIN_POS", f64::MIN_POSITIVE),
];

/// write all the constants to stdout
pub fn show_const() {
    for (i, x) in CONSTS.iter().enumerate() {
        print!("{:12}", x.name);
        if i % 4 == 3 {
            println!();
        }
    }
}

/// write all the constants to stdout
pub fn show_func() {
    for (i, x) in FUNCS.iter().enumerate() {
        print!("{:12}", x.name);
        if i % 6 == 5 {
            println!();
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct FuncDef {
    name: &'static str,
    val: FuncOp,
}

macro_rules! fun {
    ($a:expr,$b:expr) => {
        FuncDef { name: $a, val: $b }
    };
}

const FUNCS: [FuncDef; 54] = [
    fun!("acos", FuncOp::Acos),
    fun!("acosh", FuncOp::Acosh),
    fun!("asin", FuncOp::Asin),
    fun!("asinh", FuncOp::Asinh),
    fun!("atan", FuncOp::Atan),
    fun!("atan2", FuncOp::Atan2),
    fun!("atanh", FuncOp::Atanh),
    fun!("cbrt", FuncOp::Cbrt),
    fun!("ceil", FuncOp::Ceil),
    fun!("copysign", FuncOp::Copysign),
    fun!("cos", FuncOp::Cos),
    fun!("cosh", FuncOp::Cosh),
    fun!("erf", FuncOp::Erf),
    fun!("exp", FuncOp::Exp),
    fun!("exp2", FuncOp::Exp2),
    fun!("exp10", FuncOp::Exp10),
    fun!("expm1", FuncOp::Expm1),
    fun!("fabs", FuncOp::Fabs),
    fun!("fdim", FuncOp::Fdim),
    fun!("floor", FuncOp::Floor),
    fun!("fma", FuncOp::Fma),
    fun!("fmax", FuncOp::Fmax),
    fun!("fmin", FuncOp::Fmin),
    fun!("fmod", FuncOp::Fmod),
    fun!("hypot", FuncOp::Hypot),
    fun!("j0", FuncOp::J0),
    fun!("j1", FuncOp::J1),
    fun!("jn", FuncOp::Jn),
    fun!("ldexp", FuncOp::Ldexp),
    fun!("lgamma", FuncOp::Lgamma),
    fun!("log", FuncOp::Log),
    fun!("log1p", FuncOp::Log1p),
    fun!("log2", FuncOp::Log2),
    fun!("log10", FuncOp::Log10),
    fun!("nextafter", FuncOp::Nextafter),
    fun!("pow", FuncOp::Pow),
    fun!("remainder", FuncOp::Remainder),
    fun!("round", FuncOp::Round),
    fun!("scalbn", FuncOp::Scalbn),
    fun!("sin", FuncOp::Sin),
    fun!("sinh", FuncOp::Sinh),
    fun!("sqrt", FuncOp::Sqrt),
    fun!("tan", FuncOp::Tan),
    fun!("tanh", FuncOp::Tanh),
    fun!("tgamma", FuncOp::Tgamma),
    fun!("trunc", FuncOp::Trunc),
    fun!("y0", FuncOp::Y0),
    fun!("y1", FuncOp::Y1),
    fun!("yn", FuncOp::Yn),
    fun!("abs", FuncOp::Abs),
    fun!("if", FuncOp::If),
    fun!("max", FuncOp::Max),
    fun!("min", FuncOp::Min),
    fun!("avg", FuncOp::Avg),
];

const fn min_args(f: FuncOp) -> usize {
    use FuncOp::{
        Atan2, Copysign, Fdim, Fma, Fmax, Fmin, Fmod, Hypot, If, Jn, Ldexp, Nextafter, Pow,
        Remainder, Scalbn, Yn,
    };
    match f {
        Atan2 | Copysign | Fmin | Fdim | Fmax | Fmod | Hypot | Jn | Ldexp | Nextafter
        | Remainder | Scalbn | Pow | Yn => 2,
        Fma | If => 3,
        _ => 1,
    }
}

const fn max_args(f: FuncOp) -> usize {
    use FuncOp::{
        Atan2, Avg, Copysign, Fdim, Fma, Fmax, Fmin, Fmod, Hypot, If, Jn, Ldexp, Max, Min,
        Nextafter, Pow, Remainder, Scalbn, Yn,
    };
    match f {
        Atan2 | Copysign | Fmin | Fdim | Fmax | Fmod | Hypot | Jn | Ldexp | Nextafter
        | Remainder | Scalbn | Pow | Yn => 2,
        Fma | If => 3,
        Min | Max | Avg => 0,
        _ => 1,
    }
}

#[derive(Debug, Copy, Clone)]
enum Node {
    Value(f64),
    Var(usize),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Func(FuncOp, usize),
}

#[derive(Default, Debug)]
struct VarMap {
    name: String,
    val: f64,
    col: Option<usize>, //  associated column number
}
impl VarMap {
    fn new(name: &str) -> Self {
        Self { name: name.to_string(), val: 0.0, col: None }
    }
}

#[derive(Default, Debug)]
/// A floating point expression
#[allow(clippy::struct_field_names)]
pub struct Expr {
    expr_str: String,
    expr: Vec<Node>,
    vars: Vec<VarMap>, // FIXME - Rc<RefCell<Vec<VarMap>>>, shared
    stack: Vec<f64>,   // temp space for eval
}

const fn to_f(x: bool) -> f64 {
    if x { 1.0 } else { 0.0 }
}

fn apply_unary(op: UnaryOp, x: f64) -> f64 {
    match op {
        UnaryOp::Plus => x,
        UnaryOp::Minus => -x,
        UnaryOp::Fact => factorial(x),
    }
}

#[allow(clippy::missing_asserts_for_indexing)]
#[allow(clippy::cast_precision_loss)]
fn apply_func(op: FuncOp, args: &[f64]) -> f64 {
    use FuncOp::{
        Abs, Acos, Acosh, Asin, Asinh, Atan, Atan2, Atanh, Avg, Cbrt, Ceil, Copysign, Cos, Cosh,
        Erf, Exp, Exp2, Exp10, Expm1, Fabs, Fdim, Floor, Fma, Fmax, Fmin, Fmod, Hypot, If, J0, J1,
        Jn, Ldexp, Lgamma, Log, Log1p, Log2, Log10, Max, Min, Nextafter, Pow, Remainder, Round,
        Scalbn, Sin, Sinh, Sqrt, Tan, Tanh, Tgamma, Trunc, Y0, Y1, Yn,
    };
    match op {
        Acos => args[0].abs(),
        Acosh => args[0].acosh(),
        Asinh => args[0].asinh(),
        Asin => args[0].asin(),
        Atan => args[0].atan(),
        Atan2 => args[0].atan2(args[1]),
        Atanh => args[0].atanh(),
        Cbrt => args[0].cbrt(),
        Ceil => args[0].ceil(),
        // clamp classify
        Copysign => args[0].copysign(args[1]),
        Cos => args[0].cos(),
        Cosh => args[0].cosh(),
        // div_euclid
        Erf => libm::erf(args[0]),
        Exp => args[0].exp(),
        Exp2 => args[0].exp2(),
        Exp10 => libm::exp10(args[0]),
        Expm1 => args[0].exp_m1(),
        Fabs => args[0].abs(),
        Fdim => libm::fdim(args[0], args[1]),
        Floor => args[0].floor(),
        // fract
        Fma => args[0].mul_add(args[1], args[2]),
        Fmax => args[0].max(args[1]),
        Fmin => args[0].min(args[1]),
        Fmod => libm::fmod(args[0], args[1]),
        Hypot => args[0].hypot(args[1]),
        // is_*
        J0 => libm::j0(args[0]),
        J1 => libm::j1(args[0]),
        Jn => libm::jn(args[0] as i32, args[1]),
        Ldexp => libm::ldexp(args[0], args[1] as i32),
        Lgamma => libm::lgamma(args[0]),
        // log(x, y)
        Log => args[0].ln(),
        Log1p => args[0].ln_1p(),
        Log2 => args[0].log2(),
        Log10 => args[0].log10(),
        Nextafter => libm::nextafter(args[0], args[1]),
        Pow => args[0].powf(args[1]),
        // recip
        // rem_euclid
        Remainder => libm::remainder(args[0], args[1]),
        Round => args[0].round(),
        Scalbn => libm::scalbn(args[0], args[1] as i32),
        // signum
        Sin => args[0].sin(),
        Sinh => args[0].sinh(),
        Sqrt => args[0].sqrt(),
        Tan => args[0].tan(),
        Tanh => args[0].tanh(),
        // to_degreees to_radians
        Tgamma => libm::tgamma(args[0]),
        Trunc => args[0].trunc(),
        Y0 => libm::y0(args[0]),
        Y1 => libm::y1(args[0]),
        Yn => libm::yn(args[0] as i32, args[1]),
        Abs => args[0].abs(),
        Max => {
            let mut v: f64 = args[0];
            for x in &args[1..] {
                if *x > v {
                    v = *x;
                }
            }
            v
        }
        Min => {
            let mut v: f64 = args[0];
            for x in &args[1..] {
                if *x < v {
                    v = *x;
                }
            }
            v
        }
        Avg => {
            let mut v: f64 = 0.0;
            for x in args {
                v += *x;
            }
            v / (args.len() as f64)
        }
        If => {
            if args[0] == 0.0 {
                args[2]
            } else {
                args[1]
            }
        }
    }
}

fn apply_binary(op: BinaryOp, left: f64, right: f64) -> f64 {
    match op {
        BinaryOp::Plus => left + right,
        BinaryOp::Minus => left - right,
        BinaryOp::Times => left * right,
        BinaryOp::Div => left / right,
        BinaryOp::Rem => left % right,
        BinaryOp::Pow => left.powf(right),
        BinaryOp::LT => to_f(left < right),
        BinaryOp::GT => to_f(left > right),
        BinaryOp::EQ => to_f(left == right),
        BinaryOp::NE => to_f(left != right),
        BinaryOp::LE => to_f(left <= right),
        BinaryOp::GE => to_f(left >= right),
    }
}

impl Expr {
    /// create Expr from expression
    pub fn new(expr: &str) -> Result<Self> {
        Ok(Self { expr_str: expr.to_string(), ..Self::default() })
    }
    /// which columns are in use
    pub fn used_cols(&self, v: &mut Vec<usize>) {
        for x in &self.vars {
            if let Some(c) = x.col {
                v.push(c);
            }
        }
    }
    /// original text string
    #[must_use]
    pub fn expr(&self) -> &str {
        &self.expr_str
    }
    fn parse(&mut self, exp: &str) -> Result<()> {
        let tokens = tokenize(exp)?;
        let rpn = to_rpn(&tokens)?;
        self.rpn_to_expr(&rpn)
    }
    fn parse_self(&mut self) -> Result<()> {
        let tokens = tokenize(&self.expr_str)?;
        let rpn = to_rpn(&tokens)?;
        self.rpn_to_expr(&rpn)
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        let mut n = self.expr_str.clone();
        while let Some(pos) = n.find('[') {
            let len = find_close(&n[pos..])?;
            let mut s = ColumnSet::new();
            s.add_yes(&n[pos + 1..pos + len - 1])?;
            s.lookup(fieldnames)?;
            let v = s.get_cols_num();
            let mut nval = String::new();
            if !v.is_empty() {
                write!(nval, "c{}", v[0] + 1)?;
            }
            for x in v.iter().skip(1) {
                write!(nval, ",c{}", x + 1)?;
            }
            n.replace_range(pos..pos + len, &nval);
        }
        let start_vars = self.vars.len();
        self.parse(&n)?;
        'outer: for y in self.vars.iter_mut().skip(start_vars) {
            static CN: LazyLock<Regex> = LazyLock::new(|| Regex::new("^c([0-9]+)$").unwrap());
            for (i, x) in fieldnames.iter().enumerate() {
                if *x == y.name {
                    y.col = Some(i);
                    continue 'outer;
                }
            }
            // lazy_static! {
            //     static ref CN: Regex = Regex::new("^c([0-9]+)$").unwrap();
            // }
            if let Some(cn) = CN.captures(&y.name) {
                y.col = Some(cn.get(1).unwrap().as_str().to_usize_whole(b"", "").unwrap() - 1);
                continue;
            }
            // FIXME - some way to declare other variables
            return err!("Undefined variable {}", y.name);
        }
        Ok(())
    }
    fn prepare(&mut self, t: &TextLine) {
        for x in &mut self.vars {
            if let Some(c) = x.col {
                x.val = t.get(c).to_f64_lossy();
            }
        }
    }
    fn prepare_line(&mut self, t: &[u8], delim: u8) {
        for x in &mut self.vars {
            if let Some(c) = x.col {
                x.val = get_col(t, c, delim).to_f64_lossy();
            }
        }
    }
    /// set the value of a variable
    pub fn set_var(&mut self, which: usize, val: f64) {
        self.vars[which].val = val;
    }
    /// return index of var, adding if necessary
    pub fn find_var(&mut self, name: &str) -> usize {
        for (i, x) in self.vars.iter().enumerate() {
            if x.name == name {
                return i;
            }
        }
        self.vars.push(VarMap::new(name));
        self.vars.len() - 1
    }
    fn rpn_to_expr(&mut self, rpn: &[Token]) -> Result<()> {
        let mut e = Vec::new();
        for x in rpn {
            match x {
                Token::Binary(op) => {
                    let top = e.len() - 1;
                    if let Node::Value(right) = e[top]
                        && let Node::Value(left) = e[top - 1]
                    {
                        e[top - 1] = Node::Value(apply_binary(*op, left, right));
                        e.pop();
                        continue;
                    }
                    e.push(Node::Binary(*op));
                }
                Token::Unary(op) => {
                    if *op != UnaryOp::Plus {
                        if let Node::Value(v) = e.last_mut().unwrap() {
                            *v = apply_unary(*op, *v);
                        } else {
                            e.push(Node::Unary(*op));
                        }
                    }
                }
                Token::LParen | Token::RParen | Token::Comma => {
                    eprintln!("Andy doesn't know what's going on");
                }
                Token::Number(n) => e.push(Node::Value(*n)),
                Token::Var(name) => {
                    if let Some(con) = find_const(name) {
                        e.push(Node::Value(con));
                    } else {
                        e.push(Node::Var(self.find_var(name)));
                    }
                }
                Token::Func(name, num) => {
                    if let Some(n) = num {
                        let f = find_func(name)?;
                        let min = min_args(f);
                        let max = max_args(f);
                        if *n < min {
                            return err!("Function {} requires at least {} parameters", name, min);
                        }
                        if (max > 0) && (*n > max) {
                            return err!("Function {} takes no more than {} parameters", name, max);
                        }
                        if *n == 1
                            && let Node::Value(v) = e.last_mut().unwrap()
                        {
                            *v = apply_func(f, &[*v]);
                            continue;
                        }
                        // FIXME -- hndle N args somehow?
                        e.push(Node::Func(f, *n));
                    } else {
                        eprintln!("Andy doesn't understand functions");
                    }
                }
            }
        }
        self.expr = e;
        Ok(())
    }
    /// evaluate constant expression
    pub fn eval_plain(&mut self) -> Result<f64> {
        // Error if variables?
        self.parse_self()?;
        Ok(self.do_eval())
    }
    /// evaluate expression with values from line
    pub fn eval(&mut self, line: &TextLine) -> f64 {
        self.prepare(line);
        self.do_eval()
    }
    /// evaluate expression with values from line
    pub fn eval_line(&mut self, line: &[u8], delim: u8) -> f64 {
        self.prepare_line(line, delim);
        self.do_eval()
    }
    fn do_eval(&mut self) -> f64 {
        self.stack.clear();
        for x in &self.expr {
            match x {
                Node::Value(v) => self.stack.push(*v),
                Node::Var(v) => self.stack.push(self.vars[*v].val),
                Node::Unary(op) => {
                    let top = self.stack.len() - 1;
                    self.stack[top] = apply_unary(*op, self.stack[top]);
                }
                Node::Binary(op) => {
                    let top = self.stack.len() - 2;
                    let left: f64 = self.stack[top];
                    let right: f64 = self.stack[top + 1];
                    self.stack.pop().unwrap();
                    self.stack[top] = apply_binary(*op, left, right);
                }
                Node::Func(op, num) => {
                    let v = apply_func(*op, &self.stack[self.stack.len() - num..]);
                    self.stack.resize(self.stack.len() - (num - 1), 0.0);
                    let pos = self.stack.len() - 1;
                    self.stack[pos] = v;
                }
            }
        }
        self.stack[0]
    }
}

fn factorial(x: f64) -> f64 {
    if x < 0.0 {
        return 0.0;
    }
    let pos = x.round() as usize;
    if pos > 170 { f64::INFINITY } else { FACTORIAL[pos] }
}

const FACTORIAL: [f64; 171] = [
    0.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5.04e3,
    4.032e4,
    3.6288e5,
    3.6288e6,
    3.99168e7,
    4.790_016e8,
    6.227_020_8e9,
    8.717_829_12e10,
    1.307_674_368e12,
    2.092_278_988_8e13,
    3.556_874_280_96e14,
    6.402_373_705_728e15,
    1.216_451_004_088_32e17,
    2.432_902_008_176_64e18,
    5.109_094_217_170_944e19,
    1.124_000_727_777_607_7e21,
    2.585_201_673_888_498e22,
    6.204_484_017_332_394e23,
    1.551_121_004_333_098_6e25,
    4.032_914_611_266_056_5e26,
    1.088_886_945_041_835_2e28,
    3.048_883_446_117_138_4e29,
    8.841_761_993_739_701e30,
    2.652_528_598_121_910_3e32,
    8.222_838_654_177_922e33,
    2.631_308_369_336_935e35,
    8.683_317_618_811_886e36,
    2.952_327_990_396_041_2e38,
    1.033_314_796_638_614_4e40,
    3.719_933_267_899_012e41,
    1.376_375_309_122_634_3e43,
    5.230_226_174_666_01e44,
    2.039_788_208_119_744_2e46,
    8.159_152_832_478_977e47,
    3.345_252_661_316_380_3e49,
    1.405_006_117_752_879_8e51,
    6.041_526_306_337_383e52,
    2.658_271_574_788_448_5e54,
    1.196_222_208_654_801_9e56,
    5.502_622_159_812_088_5e57,
    2.586_232_415_111_681_8e59,
    1.241_391_559_253_607_3e61,
    6.082_818_640_342_675e62,
    3.041_409_320_171_337_6e64,
    1.551_118_753_287_382_2e66,
    8.065_817_517_094_388e67,
    4.274_883_284_060_025_5e69,
    2.308_436_973_392_414e71,
    1.269_640_335_365_827_6e73,
    7.109_985_878_048_635e74,
    4.052_691_950_487_722e76,
    2.350_561_331_282_879e78,
    1.386_831_185_456_898_6e80,
    8.320_987_112_741_392e81,
    5.075_802_138_772_248e83,
    3.146_997_326_038_794e85,
    1.982_608_315_404_44e87,
    1.268_869_321_858_841_7e89,
    8.247_650_592_082_472e90,
    5.443_449_390_774_431e92,
    3.647_111_091_818_868e94,
    2.480_035_542_436_830_5e96,
    1.711_224_524_281_413e98,
    1.197_857_166_996_989e100,
    8.504_785_885_678_622e101,
    6.123_445_837_688_608e103,
    4.470_115_461_512_683_4e105,
    3.307_885_441_519_385_6e107,
    2.480_914_081_139_539e109,
    1.885_494_701_666_049_8e111,
    1.451_830_920_282_858_4e113,
    1.132_428_117_820_629_5e115,
    8.946_182_130_782_973e116,
    7.156_945_704_626_378e118,
    5.797_126_020_747_366e120,
    4.753_643_337_012_84e122,
    3.945_523_969_720_657e124,
    3.314_240_134_565_352e126,
    2.817_104_114_380_549_4e128,
    2.422_709_538_367_272_4e130,
    2.107_757_298_379_527e132,
    1.854_826_422_573_983_6e134,
    1.650_795_516_090_845_2e136,
    1.485_715_964_481_760_7e138,
    1.352_001_527_678_402_3e140,
    1.243_841_405_464_13e142,
    1.156_772_507_081_640_9e144,
    1.087_366_156_656_742_4e146,
    1.032_997_848_823_905_2e148,
    9.916_779_348_709_491e149,
    9.619_275_968_248_206e151,
    9.426_890_448_883_242e153,
    9.332_621_544_394_41e155,
    9.332_621_544_394_41e157,
    9.425_947_759_838_354e159,
    9.614_466_715_035_121e161,
    9.902_900_716_486_175e163,
    1.029_901_674_514_562_2e166,
    1.081_396_758_240_290_3e168,
    1.146_280_563_734_707_8e170,
    1.226_520_203_196_137_3e172,
    1.324_641_819_451_828_4e174,
    1.443_859_583_202_492_8e176,
    1.588_245_541_522_742_1e178,
    1.762_952_551_090_243_7e180,
    1.974_506_857_221_072_8e182,
    2.231_192_748_659_812_3e184,
    2.543_559_733_472_186e186,
    2.925_093_693_493_014e188,
    3.393_108_684_451_896_5e190,
    3.969_937_160_808_719e192,
    4.684_525_849_754_288_3e194,
    5.574_585_761_207_603e196,
    6.689_502_913_449_124e198,
    8.094_298_525_273_44e200,
    9.875_044_200_833_598e202,
    1.214_630_436_702_532_5e205,
    1.506_141_741_511_140_4e207,
    1.882_677_176_888_925_4e209,
    2.372_173_242_880_046e211,
    3.012_660_018_457_658e213,
    3.856_204_823_625_802_5e215,
    4.974_504_222_477_285_5e217,
    6.466_855_489_220_472e219,
    8.471_580_690_878_817e221,
    1.118_248_651_196_004e224,
    1.487_270_706_090_685_2e226,
    1.992_942_746_161_518e228,
    2.690_472_707_318_049_5e230,
    3.659_042_881_952_547e232,
    5.012_888_748_274_99e234,
    6.917_786_472_619_486e236,
    9.615_723_196_941_086e238,
    1.346_201_247_571_752e241,
    1.898_143_759_076_17e243,
    2.695_364_137_888_161_4e245,
    3.854_370_717_180_070_6e247,
    5.550_293_832_739_301e249,
    8.047_926_057_471_987e251,
    1.174_997_204_390_91e254,
    1.727_245_890_454_637_6e256,
    2.556_323_917_872_863_7e258,
    3.808_922_637_630_567e260,
    5.713_383_956_445_850_5e262,
    8.627_209_774_233_235e264,
    1.311_335_885_683_451_8e267,
    2.006_343_905_095_681e269,
    3.089_769_613_847_349e271,
    4.789_142_901_463_391e273,
    7.471_062_926_282_89e275,
    1.172_956_879_426_413_8e278,
    1.853_271_869_493_733_8e280,
    2.946_702_272_495_037e282,
    4.714_723_635_992_059e284,
    7.590_705_053_947_215e286,
    1.229_694_218_739_448_8e289,
    2.004_401_576_545_301_5e291,
    3.287_218_585_534_294_5e293,
    5.423_910_666_131_586e295,
    9.003_691_705_778_433e297,
    1.503_616_514_864_998_3e300,
    2.526_075_744_973_197e302,
    4.269_068_009_004_702_7e304,
    7.257_415_615_307_994e306,
];
