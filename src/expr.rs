//! Floating Point expressions
#![allow(dead_code)]
#![allow(clippy::float_cmp)]

use crate::column::get_col;
use crate::num;
use crate::shunting_yard::*;
use crate::tokenizer::*;
use crate::{err, Error, Result, TextLine};
use lazy_static::lazy_static;
use libm;
use regex::Regex;
use std::f64::consts;

/// evaluate a constant arithmetic expression
pub fn calc(expr: &str) -> Result<f64> {
    let mut c = Expr::new(expr)?;
    Ok(c.eval_plain())
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
    use FuncOp::*;
    match f {
        Atan2 | Copysign | Fmin | Fdim | Fmax | Fmod | Hypot | Jn | Ldexp | Nextafter
        | Remainder | Scalbn | Pow | Yn => 2,
        Fma | If => 3,
        _ => 1,
    }
}

const fn max_args(f: FuncOp) -> usize {
    use FuncOp::*;
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
#[derive(Default, Debug)]
/// A floating point expression
pub struct Expr {
    expr_str: String,
    expr: Vec<Node>,
    vars: Vec<VarMap>,
    stack: Vec<f64>, // temp space for eval
}

const fn to_f(x: bool) -> f64 {
    if x {
        1.0
    } else {
        0.0
    }
}

fn apply_unary(op: UnaryOp, x: f64) -> f64 {
    match op {
        UnaryOp::Plus => x,
        UnaryOp::Minus => -x,
        UnaryOp::Fact => factorial(x),
    }
}

fn apply_func(op: FuncOp, args: &[f64]) -> f64 {
    use FuncOp::*;
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
            if args[0] != 0.0 {
                args[1]
            } else {
                args[2]
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
        let mut s = Self {
            expr_str: expr.to_string(),
            ..Self::default()
        };
        let tokens = tokenize(expr)?;
        let rpn = to_rpn(&tokens)?;
        s.rpn_to_expr(&rpn)?;
        Ok(s)
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
    pub fn expr(&self) -> &str {
        &self.expr_str
    }
    /// resolve named columns
    pub fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        'outer: for y in &mut self.vars {
            for (i, x) in fieldnames.iter().enumerate() {
                if *x == y.name {
                    y.col = Some(i);
                    continue 'outer;
                }
            }
            lazy_static! {
                static ref CN: Regex = Regex::new("^c[0-9]+$").unwrap();
            }
            if let Some(cn) = CN.captures(&y.name) {
                y.col = Some(cn.get(1).unwrap().as_str().parse::<usize>().unwrap());
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
                x.val = num::str_to_d_lossy(t.get(c));
            }
        }
    }
    fn prepare_line(&mut self, t: &[u8], delim: u8) {
        for x in &mut self.vars {
            if let Some(c) = x.col {
                x.val = num::str_to_d_lossy(get_col(t, c, delim));
            }
        }
    }
    fn find_var(&mut self, name: &str) -> usize {
        for (i, x) in self.vars.iter().enumerate() {
            if x.name == name {
                return i;
            }
        }
        self.vars.push(VarMap {
            name: name.to_string(),
            val: 0.0,
            col: None,
        });
        self.vars.len() - 1
    }
    fn rpn_to_expr(&mut self, rpn: &[Token]) -> Result<()> {
        let mut e = Vec::new();
        for x in rpn {
            match x {
                Token::Binary(op) => {
                    let top = e.len() - 1;
                    if let Node::Value(right) = e[top] {
                        if let Node::Value(left) = e[top - 1] {
                            e[top - 1] = Node::Value(apply_binary(*op, left, right));
                            e.pop();
                            continue;
                        }
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
                    };
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
                        if *n == 1 {
                            if let Node::Value(v) = e.last_mut().unwrap() {
                                *v = apply_func(f, &[*v]);
                                continue;
                            }
                        }
                        // FIXME -- hndle N args somehow?
                        e.push(Node::Func(f, *n))
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
    pub fn eval_plain(&mut self) -> f64 {
        // Error if variables?
        self.do_eval()
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
    if pos > 170 {
        f64::INFINITY
    } else {
        FACTORIAL[pos]
    }
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
    4.790016e8,
    6.2270208e9,
    8.71782912e10,
    1.307674368e12,
    2.0922789888e13,
    3.55687428096e14,
    6.402373705728e15,
    1.21645100408832e17,
    2.43290200817664e18,
    5.109094217170944e19,
    1.1240007277776077e21,
    2.585201673888498e22,
    6.204484017332394e23,
    1.5511210043330986e25,
    4.0329146112660565e26,
    1.0888869450418352e28,
    3.0488834461171384e29,
    8.841761993739701e30,
    2.6525285981219103e32,
    8.222838654177922e33,
    2.631308369336935e35,
    8.683317618811886e36,
    2.9523279903960412e38,
    1.0333147966386144e40,
    3.719933267899012e41,
    1.3763753091226343e43,
    5.23022617466601e44,
    2.0397882081197442e46,
    8.159152832478977e47,
    3.3452526613163803e49,
    1.4050061177528798e51,
    6.041526306337383e52,
    2.6582715747884485e54,
    1.1962222086548019e56,
    5.5026221598120885e57,
    2.5862324151116818e59,
    1.2413915592536073e61,
    6.082818640342675e62,
    3.0414093201713376e64,
    1.5511187532873822e66,
    8.065817517094388e67,
    4.2748832840600255e69,
    2.308436973392414e71,
    1.2696403353658276e73,
    7.109985878048635e74,
    4.052691950487722e76,
    2.350561331282879e78,
    1.3868311854568986e80,
    8.320987112741392e81,
    5.075802138772248e83,
    3.146997326038794e85,
    1.98260831540444e87,
    1.2688693218588417e89,
    8.247650592082472e90,
    5.443449390774431e92,
    3.647111091818868e94,
    2.4800355424368305e96,
    1.711224524281413e98,
    1.197857166996989e100,
    8.504785885678622e101,
    6.123445837688608e103,
    4.4701154615126834e105,
    3.3078854415193856e107,
    2.480914081139539e109,
    1.8854947016660498e111,
    1.4518309202828584e113,
    1.1324281178206295e115,
    8.946182130782973e116,
    7.156945704626378e118,
    5.797126020747366e120,
    4.75364333701284e122,
    3.945523969720657e124,
    3.314240134565352e126,
    2.8171041143805494e128,
    2.4227095383672724e130,
    2.107757298379527e132,
    1.8548264225739836e134,
    1.6507955160908452e136,
    1.4857159644817607e138,
    1.3520015276784023e140,
    1.24384140546413e142,
    1.1567725070816409e144,
    1.0873661566567424e146,
    1.0329978488239052e148,
    9.916779348709491e149,
    9.619275968248206e151,
    9.426890448883242e153,
    9.33262154439441e155,
    9.33262154439441e157,
    9.425947759838354e159,
    9.614466715035121e161,
    9.902900716486175e163,
    1.0299016745145622e166,
    1.0813967582402903e168,
    1.1462805637347078e170,
    1.2265202031961373e172,
    1.3246418194518284e174,
    1.4438595832024928e176,
    1.5882455415227421e178,
    1.7629525510902437e180,
    1.9745068572210728e182,
    2.2311927486598123e184,
    2.543559733472186e186,
    2.925093693493014e188,
    3.3931086844518965e190,
    3.969937160808719e192,
    4.6845258497542883e194,
    5.574585761207603e196,
    6.689502913449124e198,
    8.09429852527344e200,
    9.875044200833598e202,
    1.2146304367025325e205,
    1.5061417415111404e207,
    1.8826771768889254e209,
    2.372173242880046e211,
    3.012660018457658e213,
    3.8562048236258025e215,
    4.9745042224772855e217,
    6.466855489220472e219,
    8.471580690878817e221,
    1.118248651196004e224,
    1.4872707060906852e226,
    1.992942746161518e228,
    2.6904727073180495e230,
    3.659042881952547e232,
    5.01288874827499e234,
    6.917786472619486e236,
    9.615723196941086e238,
    1.346201247571752e241,
    1.89814375907617e243,
    2.6953641378881614e245,
    3.8543707171800706e247,
    5.550293832739301e249,
    8.047926057471987e251,
    1.17499720439091e254,
    1.7272458904546376e256,
    2.5563239178728637e258,
    3.808922637630567e260,
    5.7133839564458505e262,
    8.627209774233235e264,
    1.3113358856834518e267,
    2.006343905095681e269,
    3.089769613847349e271,
    4.789142901463391e273,
    7.47106292628289e275,
    1.1729568794264138e278,
    1.8532718694937338e280,
    2.946702272495037e282,
    4.714723635992059e284,
    7.590705053947215e286,
    1.2296942187394488e289,
    2.0044015765453015e291,
    3.2872185855342945e293,
    5.423910666131586e295,
    9.003691705778433e297,
    1.5036165148649983e300,
    2.526075744973197e302,
    4.2690680090047027e304,
    7.257415615307994e306,
];
