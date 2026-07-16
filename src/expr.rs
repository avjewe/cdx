//! Floating Point expressions

use crate::column::get_col;
use crate::prelude::*;
use crate::shunting_yard::to_rpn;
use crate::tok2::{BinaryOp, Token, UnaryOp, tokenize};
use crate::util::find_close;
use crate::*;
use regex::Regex;
use std::f64::consts;
use std::fmt::Write;
use std::sync::LazyLock;

/// evaluate a constant arithmetic expression
pub fn calc(expr: &str, trig_mode: TrigMode) -> Result<f64> {
    let mut c = Expr::new(expr)?;
    c.set_trig_mode(trig_mode);
    c.eval_plain()
}

/// evaluate an arithmetic expression
pub fn eval(expr: &str) -> Result<f64> {
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
    for f in &FUNCTIONS {
        if f.name == x {
            return Ok(f.val);
        }
    }
    err!("No such function as {}", x)
}

#[derive(Debug, Copy, Clone)]
enum FuncOp {
    Abs,
    Acos,
    Acosh,
    Asin,
    Asinh,
    Atan,
    Atan2,
    Atanh,
    Cbrt,
    Ceil,
    Clamp,
    Copysign,
    Cos,
    Cosh,
    DivEuclid,
    Exp,
    Exp2,
    Expm1,
    Floor,
    MulAdd,
    Fract,
    Hypot,
    IsFinite,
    IsInfinite,
    IsNan,
    IsNormal,
    IsSignNegative,
    IsSignPositive,
    IsSubnormal,
    Ln,
    Ln1p,
    Log,
    Log2,
    Log10,
    Max,
    Midpoint,
    Min,
    NextUp,
    NextDown,
    PowF,
    PowI,
    Recip,
    RemEuclid,
    Round,
    RoundTiesEven,
    Signum,
    Sin,
    Sinh,
    Sqrt,
    Tan,
    Tanh,
    ToDegrees,
    ToRadians,
    TotalCmp,
    Trunc,
    If,
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

const CONSTS: [ConstDef; 37] = [
    con!("pi", consts::PI),
    con!("e", consts::E),
    con!("E", consts::E),
    con!("EULER_GAMMA", consts::EULER_GAMMA),
    con!("FRAC_1_PI", consts::FRAC_1_PI),
    con!("FRAC_1_SQRT_2", consts::FRAC_1_SQRT_2),
    con!("FRAC_2_SQRT_PI", consts::FRAC_2_SQRT_PI),
    con!("FRAC_PI_2", consts::FRAC_PI_2),
    con!("FRAC_PI_3", consts::FRAC_PI_3),
    con!("FRAC_PI_4", consts::FRAC_PI_4),
    con!("FRAC_PI_6", consts::FRAC_PI_6),
    con!("FRAC_PI_8", consts::FRAC_PI_8),
    con!("FRAC_2_PI", consts::FRAC_2_PI),
    con!("GOLDEN_RATIO", consts::GOLDEN_RATIO),
    con!("LN_2", consts::LN_2),
    con!("LN_10", consts::LN_10),
    con!("LOG2_10", consts::LOG2_10),
    con!("LOG2_E", consts::LOG2_E),
    con!("LOG10_2", consts::LOG10_2),
    con!("LOG10_E", consts::LOG10_E),
    con!("PI", consts::PI),
    con!("SQRT_2", consts::SQRT_2),
    con!("TAU", consts::TAU),
    con!("DIGITS", f64::DIGITS as f64),
    con!("EPSILON", f64::EPSILON),
    con!("INFINITY", f64::INFINITY),
    con!("MANTISSA_DIGITS", f64::MANTISSA_DIGITS as f64),
    con!("MAX", f64::MAX),
    con!("MAX_10_EXP", f64::MAX_10_EXP as f64),
    con!("MAX_EXP", f64::MAX_EXP as f64),
    con!("MIN", f64::MIN),
    con!("MIN_10_EXP", f64::MIN_10_EXP as f64),
    con!("MIN_EXP", f64::MIN_EXP as f64),
    con!("MIN_POSITIVE", f64::MIN_POSITIVE),
    con!("NAN", f64::NAN),
    con!("NEG_INFINITY", f64::NEG_INFINITY),
    con!("RADIX", f64::RADIX as f64),
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
    for (i, x) in FUNCTIONS.iter().enumerate() {
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

const FUNCTIONS: [FuncDef; 57] = [
    fun!("abs", FuncOp::Abs),
    fun!("acos", FuncOp::Acos),
    fun!("acosh", FuncOp::Acosh),
    fun!("asin", FuncOp::Asin),
    fun!("asinh", FuncOp::Asinh),
    fun!("atan", FuncOp::Atan),
    fun!("atan2", FuncOp::Atan2),
    fun!("atanh", FuncOp::Atanh),
    fun!("cbrt", FuncOp::Cbrt),
    fun!("ceil", FuncOp::Ceil),
    fun!("clamp", FuncOp::Clamp),
    fun!("copysign", FuncOp::Copysign),
    fun!("cos", FuncOp::Cos),
    fun!("cosh", FuncOp::Cosh),
    fun!("div_euclid", FuncOp::DivEuclid),
    fun!("exp", FuncOp::Exp),
    fun!("exp2", FuncOp::Exp2),
    fun!("exp_m1", FuncOp::Expm1),
    fun!("floor", FuncOp::Floor),
    fun!("fract", FuncOp::Fract),
    fun!("hypot", FuncOp::Hypot),
    fun!("is_finite", FuncOp::IsFinite),
    fun!("is_infinite", FuncOp::IsInfinite),
    fun!("is_nan", FuncOp::IsNan),
    fun!("is_normal", FuncOp::IsNormal),
    fun!("is_sign_negative", FuncOp::IsSignNegative),
    fun!("is_sign_positive", FuncOp::IsSignPositive),
    fun!("is_subnormal", FuncOp::IsSubnormal),
    fun!("ln", FuncOp::Ln),
    fun!("ln_1p", FuncOp::Ln1p),
    fun!("log", FuncOp::Log),
    fun!("log2", FuncOp::Log2),
    fun!("log10", FuncOp::Log10),
    fun!("max", FuncOp::Max),
    fun!("midpoint", FuncOp::Midpoint),
    fun!("min", FuncOp::Min),
    fun!("mul_add", FuncOp::MulAdd),
    fun!("next_down", FuncOp::NextDown),
    fun!("next_up", FuncOp::NextUp),
    fun!("powf", FuncOp::PowF),
    fun!("powi", FuncOp::PowI),
    fun!("recip", FuncOp::Recip),
    fun!("rem_euclid", FuncOp::RemEuclid),
    fun!("round", FuncOp::Round),
    fun!("round_ties_even", FuncOp::RoundTiesEven),
    fun!("signum", FuncOp::Signum),
    fun!("sin", FuncOp::Sin),
    fun!("sinh", FuncOp::Sinh),
    fun!("sqrt", FuncOp::Sqrt),
    fun!("tan", FuncOp::Tan),
    fun!("tanh", FuncOp::Tanh),
    fun!("trunc", FuncOp::Trunc),
    fun!("to_degrees", FuncOp::ToDegrees),
    fun!("to_radians", FuncOp::ToRadians),
    fun!("total_cmp", FuncOp::TotalCmp),
    fun!("if", FuncOp::If),
    fun!("avg", FuncOp::Avg),
];

const fn min_args(f: FuncOp) -> usize {
    match f {
        FuncOp::Atan2
        | FuncOp::Copysign
        | FuncOp::DivEuclid
        | FuncOp::Hypot
        | FuncOp::Log
        | FuncOp::Midpoint
        | FuncOp::PowF
        | FuncOp::PowI
        | FuncOp::RemEuclid
        | FuncOp::TotalCmp => 2,
        FuncOp::Clamp | FuncOp::MulAdd | FuncOp::If => 3,
        _ => 1,
    }
}

const fn max_args(f: FuncOp) -> usize {
    match f {
        FuncOp::Min | FuncOp::Max | FuncOp::Avg => 0,
        _ => min_args(f),
    }
}

#[derive(Debug, Copy, Clone)]
enum Node {
    Value(f64),
    Var(usize),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Func(FuncOp, usize),
    Dice(usize, usize),
}

#[derive(Default, Debug, Clone)]
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

/// trigonometric mode for trig functions
#[derive(Debug, Copy, Clone, Default)]
pub enum TrigMode {
    /// interpret trig function arguments as radians, and return radians for inverse trig functions
    #[default]
    Radians,
    /// interpret trig function arguments as degrees, and return degrees for inverse trig functions
    Degrees,
}

#[derive(Default, Debug, Clone)]
/// A floating point expression
#[expect(clippy::struct_field_names)]
pub struct Expr {
    expr_str: String,
    expr: Vec<Node>,
    vars: Vec<VarMap>, // FIXME - Rc<RefCell<Vec<VarMap>>>, shared
    stack: Vec<f64>,   // temp space for eval
    trig_mode: TrigMode,
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

#[expect(clippy::cast_precision_loss)]
fn roll_dice(x: usize, y: usize) -> f64 {
    let mut ret = 0usize;
    for _ in 0..x {
        ret += fastrand::usize(..y) + 1;
    }
    ret as f64
}

impl Expr {
    /// set trigonometric mode for trig functions
    pub const fn set_trig_mode(&mut self, mode: TrigMode) {
        self.trig_mode = mode;
    }

    #[expect(clippy::missing_asserts_for_indexing)]
    #[expect(clippy::cast_precision_loss)]
    fn apply_func(&self, op: FuncOp, args: &[f64]) -> f64 {
        match op {
            FuncOp::Acos => self.acos(args[0]),
            FuncOp::Acosh => args[0].acosh(),
            FuncOp::Asinh => args[0].asinh(),
            FuncOp::Asin => self.asin(args[0]),
            FuncOp::Atan => self.atan(args[0]),
            FuncOp::Atan2 => self.atan2(args[1], args[0]),
            FuncOp::Atanh => args[0].atanh(),
            FuncOp::Cbrt => args[0].cbrt(),
            FuncOp::Ceil => args[0].ceil(),
            FuncOp::Clamp => args[0].clamp(args[1], args[2]),
            FuncOp::Copysign => args[0].copysign(args[1]),
            FuncOp::Cos => self.cos(args[0]),
            FuncOp::Cosh => args[0].cosh(),
            FuncOp::DivEuclid => args[0].div_euclid(args[1]),
            FuncOp::Exp => args[0].exp(),
            FuncOp::Exp2 => args[0].exp2(),
            FuncOp::Expm1 => args[0].exp_m1(),
            FuncOp::Floor => args[0].floor(),
            FuncOp::Fract => args[0].fract(),
            FuncOp::Hypot => args[0].hypot(args[1]),
            FuncOp::IsFinite => to_f(args[0].is_finite()),
            FuncOp::IsInfinite => to_f(args[0].is_infinite()),
            FuncOp::IsNan => to_f(args[0].is_nan()),
            FuncOp::IsNormal => to_f(args[0].is_normal()),
            FuncOp::IsSignNegative => to_f(args[0].is_sign_negative()),
            FuncOp::IsSignPositive => to_f(args[0].is_sign_positive()),
            FuncOp::IsSubnormal => to_f(args[0].is_subnormal()),
            FuncOp::Ln => args[0].ln(),
            FuncOp::Ln1p => args[0].ln_1p(),
            FuncOp::Log => args[0].log(args[1]),
            FuncOp::Log2 => args[0].log2(),
            FuncOp::Log10 => args[0].log10(),
            FuncOp::Midpoint => args[0].midpoint(args[1]),
            FuncOp::MulAdd => args[0].mul_add(args[1], args[2]),
            FuncOp::NextDown => args[0].next_down(),
            FuncOp::NextUp => args[0].next_up(),
            FuncOp::PowF => args[0].powf(args[1]),
            FuncOp::PowI => args[0].powi(args[1].round() as i32),
            FuncOp::Recip => args[0].recip(),
            FuncOp::RemEuclid => args[0].rem_euclid(args[1]),
            FuncOp::Round => args[0].round(),
            FuncOp::RoundTiesEven => args[0].round_ties_even(),
            FuncOp::Signum => args[0].signum(),
            FuncOp::Sin => self.sin(args[0]),
            FuncOp::Sinh => args[0].sinh(),
            FuncOp::Sqrt => args[0].sqrt(),
            FuncOp::Tan => self.tan(args[0]),
            FuncOp::Tanh => args[0].tanh(),
            FuncOp::ToDegrees => args[0].to_degrees(),
            FuncOp::ToRadians => args[0].to_radians(),
            FuncOp::TotalCmp => f64::from(args[0].total_cmp(&args[1]) as i32),
            FuncOp::Trunc => args[0].trunc(),
            FuncOp::Abs => args[0].abs(),
            FuncOp::Max => {
                let mut v: f64 = args[0];
                for x in &args[1..] {
                    v = v.max(*x);
                }
                v
            }
            FuncOp::Min => {
                let mut v: f64 = args[0];
                for x in &args[1..] {
                    v = v.min(*x);
                }
                v
            }
            FuncOp::Avg => {
                let mut v: f64 = 0.0;
                for x in args {
                    v += *x;
                }
                v / (args.len() as f64)
            }
            FuncOp::If => {
                if args[0] == 0.0 {
                    args[2]
                } else {
                    args[1]
                }
            }
        }
    }

    fn sin(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.sin(),
            TrigMode::Degrees => (x.to_radians()).sin(),
        }
    }
    fn cos(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.cos(),
            TrigMode::Degrees => (x.to_radians()).cos(),
        }
    }
    fn tan(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.tan(),
            TrigMode::Degrees => (x.to_radians()).tan(),
        }
    }
    fn atan(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.atan(),
            TrigMode::Degrees => x.atan().to_degrees(),
        }
    }
    fn acos(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.acos(),
            TrigMode::Degrees => x.acos().to_degrees(),
        }
    }
    fn asin(&self, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => x.asin(),
            TrigMode::Degrees => x.asin().to_degrees(),
        }
    }
    fn atan2(&self, y: f64, x: f64) -> f64 {
        match self.trig_mode {
            TrigMode::Radians => y.atan2(x),
            TrigMode::Degrees => y.atan2(x).to_degrees(),
        }
    }

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
    pub fn lookup(&mut self, field_names: &ColumnNamesRef) -> Result<()> {
        let mut n = self.expr_str.clone();
        while let Some(pos) = n.find('[') {
            let len = find_close(&n[pos..])?;
            let mut s = ColumnSet::new();
            s.add_yes(&n[pos + 1..pos + len - 1])?;
            s.lookup(field_names)?;
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
            for (i, x) in field_names.iter().enumerate() {
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
                Token::Dice(x, y) => {
                    e.push(Node::Dice(*x, *y));
                }
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
                            *v = self.apply_func(f, &[*v]);
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
                Node::Dice(x, y) => self.stack.push(roll_dice(*x, *y)),
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
                    let v = self.apply_func(*op, &self.stack[self.stack.len() - num..]);
                    self.stack.resize(self.stack.len() - (num - 1), 0.0);
                    let pos = self.stack.len() - 1;
                    self.stack[pos] = v;
                }
            }
        }
        if self.stack.is_empty() { 0.0 } else { self.stack[0] }
    }
}

fn factorial(x: f64) -> f64 {
    if x < 0.0 {
        return 0.0;
    }
    let pos = x.round() as usize;
    if pos > 170 { f64::INFINITY } else { FACTORIAL[pos] }
}

const MAX_FACTORIAL: usize = 170;

// Initialization for pre-computed cache of 171 factorial
// values 0!...170!
#[expect(clippy::cast_precision_loss)]
const FACTORIAL: [f64; MAX_FACTORIAL + 1] = {
    let mut fcache = [1.0; MAX_FACTORIAL + 1];

    // `const` only allow while loops
    let mut i = 1;
    while i < MAX_FACTORIAL + 1 {
        fcache[i] = fcache[i - 1] * i as f64;
        i += 1;
    }

    fcache
};

#[cfg(test)]
mod tests {
    use super::*;

    // Should always pass, unless using extension like 3d6
    macro_rules! test_expr {
        ($e:expr) => {
            assert_eq!($e, eval(stringify!($e)).unwrap());
        };
    }

    //
    macro_rules! test_expr2 {
        ($e:expr, $e2:expr) => {
            assert_eq!($e, eval($e2).unwrap());
        };
    }

    #[test]
    fn exprs() {
        test_expr!(2.0 * 3.0);
        test_expr2!(2.0 * 3.0, "2*3");
        test_expr2!(2.0 * 3.0 * 4.0, "4!");
    }
}
