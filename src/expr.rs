//! Floating Point expressions
#![allow(dead_code)]
#![allow(clippy::float_cmp)]

use crate::num;
use crate::shunting_yard::*;
use crate::tokenizer::*;
use crate::{err, Error, Result, TextLine};
use std::f64::consts;
//use libm;

/// calc
pub fn calc(expr: &str) -> Result<()> {
    eprintln!("{}", expr);
    let tokens = tokenize(expr)?;
    eprintln!("{:?}", tokens);
    let rpn = to_rpn(&tokens)?;
    eprintln!("{:?}", rpn);
    let mut c = ExprContext::new();
    let pos = c.rpn_to_expr(&rpn)?;
    eprintln!("{:?}", c.e[pos]);
    let val = c.eval(pos);
    eprintln!("{}", val);
    Ok(())
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
    Abs,
    Sin,
    Max,
    If,
}

#[derive(Debug, Copy, Clone)]
struct ConstDef {
    name: &'static str,
    val: f64,
}
const CONSTS: [ConstDef; 15] = [
    ConstDef {
        name: "pi",
        val: consts::PI,
    },
    ConstDef {
        name: "e",
        val: consts::E,
    },
    ConstDef {
        name: "M_E",
        val: consts::E,
    },
    ConstDef {
        name: "M_LOG2E",
        val: consts::LOG2_E,
    },
    ConstDef {
        name: "M_LOG10E",
        val: consts::LOG10_E,
    },
    ConstDef {
        name: "M_LN2",
        val: consts::LN_2,
    },
    ConstDef {
        name: "M_LN10",
        val: consts::LN_10,
    },
    ConstDef {
        name: "M_PI",
        val: consts::PI,
    },
    ConstDef {
        name: "M_PI_2",
        val: consts::FRAC_PI_2,
    },
    ConstDef {
        name: "M_PI_4",
        val: consts::FRAC_PI_4,
    },
    ConstDef {
        name: "M_1_PI",
        val: consts::FRAC_1_PI,
    },
    ConstDef {
        name: "M_2_PI",
        val: consts::FRAC_2_PI,
    },
    ConstDef {
        name: "M_2_SQRTPI",
        val: consts::FRAC_2_SQRT_PI,
    },
    ConstDef {
        name: "M_SQRT2",
        val: consts::SQRT_2,
    },
    ConstDef {
        name: "M_SQRT1_2",
        val: consts::FRAC_1_SQRT_2,
    },
];

#[derive(Debug, Copy, Clone)]
struct FuncDef {
    name: &'static str,
    val: FuncOp,
}
const FUNCS: [FuncDef; 4] = [
    FuncDef {
        name: "abs",
        val: FuncOp::Abs,
    },
    FuncDef {
        name: "sin",
        val: FuncOp::Sin,
    },
    FuncDef {
        name: "max",
        val: FuncOp::Max,
    },
    FuncDef {
        name: "if",
        val: FuncOp::If,
    },
];

const fn min_args(f: FuncOp) -> usize {
    match f {
        FuncOp::If => 2,
        _ => 1,
    }
}

const fn max_args(f: FuncOp) -> usize {
    match f {
        FuncOp::If => 2,
        FuncOp::Max => 0,
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

type Expr = Vec<Node>;

struct ColMap {
    col_num: usize,
    var_num: usize,
}
struct VarMap {
    name: String,
    val: f64,
}

#[derive(Default)]
struct ExprContext {
    map: Vec<ColMap>,
    vals: Vec<VarMap>,
    e: Vec<Expr>,
    stack: Vec<f64>,
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
    match op {
        FuncOp::Abs => args[0].abs(),
        FuncOp::Sin => args[0].sin(),
        FuncOp::Max => {
            let mut v: f64 = args[0];
            for x in &args[1..] {
                if *x > v {
                    v = *x;
                }
            }
            v
        }
        FuncOp::If => {
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

impl ExprContext {
    fn new() -> Self {
        Self::default()
    }
    fn lookup(&mut self, fieldnames: &[&str]) -> Result<()> {
        'outer: for (j, y) in self.vals.iter().enumerate() {
            for (i, x) in fieldnames.iter().enumerate() {
                if *x == y.name {
                    self.map.push(ColMap {
                        col_num: i,
                        var_num: j,
                    });
                    continue 'outer;
                }
            }
            return err!("Undefined variable {}", y.name);
        }
        Ok(())
    }
    fn prepare(&mut self, t: &TextLine) {
        for x in &mut self.map {
            self.vals[x.var_num].val = num::str_to_d_lossy(t.get(x.col_num));
        }
    }
    fn find_var(&mut self, name: &str) -> usize {
        for (i, x) in self.vals.iter().enumerate() {
            if x.name == name {
                return i;
            }
        }
        self.vals.push(VarMap {
            name: name.to_string(),
            val: 0.0,
        });
        self.vals.len() - 1
    }
    fn rpn_to_expr(&mut self, rpn: &[Token]) -> Result<usize> {
        let mut e: Expr = Vec::new();
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
        self.e.push(e);
        Ok(self.e.len() - 1)
    }
    fn eval(&mut self, e_num: usize) -> f64 {
        self.stack.clear();
        for x in &self.e[e_num] {
            match x {
                Node::Value(v) => self.stack.push(*v),
                Node::Var(v) => self.stack.push(self.vals[*v].val),
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
