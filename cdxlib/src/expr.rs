//! Floating Point expressions
#![allow(dead_code)]
#![allow(clippy::float_cmp)]
/// Single operator
pub trait Expr {
    // type: string, u64, bool, f64?
    /// evaluate the expression in the current context
    fn get(&self, vars: &[f64]) -> f64;
}

struct Var {
    var: usize,
}

impl Var {
    fn new(var: usize) -> Self {
        Self { var }
    }
}

impl Expr for Var {
    fn get(&self, vars: &[f64]) -> f64 {
        vars[self.var]
    }
}

struct Const {
    val: f64,
}

impl Const {
    fn new(val: f64) -> Self {
        Self { val }
    }
}

impl Expr for Const {
    fn get(&self, _vars: &[f64]) -> f64 {
        self.val
    }
}

macro_rules! Binary {
    ($name: ident, $op:tt) => {
	struct $name {
	    left: Box<dyn Expr>,
	    right: Box<dyn Expr>,
	}

	impl $name {
	    fn new(left: Box<dyn Expr>, right: Box<dyn Expr>) ->Self {
		Self {left, right}
	    }
	}

	impl Expr for $name {
	    fn get(&self, vars: &[f64]) -> f64 {
		self.left.get(vars) $op self.right.get(vars)
	    }
	}
    }
}

fn make_bool(val: f64) -> bool {
    val != 0.0
}
fn make_f64(val: bool) -> f64 {
    if val {
        1.0
    } else {
        0.0
    }
}

macro_rules! BinaryCmp {
    ($name: ident, $op:tt) => {
	struct $name {
	    left: Box<dyn Expr>,
	    right: Box<dyn Expr>,
	}

	impl $name {
	    fn new(left: Box<dyn Expr>, right: Box<dyn Expr>) ->Self {
		Self {left, right}
	    }
	}

	impl Expr for $name {
	    fn get(&self, vars: &[f64]) -> f64 {
		make_f64(self.left.get(vars) $op self.right.get(vars))
	    }
	}
    }
}

macro_rules! BinaryJoin {
    ($name: ident, $op:tt) => {
	struct $name {
	    left: Box<dyn Expr>,
	    right: Box<dyn Expr>,
	}

	impl $name {
	    fn new(left: Box<dyn Expr>, right: Box<dyn Expr>) ->Self {
		Self {left, right}
	    }
	}

	impl Expr for $name {
	    fn get(&self, vars: &[f64]) -> f64 {
		make_f64(make_bool(self.left.get(vars)) $op make_bool(self.right.get(vars)))
	    }
	}
    }
}

Binary!(Plus, +);
Binary!(Minus, -);
Binary!(Mult, *);
Binary!(Divide, /);
BinaryCmp!(LT, <);
BinaryCmp!(LE, <=);
BinaryCmp!(GT, >);
BinaryCmp!(GE, >=);
BinaryCmp!(Equal, ==);
BinaryCmp!(NotEqual, !=);
BinaryJoin!(And, &&);
BinaryJoin!(Or, ||);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_plus() {
        let mut vars: Vec<f64> = Vec::new();
        vars.push(1.0);
        vars.push(2.0);
        let left = Box::new(Var::new(0));
        let right = Box::new(Const::new(5.0));
        let left2 = Box::new(Const::new(4.0));
        let right2 = Box::new(Var::new(1));
        let plus = Box::new(Plus::new(left, right));
        let minus = Box::new(Minus::new(left2, right2));
        let mult = Box::new(Mult::new(plus, minus));
        assert_eq!(12.0, mult.get(&vars));
    }

    #[test]
    fn check_comp() {
        let mut vars: Vec<f64> = Vec::new();
        vars.push(1.0);
        vars.push(2.0);
        let left = Box::new(Var::new(0));
        let right = Box::new(Const::new(5.0));
        let left2 = Box::new(Const::new(4.0));
        let right2 = Box::new(Var::new(1));
        let less = Box::new(LT::new(left, right));
        let equal = Box::new(Equal::new(left2, right2));
        assert_eq!(1.0, less.get(&vars));
        assert_eq!(0.0, equal.get(&vars));
    }
}
