//! Implementation of the shunting-yard algorithm for converting an infix expression to an
//! expression in reverse Polish notation (RPN).
//!
//! See the Wikipedia articles on the [shunting-yard algorithm][shunting] and on [reverse Polish
//! notation][RPN] for more details.
//!
//! [RPN]: https://en.wikipedia.org/wiki/Reverse_Polish_notation
//! [shunting]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
use self::Associativity::{Left, Right, NA};
use crate::prelude::*;
use crate::tok2::BinaryOp::{Div, Minus, Plus, Pow, Rem, Times, EQ, GE, GT, LE, LT, NE};
use crate::tok2::Token;
use crate::tok2::Token::{Binary, Comma, Func, LParen, Number, RParen, Unary, Var};
use crate::tok2::UnaryOp;

#[derive(Debug, Clone, Copy)]
enum Associativity {
    Left,
    Right,
    NA,
}

/// Returns the operator precedence and associativity for a given token.
const fn prec_assoc(token: &Token) -> (u32, Associativity) {
    match *token {
        Binary(op) => match op {
            LE | GE | LT | GT | EQ | NE => (1, Left),
            Plus | Minus => (2, Left),
            Times | Div | Rem => (3, Left),
            Pow => (5, Right),
        },
        Unary(op) => match op {
            UnaryOp::Plus | UnaryOp::Minus => (4, NA),
            UnaryOp::Fact => (6, NA),
        },
        Var(_) | Number(_) | Func(..) | LParen | RParen | Comma => (0, NA),
    }
}

/// Converts a tokenized infix expression to reverse Polish notation.
///
/// # Failure
///
/// Returns `Err` if the input expression is not well-formed.
pub(crate) fn to_rpn(input: &[Token]) -> Result<Vec<Token>> {
    let mut output = Vec::with_capacity(input.len());
    let mut stack = Vec::with_capacity(input.len());

    for (index, token) in input.iter().enumerate() {
        let token = token.clone();
        match token {
            Number(_) | Var(_) => output.push(token),
            Unary(_) => stack.push((index, token)),
            Binary(_) => {
                let pa1 = prec_assoc(&token);
                while !stack.is_empty() {
                    let pa2 = prec_assoc(&stack.last().unwrap().1);
                    match (pa1, pa2) {
                        ((i, Left), (j, _)) if i <= j => {
                            output.push(stack.pop().unwrap().1);
                        }
                        ((i, Right), (j, _)) if i < j => {
                            output.push(stack.pop().unwrap().1);
                        }
                        _ => {
                            break;
                        }
                    }
                }
                stack.push((index, token));
            }
            LParen => stack.push((index, token)),
            RParen => {
                let mut found = false;
                while let Some((_, t)) = stack.pop() {
                    match t {
                        LParen => {
                            found = true;
                            break;
                        }
                        Func(name, nargs) => {
                            found = true;
                            output.push(Func(name, Some(nargs.unwrap_or(0) + 1)));
                            break;
                        }
                        _ => output.push(t),
                    }
                }
                if !found {
                    return err!("Mismatched Right Paren");
                }
            }
            Comma => {
                let mut found = false;
                while let Some((i, t)) = stack.pop() {
                    match t {
                        LParen => {
                            return err!("Unexpected Comma a");
                        }
                        Func(name, nargs) => {
                            found = true;
                            stack.push((i, Func(name, Some(nargs.unwrap_or(0) + 1))));
                            break;
                        }
                        _ => output.push(t),
                    }
                }
                if !found {
                    return err!("Unexpected Comma b");
                }
            }
            Func(..) => stack.push((index, token)),
        }
    }

    while let Some((_index, token)) = stack.pop() {
        match token {
            Unary(_) | Binary(_) => output.push(token),
            LParen | Func(..) => {
                return err!("Mismatched Left Paren");
            }
            _ => panic!("Unexpected token on stack."),
        }
    }

    // verify rpn
    let mut n_operands = 0isize;
    for token in &output {
        match *token {
            Var(_) | Number(_) => n_operands += 1,
            Unary(_) => (),
            Binary(_) => n_operands -= 1,
            Func(_, Some(n_args)) => n_operands -= n_args as isize - 1,
            _ => panic!("Nothing else should be here"),
        }
        if n_operands <= 0 {
            return err!("Not Enough Operands");
        }
    }

    if n_operands > 1 {
        return err!("Too Many Operands");
    }

    output.shrink_to_fit();
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tok2::UnaryOp;

    #[test]
    fn test_to_rpn() {
        assert_eq!(to_rpn(&[Number(1.)]).unwrap(), vec![Number(1.)]);
        assert_eq!(
            to_rpn(&[Number(1.), Binary(Plus), Number(2.)]).unwrap(),
            vec![Number(1.), Number(2.), Binary(Plus)]
        );
        assert_eq!(
            to_rpn(&[Unary(UnaryOp::Minus), Number(1.), Binary(Pow), Number(2.)]).unwrap(),
            vec![Number(1.), Number(2.), Binary(Pow), Unary(UnaryOp::Minus)]
        );
        assert_eq!(
            to_rpn(&[Number(1.), Unary(UnaryOp::Fact), Binary(Pow), Number(2.)]).unwrap(),
            vec![Number(1.), Unary(UnaryOp::Fact), Number(2.), Binary(Pow)]
        );
        assert_eq!(
            to_rpn(&[
                Number(1.),
                Unary(UnaryOp::Fact),
                Binary(Div),
                LParen,
                Number(2.),
                Binary(Plus),
                Number(3.),
                RParen,
                Unary(UnaryOp::Fact)
            ])
            .unwrap(),
            vec![
                Number(1.),
                Unary(UnaryOp::Fact),
                Number(2.),
                Number(3.),
                Binary(Plus),
                Unary(UnaryOp::Fact),
                Binary(Div)
            ]
        );
        assert_eq!(
            to_rpn(&[
                Number(3.),
                Binary(Minus),
                Number(1.),
                Binary(Times),
                Number(2.)
            ])
            .unwrap(),
            vec![
                Number(3.),
                Number(1.),
                Number(2.),
                Binary(Times),
                Binary(Minus)
            ]
        );
        assert_eq!(
            to_rpn(&[
                LParen,
                Number(3.),
                Binary(Minus),
                Number(1.),
                RParen,
                Binary(Times),
                Number(2.)
            ])
            .unwrap(),
            vec![
                Number(3.),
                Number(1.),
                Binary(Minus),
                Number(2.),
                Binary(Times)
            ]
        );
        assert_eq!(
            to_rpn(&[
                Number(1.),
                Binary(Minus),
                Unary(UnaryOp::Minus),
                Unary(UnaryOp::Minus),
                Number(2.)
            ])
            .unwrap(),
            vec![
                Number(1.),
                Number(2.),
                Unary(UnaryOp::Minus),
                Unary(UnaryOp::Minus),
                Binary(Minus)
            ]
        );
        assert_eq!(
            to_rpn(&[Var("x".into()), Binary(Plus), Var("y".into())]).unwrap(),
            vec![Var("x".into()), Var("y".into()), Binary(Plus)]
        );

        assert_eq!(
            to_rpn(&[
                Func("max".into(), None),
                Func("sin".into(), None),
                Number(1f64),
                RParen,
                Comma,
                Func("cos".into(), None),
                Number(2f64),
                RParen,
                RParen
            ])
            .unwrap(),
            vec![
                Number(1f64),
                Func("sin".into(), Some(1)),
                Number(2f64),
                Func("cos".into(), Some(1)),
                Func("max".into(), Some(2))
            ]
        );

        assert!(to_rpn(&[Binary(Plus)]).is_err());
        assert!(to_rpn(&[Func("f".into(), None), Binary(Plus), RParen]).is_err());
        assert!(to_rpn(&[Var("x".into()), Number(1.)]).is_err());
        assert!(to_rpn(&[LParen]).is_err());
        assert!(to_rpn(&[RParen]).is_err());
        assert!(to_rpn(&[Func("sin".into(), None)]).is_err());
        assert!(to_rpn(&[Comma]).is_err());
        assert!(to_rpn(&[Func("f".into(), None), Comma]).is_err());
        assert!(to_rpn(&[Func("f".into(), None), LParen, Comma, RParen]).is_err());
    }
}
