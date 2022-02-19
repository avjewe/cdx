//! Turn an expression into a Vec<Token>

use crate::prelude::*;

/// Mathematical operations.
#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum BinaryOp {
    Plus,
    Minus,
    Times,
    Div,
    Rem,
    Pow,
    LT,
    GT,
    EQ,
    NE,
    LE,
    GE,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum UnaryOp {
    Plus,
    Minus,
    Fact,
}

/// Expression tokens.
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token {
    /// Binary operation.
    Binary(BinaryOp),
    /// Unary operation.
    Unary(UnaryOp),

    /// Left parenthesis.
    LParen,
    /// Right parenthesis.
    RParen,
    /// Comma: function argument separator
    Comma,

    /// A number.
    Number(f64),
    /// A variable.
    Var(String),
    /// A function with name and number of arguments.
    Func(String, Option<usize>),
}
impl Token {
    fn make_func(&mut self) -> bool {
        if let Self::Var(s) = self {
            *self = Self::Func(s.to_string(), None);
            true
        } else {
            false
        }
    }
}

const fn can_binary(tok: &Token) -> bool {
    use Token::*;
    match tok {
        Binary(_) => false,
        Unary(_) => false,
        LParen => false,
        RParen => true,
        Comma => false,
        Number(_) => true,
        Var(_) => true,
        Func(_, _) => true,
    }
}
pub(crate) fn tokenize<S: AsRef<str>>(orig: S) -> Result<Vec<Token>> {
    let orig = orig.as_ref();
    let mut input = orig;
    let mut res: Vec<Token> = vec![];

    while !input.is_empty() {
        let prev = input.len();
        let ch = input.take_first();
        let next = if input.is_empty() {
            0 as char
        } else {
            input.first()
        };
        match ch {
            '(' => {
                if !res.is_empty() {
                    let last = res.len() - 1;
                    if !res[last].make_func() {
                        res.push(Token::LParen);
                    }
                } else {
                    res.push(Token::LParen);
                }
            }
            ')' => res.push(Token::RParen),
            ',' => res.push(Token::Comma),
            '^' => res.push(Token::Binary(BinaryOp::Pow)),
            '%' => res.push(Token::Binary(BinaryOp::Rem)),
            '/' => res.push(Token::Binary(BinaryOp::Div)),
            '*' => res.push(Token::Binary(BinaryOp::Times)),
            '+' => {
                if res.is_empty() || !can_binary(&res[res.len() - 1]) {
                    res.push(Token::Unary(UnaryOp::Plus));
                } else {
                    res.push(Token::Binary(BinaryOp::Plus));
                }
            }
            '-' => {
                if res.is_empty() || !can_binary(&res[res.len() - 1]) {
                    res.push(Token::Unary(UnaryOp::Minus));
                } else {
                    res.push(Token::Binary(BinaryOp::Minus));
                }
            }
            '!' => {
                if next == '=' {
                    res.push(Token::Binary(BinaryOp::NE));
                    input.skip_first();
                } else {
                    res.push(Token::Unary(UnaryOp::Fact));
                }
            }
            '<' => {
                if next == '=' {
                    res.push(Token::Binary(BinaryOp::LE));
                    input.skip_first();
                } else {
                    res.push(Token::Binary(BinaryOp::LT));
                }
            }
            '>' => {
                if next == '=' {
                    res.push(Token::Binary(BinaryOp::GE));
                    input.skip_first();
                } else {
                    res.push(Token::Binary(BinaryOp::GT));
                }
            }
            '=' => {
                if next == '=' {
                    res.push(Token::Binary(BinaryOp::EQ));
                    input.skip_first();
                } else {
                    return err!("Equal can't be followed by {} in '{}'", next, orig);
                }
            }
            _ => {
                if ch.is_alphabetic() {
                    let mut var = String::new();
                    var.push(ch);
                    while !input.is_empty() && input.first().is_alphanumeric() {
                        var.push(input.take_first());
                    }
                    res.push(Token::Var(var));
                } else if ch.is_ascii_alphanumeric() {
                    let (x, y) = orig[orig.len() - prev..].to_f64();
                    input = y;
                    res.push(Token::Number(x));
                } else {
                    return err!("Unrecognized character {ch} in {orig}");
                }
            }
        }
    }
    Ok(res)
}
