// use std::str::pattern::Pattern;

// use chumsky::{
//     input::{Stream, ValueInput},
//     prelude::*,
// };
// use logos::{Lexer, Logos};

// #[derive(Debug, PartialEq, Clone)]
// enum Expr {
//     Literal(i64),
//     Infix(Infix),
//     Prefix(Prefix),
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Prefix {
//     expr: Box<Expr>,
//     op: PrefixOp,
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Infix {
//     l: Box<Expr>,
//     r: Box<Expr>,
//     op: InfixOp,
// }

// #[derive(Debug, PartialEq, Clone)]
// enum PrefixOp {
//     Plus,
//     Minus,
//     Not,
// }

// #[derive(Debug, PartialEq, Clone)]
// enum InfixOp {
//     Plus,
//     Minus,
//     Divide,
//     Multiply,
//     DoubleEqual,
//     NotEqual,
//     GreaterThanEqual,
//     GreaterThan,
//     LessThanEqual,
//     LessThan,
// }

// #[derive(Logos, Debug, PartialEq)]
// #[logos(skip r"([ \t\r\n]*|//[^\n]*)")]
// enum ExprToken {
//     #[token("+")]
//     Plus,
//     #[token("-")]
//     Sub,
//     #[token("*")]
//     Mul,
//     #[token("/")]
//     Div,
//     #[token("(")]
//     LParen,
//     #[token(")")]
//     RParen,
//     #[regex(r#"-?[0-9]+"#, to_int)]
//     Int(i64),
// }

// #[derive(Debug)]
// struct Er {
//     val: String,
// }

// impl<T: Into<Pattern>> chumsky::Error<T> for Er {}

// fn to_int(lex: &mut Lexer<ExprToken>) -> Option<i64> {
//     lex.slice().parse().ok()
// }

// fn parser() -> impl chumsky::Parser<ExprToken, Expr, Error = Er> + Clone {
//     todo!()
// }

// fn main() -> anyhow::Result<()> {
//     let code = "34 + 45 - 2 * 23 + (34 * 3)";
//     Ok(())
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::ExprToken;

//     #[test]
//     fn test_lexer() {
//         use crate::ExprToken::*;
//         let code = "34 + 45 - 2 * 23 + (34 * 3)";
//         // println!("{:?}", ExprToken::lexer(code).collect::<Vec<_>>());
//         assert_eq!(
//             ExprToken::lexer(code).collect::<Vec<_>>(),
//             [
//                 Ok(Int(34)),
//                 Ok(Plus),
//                 Ok(Int(45)),
//                 Ok(Sub),
//                 Ok(Int(2)),
//                 Ok(Mul),
//                 Ok(Int(23)),
//                 Ok(Plus),
//                 Ok(LParen),
//                 Ok(Int(34)),
//                 Ok(Mul),
//                 Ok(Int(3)),
//                 Ok(RParen)
//             ]
//         )
//     }

//     #[test]
//     fn test_parser() {
//         println!("Hey");
//     }
// }

fn main() {
    // something
    print!("something");
}
