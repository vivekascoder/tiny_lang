use chumsky::prelude::*;

// fn parser<'a, I>() -> impl Parser<'a, I, SExpr, extra::Err<Rich<'a, Token<'a>>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
// {
//     recursive(|sexpr| {
//         // let atom = select! {
//         //     Token::Float(x) => SExpr::Float(x.parse().unwrap()),
//         //     Token::Add => SExpr::Add,
//         //     Token::Sub => SExpr::Sub,
//         //     Token::Mul => SExpr::Mul,
//         //     Token::Div => SExpr::Div,
//         // };

//         // let list = sexpr
//         //     .repeated()
//         //     .collect()
//         //     .map(SExpr::List)
//         //     .delimited_by(just(Token::LParen), just(Token::RParen));

//         // atom.or(list)
//     })
// }
