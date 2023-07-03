use crate::lexer::TokenType;

// Precedence order
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}

pub type Program = Vec<Statement>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Literal(Literal),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    GreaterThan,
    LessThanEqual,
    LessThan,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    SignedInteger(usize),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    // Literal Values
    Usize(usize),
    Isize(isize),
    // PrefixExpression(PrefixExpression)
}

// #[derive(Clone, Copy, PartialEq)]
// struct PrefixExpression {
//     operator:
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
    // Let token
    token: TokenType,
    // Identifier/name
    identifier: String,
    value: Expression,
}
