// Types for our language.

use std::fmt::Display;

pub type Program = Vec<Statement>;
pub type BlockStatement = Vec<Statement>;

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

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Prefix(Prefix, Box<Expr>),
    Literal(Literal),
    Call(FunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub parameters: Vec<Expr>,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunctionCall {
    pub parameters: Vec<ExprResult>,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprResult {
    Bool(bool),
    UnsignedInteger(usize),
    Void,
    Return(Box<ExprResult>),
    Char(char),
}

impl Display for ExprResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::UnsignedInteger(i) => write!(f, "{}", i),
            Self::Void => write!(f, ""),
            Self::Return(v) => write!(f, "{}", *v),
            Self::Char(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemoryObject {
    ExprResult(ExprResult),
    Function(Function),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    DoubleEqual,
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
    UnsignedInteger(usize),
    Bool(bool),
    Char(char),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expr),
    Function(Function),
    Return(Expr),
    If(Condition),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition {
    pub condition: Expr,
    pub if_body: BlockStatement,
    pub else_body: Option<BlockStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    UnsignedInteger,
    Bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    Identifier(String),
    Usize(usize),
    Boolean(bool),
    Colon,
    SemiColon,

    // Operators
    Equal,
    DoubleEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Bang,
    LParen,
    RParen,
    LBrace, // {
    RBrace, // }
    Comma,
    SQuote, // '
    DQuote, // "

    // Keywords
    KeywordLet,
    KeywordUsize,
    KeywordBool,
    KeywordFun,
    KeywordReturn,
    KeywordIf,
    KeywordElse,
    KeywordVoid,

    // Symbol
    SymbolReturn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub type_: TokenType,
    pub pos: (usize, usize),
}

impl Token {
    pub fn new(type_: TokenType, pos: (usize, usize)) -> Self {
        Self { type_, pos }
    }
}
