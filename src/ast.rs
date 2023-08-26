// Types for our language.

use std::{fmt::Display, rc::Rc};

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
    LRShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
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
    pub name: Rc<str>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunctionCall {
    pub parameters: Vec<ExprResult>,
    pub name: Rc<str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprResult {
    Bool(bool),
    UnsignedInteger(usize),
    SignedInteger(isize),
    Char(char),
    Return(Box<ExprResult>),
    Void,
}

impl Display for ExprResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::UnsignedInteger(i) => write!(f, "{}", i),
            Self::SignedInteger(i) => write!(f, "{}", i),
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

    // Bitwise operations
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
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
    String(Rc<str>),
}

// enum TVec {
//     LiteralVec<Literal>
// }

// WASM specific choices
// pub enum Literal {
//     I32(i32),
//     I64(i64),
//     F32(i32),
//     F64(i64),
//     Bool(bool),
//     Char(char),
// }

#[derive(Clone, Debug, PartialEq)]
pub struct Ident(pub Rc<str>);

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Option<Type>, Expr),
    Mutate(Ident, Expr),
    Function(Function),
    Return(Expr),
    If(Condition),
    Expr(Expr),
    While(While),
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: BlockStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition {
    pub condition: Expr,
    pub if_body: BlockStatement,
    pub else_body: Option<BlockStatement>,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Type {
    UnsignedInteger,
    SignedInteger,
    Bool,
    Char,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Rc<str>,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    Identifier(Rc<str>),
    Usize(usize),
    Boolean(bool),
    String(Rc<str>),
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
    SQuote,     // '
    DQuote,     // "
    BSlash,     // \
    NewLine,    // \n
    Tab,        // \t
    Space,      // ' '
    LeftShift,  // <<
    RightShift, // >>
    Ampersand,  // &
    Carrot,     // ^
    Pipe,       // |

    // Keywords
    KeywordLet,
    KeywordUsize,
    KeywordIsize,
    KeywordBool,
    KeywordFun,
    KeywordReturn,
    KeywordIf,
    KeywordElse,
    KeywordVoid,
    KeywordChar,
    KeywordWhile,
    KeywordStr,

    // Symbol
    SymbolReturn,
}

/// implement the Debug trait for all the variants of TokenType
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenType::EOF => write!(f, "EOF"),
            TokenType::Identifier(s) => write!(f, "{}", s),
            TokenType::Usize(i) => write!(f, "{}", i),
            TokenType::Boolean(b) => write!(f, "{}", b),
            TokenType::Colon => write!(f, ":"),
            TokenType::SemiColon => write!(f, ";"),
            TokenType::Equal => write!(f, "="),
            TokenType::DoubleEqual => write!(f, "=="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Multiply => write!(f, "*"),
            TokenType::Divide => write!(f, "/"),
            TokenType::Mod => write!(f, "%"),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LessThanEqual => write!(f, "<="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::GreaterThanEqual => write!(f, ">="),
            TokenType::Bang => write!(f, "!"),
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::SQuote => write!(f, "'"),
            TokenType::DQuote => write!(f, "\""),
            TokenType::BSlash => write!(f, "\\"),
            TokenType::NewLine => write!(f, "\n"),
            TokenType::Tab => write!(f, "\t"),
            TokenType::Space => write!(f, " "),
            TokenType::KeywordLet => write!(f, "let"),
            TokenType::KeywordUsize => write!(f, "usize"),
            TokenType::KeywordIsize => write!(f, "isize"),
            TokenType::KeywordBool => write!(f, "bool"),
            TokenType::KeywordFun => write!(f, "fun"),
            TokenType::KeywordReturn => write!(f, "return"),
            TokenType::KeywordIf => write!(f, "if"),
            TokenType::KeywordElse => write!(f, "else"),
            TokenType::KeywordVoid => write!(f, "void"),
            TokenType::KeywordChar => write!(f, "chat"),
            TokenType::KeywordWhile => write!(f, "while"),
            TokenType::SymbolReturn => write!(f, "=>"),
            TokenType::String(s) => write!(f, "\"{}\"", s),
            TokenType::LeftShift => write!(f, "<<"),
            TokenType::RightShift => write!(f, ">>"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::Carrot => write!(f, "^"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::KeywordStr => write!(f, "str"),
        }
    }
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
