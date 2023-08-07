use logos::{Lexer, Logos};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexError {
    CharParseError,
    #[default]
    Other,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"([ \t\r\n]*|//[^\n]*)")]
pub enum TokenKind<'a> {
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,
    #[regex(r#"(?:")(?:\\.|[^"])*""#, TokenKind::to_str)]
    Str(&'a str),
    #[regex(r#"'(?:\\.|[^'])'"#, TokenKind::to_char)]
    Char(char),

    #[regex("-?[0-9]+", TokenKind::to_number)]
    Number(u64),
    #[regex("(true|false)", TokenKind::to_bool)]
    Bool(bool),
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,

    // Operators
    #[token("=")]
    Equal,
    #[token("==")]
    DoubleEqual,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("%")]
    Mod,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanEqual,
    #[token("!")]
    Bang,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace, // {
    #[token("}")]
    RBrace, // }
    #[token(",")]
    Comma,

    // Not needed?
    // #[token("\"")]
    // DQuote, // "
    // #[token("'")]
    // SQuote, // '
    #[token("\\")]
    BSlash, // \
    #[token("<<")]
    LeftShift, // <<
    #[token(">>")]
    RightShift, // >>
    #[token("&")]
    Ampersand, // &
    #[token("^")]
    Carrot, // ^
    #[token("|")]
    Pipe, // |

    // Keywords
    #[token("let")]
    KeywordLet,
    #[token("usize")]
    KeywordUsize,
    #[token("isize")]
    KeywordIsize,
    #[token("bool")]
    KeywordBool,
    #[token("fun")]
    KeywordFun,
    #[token("return")]
    KeywordReturn,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("void")]
    KeywordVoid,
    #[token("char")]
    KeywordChar,
    #[token("while")]
    KeywordWhile,

    // Symbol
    #[token("=>")]
    SymbolReturn,
}

impl<'a> TokenKind<'a> {
    fn to_number(lex: &mut Lexer<'a, TokenKind<'a>>) -> Option<u64> {
        lex.slice().parse().ok()
    }
    fn to_bool(lex: &mut Lexer<'a, TokenKind<'a>>) -> Option<bool> {
        lex.slice().parse().ok()
    }

    fn to_str(lex: &mut Lexer<'a, TokenKind<'a>>) -> Option<&'a str> {
        let slice = lex.slice();
        Some(&slice[1..slice.len() - 1])
    }
    fn to_char(lex: &mut Lexer<'a, TokenKind<'a>>) -> Result<char, LexError> {
        let slice: Vec<char> = lex.slice().chars().collect();
        match slice.len() {
            3 => Ok(slice[1]),
            4 => match slice[2] {
                'n' => Ok('\n'),
                't' => Ok('\t'),
                'r' => Ok('\r'),
                '"' => Ok('\"'),
                '\'' => Ok('\''),
                '\\' => Ok('\\'),
                _ => Err(LexError::CharParseError),
            },
            _ => Err(LexError::CharParseError),
        }
    }
}
