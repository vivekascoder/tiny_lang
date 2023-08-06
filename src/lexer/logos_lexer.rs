use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"([ \t\r\n]*|//[^\n]*)")]
enum Token {
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,
    #[regex(r#""(?:\\.|[^"])*""#)]
    String,
    #[regex("-?[0-9]+")]
    Number,
    #[regex("(true|false)")]
    Boolean,
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
    #[token("'")]
    SQuote, // '
    // #[token("\"")]
    // DQuote, // "
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_logos_lexer() {
        let mut lex = Token::lexer("let =");

        loop {
            if let Some(v) = lex.next() {
                println!("> {:?}, {:?}", v, lex.span());
            } else {
                break;
            }
        }
    }

    #[test]
    fn test_function_decl_and_call_logos() {
        let code = r#"
        let a = 445;
        let b = 45;
        let bool_var = false;
        let string_val = "this is a \"string";
        // let char_val = 'a';


        fun sum(a: usize, b: usize) => usize {
            let c = a + b;
            return c;
        }

        sum(a, b);
        "#;
        insta::assert_debug_snapshot!(Token::lexer(code).collect::<Vec<_>>());
    }
}
