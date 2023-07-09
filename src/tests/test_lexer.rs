#[cfg(test)]
pub mod tests {
    use crate::ast::TokenType::*;
    use crate::ast::*;
    use crate::lexer::Lexer;
    use std::result::Result::Ok;

    #[test]
    fn test_lexing_let_syntax() {
        let source = "let some_var: usize = 345 + 35353;";
        let mut tokenizer = Lexer::new(source);

        let result = vec![
            Token {
                type_: KeywordLet,
                pos: (0, 3),
            },
            Token {
                type_: TokenType::Identifier(format!("some_var")),
                pos: (4, 12),
            },
            Token {
                type_: TokenType::Colon,
                pos: (12, 13),
            },
            Token {
                type_: KeywordUsize,
                pos: (14, 19),
            },
            Token {
                type_: TokenType::Equal,
                pos: (20, 21),
            },
            Token {
                type_: TokenType::Usize(345),
                pos: (22, 25),
            },
            Token {
                type_: TokenType::Plus,
                pos: (26, 27),
            },
            Token {
                type_: TokenType::Usize(35353),
                pos: (28, 33),
            },
            Token {
                type_: TokenType::SemiColon,
                pos: (33, 34),
            },
            Token {
                type_: TokenType::EOF,
                pos: (34, 34),
            },
        ];
        let mut output = vec![];
        while let Ok(token) = tokenizer.next() {
            output.push(token.clone());
            if token.type_ == TokenType::EOF {
                break;
            }
        }
        println!("{:?}", &output);
        assert_eq!(output, result);
    }

    #[test]
    fn test_function_declaration() {
        let source = r#"
        fun calculate_something(a: usize, b: usize) => bool {
            if (a > b) {
                return true;
            } else {
                return false;
            }
        }
        "#;
        let tokens: Vec<Token> = Lexer::new(source).into_iter().collect();

        assert_eq!(
            tokens,
            [
                Token {
                    type_: KeywordFun,
                    pos: (9, 12,),
                },
                Token {
                    type_: Identifier("calculate_something".to_string()),
                    pos: (13, 32,),
                },
                Token {
                    type_: LParen,
                    pos: (32, 33,),
                },
                Token {
                    type_: Identifier("a".to_string()),
                    pos: (33, 34,),
                },
                Token {
                    type_: Colon,
                    pos: (34, 35,),
                },
                Token {
                    type_: KeywordUsize,
                    pos: (36, 41,),
                },
                Token {
                    type_: Comma,
                    pos: (41, 42,),
                },
                Token {
                    type_: Identifier("b".to_string()),
                    pos: (43, 44,),
                },
                Token {
                    type_: Colon,
                    pos: (44, 45,),
                },
                Token {
                    type_: KeywordUsize,
                    pos: (46, 51,),
                },
                Token {
                    type_: RParen,
                    pos: (51, 52,),
                },
                Token {
                    type_: SymbolReturn,
                    pos: (53, 55,),
                },
                Token {
                    type_: KeywordBool,
                    pos: (56, 60,),
                },
                Token {
                    type_: LBrace,
                    pos: (61, 62,),
                },
                Token {
                    type_: KeywordIf,
                    pos: (75, 77,),
                },
                Token {
                    type_: LParen,
                    pos: (78, 79,),
                },
                Token {
                    type_: Identifier("a".to_string()),
                    pos: (79, 80,),
                },
                Token {
                    type_: GreaterThan,
                    pos: (80, 81,),
                },
                Token {
                    type_: Identifier("b".to_string()),
                    pos: (83, 84,),
                },
                Token {
                    type_: RParen,
                    pos: (84, 85,),
                },
                Token {
                    type_: LBrace,
                    pos: (86, 87,),
                },
                Token {
                    type_: KeywordReturn,
                    pos: (104, 110,),
                },
                Token {
                    type_: Boolean(true,),
                    pos: (111, 115,),
                },
                Token {
                    type_: SemiColon,
                    pos: (115, 116,),
                },
                Token {
                    type_: RBrace,
                    pos: (129, 130,),
                },
                Token {
                    type_: KeywordElse,
                    pos: (131, 135,),
                },
                Token {
                    type_: LBrace,
                    pos: (136, 137,),
                },
                Token {
                    type_: KeywordReturn,
                    pos: (154, 160,),
                },
                Token {
                    type_: Boolean(false,),
                    pos: (161, 166,),
                },
                Token {
                    type_: SemiColon,
                    pos: (166, 167,),
                },
                Token {
                    type_: RBrace,
                    pos: (180, 181,),
                },
                Token {
                    type_: RBrace,
                    pos: (190, 191,),
                },
            ]
        );
    }
}
