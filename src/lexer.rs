use crate::ast::*;
use anyhow::{bail, Result};

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer {
    // To track the current index at the string
    cur: usize,
    source: Vec<char>,
}

/// Parse `let var1: number = 34 + 353;`
impl Lexer {
    pub fn new(s: &str) -> Self {
        Self {
            cur: 0,
            source: s.chars().collect(),
        }
    }

    fn token(&self, type_: TokenType, pos: (usize, usize)) -> Token {
        Token::new(type_, pos)
    }

    fn parse_keyword(&self, s: &str) -> Option<TokenType> {
        match s {
            "let" => Some(TokenType::KeywordLet),
            "usize" => Some(TokenType::KeywordUsize),
            "fun" => Some(TokenType::KeywordFun),
            "return" => Some(TokenType::KeywordReturn),
            "if" => Some(TokenType::KeywordIf),
            "else" => Some(TokenType::KeywordElse),
            "true" => Some(TokenType::Boolean(true)),
            "false" => Some(TokenType::Boolean(false)),
            _ => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.eof_reached() {
            match self.current() {
                // Do nothing for whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    self.bump();
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn current(&self) -> char {
        self.source[self.cur]
    }

    fn bump(&mut self) {
        // if self.eof_reached() {

        // }
        self.cur += 1;
    }

    fn eof_reached(&self) -> bool {
        self.cur == self.source.len()
    }

    pub fn next(&mut self) -> Result<Token> {
        // Skip all the whitespaces till the next token.
        self.skip_whitespace();

        if self.eof_reached() {
            return Ok(self.token(TokenType::EOF, (self.cur, self.cur)));
            // bail!("EOF reached");
        }

        match self.current() {
            ';' => {
                self.bump();
                Ok(self.token(TokenType::SemiColon, (self.cur - 1, self.cur)))
            }
            ':' => {
                self.bump();
                Ok(self.token(TokenType::Colon, (self.cur - 1, self.cur)))
            }
            '=' => {
                self.bump();
                match self.current() {
                    '>' => {
                        self.bump();
                        Ok(self.token(TokenType::SymbolReturn, (self.cur - 2, self.cur)))
                    }
                    '=' => {
                        self.bump();
                        Ok(self.token(TokenType::DoubleEqual, (self.cur - 2, self.cur)))
                    }
                    _ => Ok(self.token(TokenType::Equal, (self.cur - 2, self.cur - 1))),
                }
            }

            '(' => {
                self.bump();
                Ok(self.token(TokenType::LParen, (self.cur - 1, self.cur)))
            }

            ')' => {
                self.bump();
                Ok(self.token(TokenType::RParen, (self.cur - 1, self.cur)))
            }

            '<' => {
                self.bump();
                Ok(self.token(TokenType::LessThan, (self.cur - 1, self.cur)))
            }

            '>' => {
                self.bump();

                match self.current() {
                    '=' => {
                        self.bump();
                        Ok(self.token(TokenType::GreaterThanEqual, (self.cur - 2, self.cur)))
                    }
                    _ => Ok(self.token(TokenType::GreaterThan, (self.cur - 2, self.cur - 1))),
                }
            }

            '{' => {
                self.bump();
                Ok(self.token(TokenType::LBrace, (self.cur - 1, self.cur)))
            }

            '}' => {
                self.bump();
                Ok(self.token(TokenType::RBrace, (self.cur - 1, self.cur)))
            }

            ',' => {
                self.bump();
                Ok(self.token(TokenType::Comma, (self.cur - 1, self.cur)))
            }

            '+' => {
                self.bump();
                Ok(self.token(TokenType::Plus, (self.cur - 1, self.cur)))
            }
            '-' => {
                self.bump();
                Ok(self.token(TokenType::Minus, (self.cur - 1, self.cur)))
            }
            '*' => {
                self.bump();
                Ok(self.token(TokenType::Multiply, (self.cur - 1, self.cur)))
            }
            '/' => {
                self.bump();
                Ok(self.token(TokenType::Divide, (self.cur - 1, self.cur)))
            }
            '!' => {
                self.bump();
                Ok(self.token(TokenType::Bang, (self.cur - 1, self.cur)))
            }
            '%' => {
                self.bump();
                Ok(self.token(TokenType::Mod, (self.cur, self.cur + 1)))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier
                let start = self.cur;

                // Start a loop to parse the identifier.
                loop {
                    match self.current() {
                        // TODO: Should break if EOF?
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            self.bump();
                        }

                        // Encountered something else
                        // Then it could be either identifier or keyword.
                        _ => {
                            let slice = &self.source[start..self.cur];
                            let identifier: String = slice.iter().collect();

                            if let Some(keyword) = self.parse_keyword(&identifier) {
                                return Ok(self.token(keyword, (start, self.cur)));
                            } else {
                                return Ok(self
                                    .token(TokenType::Identifier(identifier), (start, self.cur)));
                            }
                        }
                    }
                }
            }
            '0'..='9' => {
                // Number
                let start = self.cur;

                loop {
                    match self.current() {
                        '0'..='9' => {
                            self.bump();
                        }

                        // Encountered any other char.
                        _ => {
                            let s_no = &self.source[start..self.cur];
                            let number: usize = s_no.iter().collect::<String>().parse().unwrap();

                            return Ok(self.token(TokenType::Usize(number), (start, self.cur)));
                        }
                    }
                }
            }

            _ => {
                bail!("Illegal token found: {:?}", self.current());
            }
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next() {
            Ok(t) => {
                if t.type_ == TokenType::EOF {
                    None
                } else {
                    Some(t)
                }
            }
            Err(e) => {
                panic!("expected Token but got error: {:?}", e);
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::TokenType::*;
    use super::*;
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
                    type_: Identifier("bool".to_string()),
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
