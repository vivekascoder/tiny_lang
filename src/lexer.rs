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
            _ => None,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.source[self.cur] {
                // Do nothing for whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    self.cur += 1;
                }
                _ => {
                    break;
                }
            }
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        if self.cur == self.source.len() {
            return Ok(self.token(TokenType::EOF, (self.cur, self.cur)));
            // bail!("EOF reached");
        }

        // Skip all the whitespaces till the next token.
        self.skip_whitespace();

        match self.source[self.cur] {
            ';' => {
                self.cur += 1;
                Ok(self.token(TokenType::SemiColon, (self.cur - 1, self.cur)))
            }
            ':' => {
                self.cur += 1;
                Ok(self.token(TokenType::Colon, (self.cur - 1, self.cur)))
            }
            '=' => {
                self.cur += 1;
                Ok(self.token(TokenType::Equal, (self.cur - 1, self.cur)))
            }
            '+' => {
                self.cur += 1;
                Ok(self.token(TokenType::Plus, (self.cur - 1, self.cur)))
            }
            '-' => {
                self.cur += 1;
                Ok(self.token(TokenType::Minus, (self.cur - 1, self.cur)))
            }
            '*' => {
                self.cur += 1;
                Ok(self.token(TokenType::Multiply, (self.cur - 1, self.cur)))
            }
            '/' => {
                self.cur += 1;
                Ok(self.token(TokenType::Divide, (self.cur - 1, self.cur)))
            }
            '!' => {
                self.cur += 1;
                Ok(self.token(TokenType::Bang, (self.cur - 1, self.cur)))
            }
            '%' => {
                self.cur += 1;
                Ok(self.token(TokenType::Mod, (self.cur, self.cur + 1)))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // Identifier
                let start = self.cur;

                // Start a loop to parse the identifier.
                loop {
                    match self.source[self.cur] {
                        // TODO: Should break if EOF?
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            self.cur += 1;
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
                    match self.source[self.cur] {
                        '0'..='9' => {
                            self.cur += 1;
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
                bail!("Illegal token found: {:?}", self.source[self.cur]);
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
            Err(_) => None,
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

    // fn test_parsing_let_without_type() {
    //     let source = "let some_var = 345 + 35353;";
    //     let mut tokenizer = Lexer::new(source);
    //     let tokens = tokenizer.tokenize();
    //     println!("tokens: {:?}", &tokens);

    //     assert_eq!(
    //         tokens,
    //         vec![
    //             Token {
    //                 type_: KeywordLet,
    //                 pos: (0, 3)
    //             },
    //             Token {
    //                 type_: TokenType::Identifier(format!("some_var")),

    //                 pos: (4, 12)
    //             },
    //             Token {
    //                 type_: TokenType::Equal,
    //                 pos: (21, 22)
    //             },
    //             Token {
    //                 type_: TokenType::Usize(345),
    //                 pos: (23, 26)
    //             },
    //             Token {
    //                 type_: TokenType::Plus,
    //                 pos: (27, 28)
    //             },
    //             Token {
    //                 type_: TokenType::Usize(35353),
    //                 pos: (29, 34)
    //             },
    //             Token {
    //                 type_: TokenType::SemiColon,
    //                 pos: (34, 35)
    //             }
    //         ]
    //     );
    // }
}
