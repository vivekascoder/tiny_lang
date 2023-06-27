#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    Identifier(String),
    Number(usize),
    Colon,
    SemiColon,
    // Operator
    Equal,
    Plus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    type_: TokenType,
    pos: (usize, usize),
}

impl Token {
    pub fn new(type_: TokenType, pos: (usize, usize)) -> Self {
        Self { type_, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Tokenizer {
    // To track the current index at the string
    cur: usize,
    source: Vec<char>,
}

/// Parse `let var1: number = 34 + 353;`
impl Tokenizer {
    pub fn new(s: &str) -> Self {
        Self {
            cur: 0,
            source: s.chars().collect(),
        }
    }

    fn token(&self, type_: TokenType, pos: (usize, usize)) -> Token {
        Token::new(type_, pos)
    }

    fn trim_whitespace(&mut self) {
        let mut pos = 0;
        loop {
            if pos == self.source.len() {
                break;
            }

            // match self.source[self.cur] {
            //     ' ' =>
            // }

            pos += 1;
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        // Trim whitespace first
        self.trim_whitespace();

        loop {
            if self.cur == self.source.len() {
                break;
            }

            println!("{:?}", self.source[self.cur]);

            match self.source[self.cur] {
                ':' => {
                    tokens.push(self.token(TokenType::Colon, (self.cur, self.cur + 1)));
                    self.cur += 1;
                }
                '=' => {
                    tokens.push(self.token(TokenType::Equal, (self.cur, self.cur + 1)));
                    self.cur += 1;
                }
                '+' => {
                    tokens.push(self.token(TokenType::Plus, (self.cur, self.cur + 1)));
                    self.cur += 1;
                }
                ';' => {
                    tokens.push(self.token(TokenType::SemiColon, (self.cur, self.cur + 1)));
                    self.cur += 1;
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
                            _ => {
                                let slice = &self.source[start..self.cur];
                                tokens.push(self.token(
                                    TokenType::Identifier(slice.iter().collect()),
                                    (start, self.cur),
                                ));
                                break;
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
                                let number: usize =
                                    s_no.iter().collect::<String>().parse().unwrap();

                                tokens
                                    .push(self.token(TokenType::Number(number), (start, self.cur)));
                                break;
                            }
                        }
                    }
                }

                // Do nothing for whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    self.cur += 1;
                }
                _ => unreachable!(),
            }
        }
        tokens
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_parsing_let_syntax() {
        let source = "let some_var: number = 345 + 35353;";
        let mut tokenizer = Tokenizer::new(source);
        // println!("{:?}", tokenizer.tokenize());
        assert_eq!(
            tokenizer.tokenize(),
            vec![
                Token {
                    type_: TokenType::Identifier(format!("let")),
                    pos: (0, 3)
                },
                Token {
                    type_: TokenType::Identifier(format!("some_var")),

                    pos: (4, 12)
                },
                Token {
                    type_: TokenType::Colon,
                    pos: (12, 13)
                },
                Token {
                    type_: TokenType::Identifier(format!("number")),
                    pos: (14, 20)
                },
                Token {
                    type_: TokenType::Equal,
                    pos: (21, 22)
                },
                Token {
                    type_: TokenType::Number(345),
                    pos: (23, 26)
                },
                Token {
                    type_: TokenType::Plus,
                    pos: (27, 28)
                },
                Token {
                    type_: TokenType::Number(35353),
                    pos: (29, 34)
                },
                Token {
                    type_: TokenType::SemiColon,
                    pos: (34, 35)
                }
            ]
        );
    }
}
