use std::rc::Rc;

use crate::ast::*;
use anyhow::{bail, Result};
use log::info;

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer {
    // To track the current index at the string
    cur: usize,
    row: usize,
    col: usize,
    module: String,
    source: Vec<char>,
}

/// Parse `let var1: number = 34 + 353;`
impl Lexer {
    pub fn new(module: &str, s: &str) -> Self {
        Self {
            cur: 0,
            row: 1,
            col: 0,
            module: module.to_string(),
            source: s.chars().collect(),
        }
    }

    pub fn module(&self) -> String {
        self.module.clone()
    }

    pub fn get_row(&self) -> usize {
        self.row
    }

    pub fn get_col(&self) -> usize {
        self.col
    }

    fn token(&self, type_: TokenType, pos: (usize, usize)) -> Token {
        Token {
            type_: type_,
            pos: pos,
            row: self.row,
            col: self.col,
        }
    }

    fn parse_keyword(&self, s: &str) -> Option<TokenType> {
        match s {
            "let" => Some(TokenType::KeywordLet),
            "usize" => Some(TokenType::KeywordUsize),
            "isize" => Some(TokenType::KeywordIsize),
            "i8" => Some(TokenType::KeywordI8),
            "fun" => Some(TokenType::KeywordFun),
            "return" => Some(TokenType::KeywordReturn),
            "if" => Some(TokenType::KeywordIf),
            "else" => Some(TokenType::KeywordElse),
            "true" => Some(TokenType::Boolean(true)),
            "false" => Some(TokenType::Boolean(false)),
            "bool" => Some(TokenType::KeywordBool),
            "void" => Some(TokenType::KeywordVoid),
            "char" => Some(TokenType::KeywordChar),
            "while" => Some(TokenType::KeywordWhile),
            "str" => Some(TokenType::KeywordStr),
            "extern" => Some(TokenType::KeywordExtern),
            "struct" => Some(TokenType::KeywordStruct),
            "string" => Some(TokenType::KeywordStr),
            _ => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.eof_reached() {
            match self.current() {
                // Do nothing for whitespace
                ' ' | '\t' | '\r' => {
                    self.bump();
                }
                '\n' => {
                    self.col = 0;
                    self.row += 1;
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
        self.col += 1;
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
        // info!("current token: {:?}", self.current());
        match self.current() {
            ';' => {
                self.bump();
                Ok(self.token(TokenType::SemiColon, (self.cur - 1, self.cur)))
            }
            ':' => {
                self.bump();
                Ok(self.token(TokenType::Colon, (self.cur - 1, self.cur)))
            }

            '.' => {
                let start = self.cur;
                self.bump();
                loop {
                    match self.current() {
                        '.' => {
                            self.bump();
                            continue;
                        }
                        _ => {
                            let periods = &self.source[start..self.cur];
                            if periods.len() == 1 {
                                return Ok(self.token(TokenType::Period, (self.cur - 1, self.cur)));
                            } else if periods.len() == 3 {
                                return Ok(self
                                    .token(TokenType::VariableArg, (self.cur - start, self.cur)));
                            } else {
                                bail!("invalid token `{}`", periods.iter().collect::<String>());
                            }
                        }
                    }
                }
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
                    _ => Ok(self.token(TokenType::Equal, (self.cur - 1, self.cur))),
                }
            }

            '(' => {
                self.bump();
                Ok(self.token(TokenType::LParen, (self.cur - 1, self.cur)))
            }

            '\"' => {
                let start = self.cur;
                loop {
                    self.bump();
                    if self.current() == '\"' {
                        let val: String = (&self.source[start + 1..self.cur]).iter().collect();
                        self.bump();
                        return Ok(self.token(TokenType::String(val.into()), (start, self.cur)));
                    }
                }
            }

            // tokenize whitespace char.
            '\\' => {
                info!("encounterd \\");
                self.bump();
                info!("cur encounterd {}", self.current());
                match self.current() {
                    'n' => {
                        self.bump();
                        return Ok(self.token(TokenType::NewLine, (self.cur - 2, self.cur)));
                    }
                    't' => {
                        self.bump();
                        return Ok(self.token(TokenType::Tab, (self.cur - 2, self.cur)));
                    }
                    // Rust doesn't have `\v` ?
                    // 'v' => Ok(self.token(TokenType::VTab, (self.cur - 2, self.cur))),
                    '\\' => {
                        self.bump();
                        Ok(self.token(TokenType::BSlash, (self.cur - 2, self.cur)))
                    }
                    _ => {
                        bail!("`\\{:?}` is not a valid character.", self.current());
                    }
                }
            }

            '\'' => {
                self.bump();
                Ok(self.token(TokenType::SQuote, (self.cur - 1, self.cur)))
            }

            ')' => {
                self.bump();
                Ok(self.token(TokenType::RParen, (self.cur - 1, self.cur)))
            }

            '&' => {
                self.bump();
                Ok(self.token(TokenType::Ampersand, (self.cur - 1, self.cur)))
            }

            '|' => {
                self.bump();
                Ok(self.token(TokenType::Pipe, (self.cur - 1, self.cur)))
            }

            '^' => {
                self.bump();
                Ok(self.token(TokenType::Carrot, (self.cur - 1, self.cur)))
            }

            '<' => {
                self.bump();
                match self.current() {
                    '<' => {
                        self.bump();
                        Ok(self.token(TokenType::LeftShift, (self.cur - 2, self.cur)))
                    }
                    _ => Ok(self.token(TokenType::LessThan, (self.cur - 1, self.cur))),
                }
            }

            '>' => {
                self.bump();

                match self.current() {
                    '=' => {
                        self.bump();
                        Ok(self.token(TokenType::GreaterThanEqual, (self.cur - 2, self.cur)))
                    }
                    '>' => {
                        self.bump();
                        Ok(self.token(TokenType::RightShift, (self.cur - 2, self.cur)))
                    }
                    _ => Ok(self.token(TokenType::GreaterThan, (self.cur - 1, self.cur))),
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
                match self.current() {
                    // single line comment
                    '/' => loop {
                        self.bump();
                        match self.current() {
                            '\n' => {
                                self.bump();
                                return Ok(self.next()?);
                            }
                            _ => {
                                continue;
                            }
                        };
                    },

                    // multi line comment
                    '*' => loop {
                        match self.current() {
                            '*' => {
                                self.bump();
                                if self.current() == '/' {
                                    self.bump();
                                    return Ok(self.next()?);
                                } else {
                                    continue;
                                }
                            }
                            _ => {
                                self.bump();
                                continue;
                            }
                        };
                    },
                    _ => Ok(self.token(TokenType::Divide, (self.cur - 1, self.cur))),
                }
            }
            '!' => {
                self.bump();
                match self.current() {
                    '=' => {
                        self.bump();
                        Ok(self.token(TokenType::NotEqual, (self.cur - 2, self.cur)))
                    }
                    _ => Ok(self.token(TokenType::Bang, (self.cur - 1, self.cur))),
                }
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
                                return Ok(self.token(
                                    TokenType::Identifier(Rc::from(identifier.as_str())),
                                    (start, self.cur),
                                ));
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
