use crate::lexer::{Token, TokenType, Tokenizer};
use anyhow::{bail, Result};

#[derive(Clone, Debug, PartialEq)]
pub enum Program {}

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
    Let(LetStatement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
    // Let token
    token: Token,
    // Identifier/name
    identifier: String,
    value: Expression,
}

pub struct Parser {
    source: String,
    lexer: Vec<Token>,
    // Current token index
    cur: usize,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut tokenizer = Tokenizer::new(source);
        Self {
            source: source.to_string(),
            lexer: tokenizer.tokenize(),
            cur: 0,
        }
    }

    fn parse_let_statement(&self) -> Result<LetStatement> {
        // make sure the current token is a let keyword.
        if !(self.lexer[self.cur].type_ == TokenType::KeywordLet) {
            bail!(
                "Let statement parsing error, the current token is {:?} and not KeywordLet",
                self.lexer[self.cur]
            );
        }

        // The next token should be an identifier.
        let name = match &self.lexer[self.cur + 1].type_ {
            TokenType::Identifier(name) => name.clone(),
            _ => {
                bail!(
                    "Let statement parsing error, can't find the `=` operator, found {:?} instead.",
                    self.lexer[self.cur + 1]
                );
            }
        };

        // The next token should be an colon.
        if !(self.lexer[self.cur + 1].type_ == TokenType::Colon) {
            bail!(
                "Let statement parsing error, can't find the `:` operator, found {:?} instead.",
                self.lexer[self.cur + 1]
            );
        }

        // // The next token should be an keyword type.
        // if let TokenType::Identifier(name) = self.lexer[self.cur + 1].type_ {
        // } else {
        //     bail!(
        //         "Let statement parsing error, can't find the `=` operator, found {:?} instead.",
        //         self.lexer[self.cur + 1]
        //     );
        // }

        // The next token should be `=`.
        if !(self.lexer[self.cur + 1].type_ == TokenType::Equal) {
            bail!(
                "Let statement parsing error, can't find the `=` operator, found {:?} instead.",
                self.lexer[self.cur + 1]
            );
        }

        // The next token should be an `Expression`.
        // Since we support only `Number` RN let's parse that.
        // Ok(LetStatement {
        //     token: self.lexer[self.cur],
        //     identifier: name,
        //     value: ,
        // })
        todo!();
    }

    fn parse_statements(&self) -> Result<Vec<Statement>> {
        let mut statements: Vec<Statement> = vec![];
        match self.lexer[self.cur].type_ {
            TokenType::KeywordLet => {
                // Parse let statement
                statements.push(Statement::Let(self.parse_let_statement()?));
            }
            _ => {
                bail!("Can't parse this token type: {:?}", self.lexer[self.cur]);
            }
        }
        Ok(statements)
    }

    fn parse_expression(&self) -> Result<Expression> {
        todo!()
    }

    pub fn parse(&self) {
        loop {
            if self.cur == self.lexer.len() {
                break;
            }

            // Recursive descennt parser
            match self.lexer[self.cur].type_ {
                TokenType::KeywordLet => {
                    // Parse let statement
                }
                _ => {
                    todo!("Hmm")
                }
            }
        }
    }
}
