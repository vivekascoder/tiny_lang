use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenType};
use anyhow::{anyhow, bail, Result};

pub struct Parser {
    lexer: Lexer,
    current_token: TokenType,
    next_token: TokenType,
    errors: Vec<anyhow::Error>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        Self {
            lexer: Lexer::new(source),
            current_token: TokenType::EOF,
            next_token: TokenType::EOF,
            errors: vec![],
        }
    }

    fn bump(&mut self) -> Result<()> {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next()?.type_;
        Ok(())
    }

    fn expect_next_token(&mut self, token: &TokenType) -> Result<bool> {
        if self.next_token_is(token) {
            self.bump()?;
            Ok(true)
        } else {
            self.error_next_token(token);
            Ok(false)
        }
    }

    fn error_next_token(&mut self, tok: &TokenType) {
        self.errors.push(anyhow!(
            "expected {:?}, but got {:?}",
            tok,
            &self.next_token
        ))
    }

    fn next_token_is(&self, token: &TokenType) -> bool {
        self.next_token == *token
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        // the current token is `let`
        let name = match &self.next_token {
            TokenType::Identifier(name) => name.clone(),
            _ => panic!("identifier isn't here."),
        };
        self.bump()?;

        // next token should be assign.
        if !self.expect_next_token(&TokenType::Equal)? {
            panic!("next token is not assign/equal.")
        }

        // next token should be expression.
        self.bump()?;
        // current token should be expression

        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_next_token(&TokenType::SemiColon)? {
            panic!("`;` not found")
        }

        Ok(Statement::Let(Ident(name), expr))
    }

    fn parse_expression_statement(&self) -> Result<Statement> {
        todo!()
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        Ok(match self.current_token {
            TokenType::KeywordLet => self.parse_let_statement()?,
            // leave the rest for now.
            _ => self.parse_expression_statement()?,
        })
    }

    fn token_to_precedence(tok: &TokenType) -> Precedence {
        match tok {
            TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
            TokenType::LessThan | TokenType::LessThanEqual => Precedence::LessGreater,
            TokenType::GreaterThan | TokenType::GreaterThanEqual => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Divide | TokenType::Multiply => Precedence::Product,
            TokenType::Lbracket => Precedence::Index,
            TokenType::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn current_token_precedence(&self) -> Precedence {
        Self::token_to_precedence(&self.current_token)
    }

    fn next_token_precedence(&self) -> Precedence {
        Self::token_to_precedence(&self.next_token)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr> {
        // Parse the left
        let mut left = match self.current_token {
            TokenType::Usize(val) => Expr::Literal(Literal::SignedInteger(val)),
            _ => {
                panic!(
                    "no prefix parse function found for {:?}",
                    self.current_token
                );
            }
        };

        // Parse the infix
        while !self.next_token_is(&TokenType::SemiColon)
            && precedence < self.next_token_precedence()
        {
            match self.next_token {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Divide
                | TokenType::Multiply
                | TokenType::Equal
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual => {
                    self.bump()?;

                    let infix = match self.current_token {
                        TokenType::Plus => Infix::Plus,
                        TokenType::Minus => Infix::Minus,
                        TokenType::Divide => Infix::Divide,
                        TokenType::Multiply => Infix::Multiply,
                        TokenType::Equal => Infix::Equal,
                        TokenType::NotEqual => Infix::NotEqual,
                        TokenType::LessThan => Infix::LessThan,
                        TokenType::LessThanEqual => Infix::LessThanEqual,
                        TokenType::GreaterThan => Infix::GreaterThan,
                        TokenType::GreaterThanEqual => Infix::GreaterThanEqual,
                        _ => {
                            panic!("Some errror");
                        }
                    };

                    let precedence = self.current_token_precedence();
                    self.bump()?;

                    match self.parse_expression(precedence) {
                        Ok(expr) => return Ok(Expr::Infix(infix, Box::new(left), Box::new(expr))),
                        Err(_) => {
                            panic!("Can't parse the expression");
                        }
                    }
                }
                _ => return Ok(left),
            }
        }

        Ok(left)
    }

    /// main function to parse a lexer.
    pub fn parse(&self) {
        let program: Program = vec![];
        loop {
            // if self.cur == self.lexer.len() {
            //     break;
            // }

            // // Recursive descennt parser
            // match self.lexer[self.cur].type_ {
            //     TokenType::KeywordLet => {
            //         // Parse let statement
            //     }
            //     _ => {
            //         todo!("Hmm")
            //     }
            // }
        }
    }
}
#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_let_statement_parsing() {}
}
