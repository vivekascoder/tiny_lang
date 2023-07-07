use crate::ast::*;
use crate::lexer::Lexer;
use anyhow::{anyhow, bail, Result};

pub struct Parser {
    lexer: Lexer,
    current_token: TokenType,
    next_token: TokenType,
    errors: Vec<anyhow::Error>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(source),
            current_token: TokenType::EOF,
            next_token: TokenType::EOF,
            errors: vec![],
        };
        parser.bump().unwrap();
        parser.bump().unwrap();
        parser
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
    fn current_token_is(&self, token: &TokenType) -> bool {
        self.current_token == *token
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
            bail!("`;` not found")
        }

        Ok(Statement::Let(Ident(name), expr))
    }

    fn parse_expression_statement(&self) -> Result<Statement> {
        todo!()
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        println!("Current Token: {:?}", self.current_token);
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
            TokenType::LBrace => Precedence::Index,
            TokenType::LParen => Precedence::Call,
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
            TokenType::Usize(val) => Expr::Literal(Literal::UnsignedInteger(val)),
            TokenType::Minus | TokenType::Bang | TokenType::Plus => {
                // Parse prefix expression.

                // Operator
                let prefix = match self.current_token {
                    TokenType::Bang => Prefix::Not,
                    TokenType::Plus => Prefix::Plus,
                    TokenType::Minus => Prefix::Minus,
                    _ => panic!("not a valid infix operator."),
                };
                self.bump()?;

                // Expression
                match self.parse_expression(Precedence::Prefix) {
                    Ok(val) => {
                        return Ok(Expr::Prefix(prefix, Box::new(val)));
                    }
                    Err(e) => bail!("error while parsing expression for prefix with {:#?}", e),
                }
            }
            _ => {
                bail!(
                    "no prefix parse function found for {:?}",
                    self.current_token
                );
            }
        };
        println!("Left: {:?}", left);

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
                        TokenType::DoubleEqual => Infix::DoubleEqual,
                        TokenType::NotEqual => Infix::NotEqual,
                        TokenType::LessThan => Infix::LessThan,
                        TokenType::LessThanEqual => Infix::LessThanEqual,
                        TokenType::GreaterThan => Infix::GreaterThan,
                        TokenType::GreaterThanEqual => Infix::GreaterThanEqual,
                        _ => {
                            bail!("not a valid infix operator.");
                        }
                    };
                    println!("Infix: {:?}", infix);

                    let precedence = self.current_token_precedence();
                    self.bump()?;

                    left = match self.parse_expression(precedence) {
                        Ok(expr) => Expr::Infix(infix, Box::new(left), Box::new(expr)),
                        Err(e) => {
                            bail!("error while parsing expr: {:?}", e);
                        }
                    }
                }
                _ => return Ok(left),
            }
        }

        Ok(left)
    }

    /// main function to parse a lexer.
    pub fn parse(&mut self) -> Result<Program> {
        let mut program: Program = vec![];
        while !self.current_token_is(&TokenType::EOF) {
            match self.parse_statement() {
                Ok(stmt) => {
                    program.push(stmt);
                }
                Err(e) => {
                    bail!("That's it: {:?}", e)
                }
            }
            if let Err(e) = self.bump() {
                bail!("lexer error with {:?}", e)
            }
        }
        Ok(program)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_let_statement_parsing() {
        let code = "let something = 454 + 3 * 4 - 35;";
        let mut parser = Parser::new(code);
        // println!("Parsed statements: {:#?}", parser.parse());
        assert_eq!(
            parser.parse().unwrap(),
            [Statement::Let(
                Ident("something".to_string()),
                Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::UnsignedInteger(454,))),
                        Box::new(Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::UnsignedInteger(3))),
                            Box::new(Expr::Literal(Literal::UnsignedInteger(4))),
                        )),
                    )),
                    Box::new(Expr::Literal(Literal::UnsignedInteger(35,)))
                ),
            )]
        );
    }

    #[test]
    fn test_prefix_expression_parsing() {
        let code = "let val = +454;";
        let mut parser = Parser::new(code);
        // println!("Parsed statement: {:#?}", parser.parse());
        assert_eq!(
            parser.parse().unwrap(),
            [Statement::Let(
                Ident("val".to_string()),
                Expr::Prefix(
                    Prefix::Plus,
                    Box::new(Expr::Literal(Literal::UnsignedInteger(454)))
                )
            )]
        )
    }
}
