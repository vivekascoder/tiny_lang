use crate::ast::*;
use crate::lexer::Lexer;
use anyhow::{anyhow, bail, Result};
use log::info;
use std::rc::Rc;

pub struct Parser {
    lexer: Lexer,
    current_token: Rc<TokenType>,
    next_token: Rc<TokenType>,
    errors: Vec<anyhow::Error>,
}

impl Parser {
    pub fn new(module: &str, source: &str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(module, source),
            current_token: Rc::new(TokenType::EOF),
            next_token: Rc::new(TokenType::EOF),
            errors: vec![],
        };
        parser.bump().unwrap();
        parser.bump().unwrap();
        parser
    }

    pub fn module(&self) -> String {
        self.lexer.module()
    }

    pub fn get_row(&self) -> usize {
        self.lexer.get_row()
    }

    pub fn get_col(&self) -> usize {
        self.lexer.get_col()
    }

    fn bump(&mut self) -> Result<()> {
        self.current_token = Rc::clone(&self.next_token);
        self.next_token = Rc::new(self.lexer.next()?.type_);
        Ok(())
    }

    fn expect_next_token(&mut self, token: &Rc<TokenType>) -> Result<bool> {
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

    fn next_token_is(&self, token: &Rc<TokenType>) -> bool {
        &self.next_token == token
    }
    fn current_token_is(&self, token: &Rc<TokenType>) -> bool {
        &self.current_token == token
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        // the current token is `let`
        let name = match self.next_token.as_ref() {
            TokenType::Identifier(name) => name.clone(),
            _ => panic!("identifier isn't here."),
        };
        self.bump()?;

        // next token should be assign.
        if !self.expect_next_token(&Rc::new(TokenType::Equal))? {
            panic!("next token is not assign/equal.")
        }

        // next token should be expression.
        self.bump()?;

        // current token should be expression
        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_next_token(&Rc::new(TokenType::SemiColon))? {
            bail!(
                "Line: {}, Col: {}, `;` not found",
                self.lexer.get_row(),
                self.lexer.get_col()
            )
        }
        self.bump()?;

        Ok(Statement::Let(Ident(Rc::clone(&name)), expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Ok(v) => {
                if !self.expect_next_token(&Rc::new(TokenType::SemiColon))? {
                    bail!("`;` not found at the end in expression statment.");
                }
                self.bump()?;
                Ok(Statement::Expr(v))
            }
            Err(e) => {
                bail!("error while parsing expression statement: {:?}", e);
            }
        }
    }

    fn keyword_to_type(&self, tok: &TokenType) -> Result<Type> {
        match tok {
            TokenType::KeywordUsize => Ok(Type::UnsignedInteger),
            TokenType::KeywordBool => Ok(Type::Bool),
            _ => {
                bail!("{:?} is not a valid parameter type.", tok);
            }
        }
    }

    fn parse_function_statement(&mut self) -> Result<Statement> {
        // current token is `fun`.

        let fn_name = match self.next_token.as_ref() {
            TokenType::Identifier(n) => Rc::clone(&n),
            _ => {
                bail!(
                    "next token to parse function should be Identifier, got {:?} instead",
                    &self.next_token
                );
            }
        };
        self.bump()?;

        // TODO: use `expect_next_token()`
        if !self.next_token_is(&Rc::new(TokenType::LParen)) {
            bail!(
                "next token to parse function should be LParen, got {:?} instead",
                &self.next_token
            );
        }
        self.bump()?;

        let mut params: Vec<(Ident, Type)> = vec![];

        // Start parsing parameters
        while !self.current_token_is(&Rc::new(TokenType::RParen)) {
            // parse ident-> : ->type
            let param_name = match self.next_token.as_ref() {
                TokenType::Identifier(ref n) => Rc::clone(n),
                _ => {
                    bail!("next token to parse function parameters should be an Identifier, got {:?} instead", &self.next_token);
                }
            };
            self.bump()?;

            if !self.next_token_is(&Rc::new(TokenType::Colon)) {
                bail!(
                    "next token to parse function parameters should be an Colon, got {:?} instead",
                    &self.next_token
                );
            }
            self.bump()?;

            let type_ = self.keyword_to_type(&self.next_token)?;
            self.bump()?;

            if !(self.next_token_is(&Rc::new(TokenType::Comma))
                || self.next_token_is(&Rc::new(TokenType::RParen)))
            {
                bail!("expected Command, but found {:?}", self.next_token);
            }
            self.bump()?;

            params.push((Ident(param_name), type_));
        }

        // parse the optional return type.
        if !self.expect_next_token(&Rc::new(TokenType::SymbolReturn))? {
            bail!("Expected `=>` but got {:?}", self.next_token);
        }

        let mut return_type: Option<Type> = None;
        info!("Next token is: {:?}", self.next_token);
        if !self.next_token_is(&Rc::new(TokenType::KeywordVoid)) {
            return_type = Some(self.keyword_to_type(&self.next_token)?);
        }
        self.bump()?;

        if !self.expect_next_token(&Rc::new(TokenType::LBrace))? {
            bail!("next token is not `{{`");
        }

        self.bump()?;

        let body = self.parse_block_statement()?;

        Ok(Statement::Function(Function {
            name: fn_name,
            params: params,
            return_type: return_type,
            body: body,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut statements: BlockStatement = vec![];

        info!("Current token: {:?}", &self.current_token);

        while !self.current_token_is(&Rc::new(TokenType::RBrace)) {
            statements.push(self.parse_statement()?);
        }

        self.bump()?;

        Ok(statements)
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.bump()?;
        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_next_token(&Rc::new(TokenType::SemiColon))? {
            bail!("return statement has no semicolon");
        }
        self.bump()?;

        Ok(Statement::Return(expr))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        if !self.expect_next_token(&Rc::new(TokenType::LParen))? {
            bail!(
                "parsing if: expected `(` but found {:?}, instead",
                self.current_token
            );
        }

        // WHY: to make the current token not `(`
        self.bump()?;

        // condition
        let expr = self.parse_expression(Precedence::Lowest)?;
        info!("Expression: {:?}", expr);

        if !self.expect_next_token(&Rc::new(TokenType::RParen))? {
            bail!(
                "parsing if: expected `)` but found {:?}, instead",
                self.current_token
            );
        }

        if !self.expect_next_token(&Rc::new(TokenType::LBrace))? {
            bail!(
                "parsing if: expected `{{` but found {:?}, instead",
                self.current_token
            );
        }
        self.bump()?;

        let block = self.parse_block_statement()?;

        // If next token is `else` then parse else block.
        let mut else_body: Option<BlockStatement> = None;
        if self.current_token_is(&Rc::new(TokenType::KeywordElse)) {
            if !self.expect_next_token(&Rc::new(TokenType::LBrace))? {
                bail!(
                    "expected `{{` while parsing else body but got {:?}",
                    &self.next_token
                );
            }
            self.bump()?;
            else_body = Some(self.parse_block_statement()?);
        }

        Ok(Statement::If(Condition {
            condition: expr,
            if_body: block,
            else_body: else_body,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement> {
        if !self.expect_next_token(&Rc::new(TokenType::LParen))? {
            bail!("expected `(` but got {:>} instead", self.next_token);
        }
        self.bump()?;

        let expr = self.parse_expression(Precedence::Lowest)?;
        info!("Expression: {:?}", expr);

        if !self.expect_next_token(&Rc::new(TokenType::RParen))? {
            bail!(
                "parsing if: expected `)` but found {:?}, instead",
                self.current_token
            );
        }

        if !self.expect_next_token(&Rc::new(TokenType::LBrace))? {
            bail!(
                "parsing if: expected `{{` but found {:?}, instead",
                self.current_token
            );
        }
        self.bump()?;

        let block = self.parse_block_statement()?;

        info!("while statment parsed: {:?}, {:?}", &expr, &block);

        Ok(Statement::While(While {
            condition: expr,
            body: block,
        }))
    }

    fn parse_assign_or_expr(&mut self) -> Result<Statement> {
        let var = match self.current_token.as_ref() {
            TokenType::Identifier(ref i) => Rc::clone(i),
            _ => {
                bail!("{:?} is not identifier", self.current_token);
            }
        };

        if self.next_token_is(&Rc::new(TokenType::Equal)) {
            // parse assignment.
            self.bump()?;
            self.bump()?;
            let expr = self.parse_expression(Precedence::Lowest)?;

            info!("=/i {:?}, {:?}", self.current_token, self.next_token);

            if !self.expect_next_token(&Rc::new(TokenType::SemiColon))? {
                bail!("end plx");
            }
            self.bump()?;
            Ok(Statement::Assignment(Ident(var), expr))
        } else {
            Ok(self.parse_expression_statement()?)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        info!("Current Token: {:?}", self.current_token);
        Ok(match self.current_token.as_ref() {
            TokenType::KeywordLet => self.parse_let_statement()?,
            TokenType::KeywordIf => self.parse_if_statement()?,
            TokenType::KeywordFun => self.parse_function_statement()?,
            TokenType::KeywordWhile => self.parse_while_statement()?,
            TokenType::KeywordReturn => self.parse_return_statement()?,
            TokenType::Identifier(_) => self.parse_assign_or_expr()?,
            _ => self.parse_expression_statement()?,
        })
    }

    fn token_to_precedence(tok: &TokenType) -> Precedence {
        match tok {
            TokenType::Equal | TokenType::NotEqual | TokenType::DoubleEqual => Precedence::Equals,
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
        let mut left = match self.current_token.as_ref() {
            TokenType::Usize(val) => Expr::Literal(Literal::UnsignedInteger(*val)),
            TokenType::Boolean(val) => Expr::Literal(Literal::Bool(*val)),
            TokenType::Identifier(ref i) => Expr::Ident(Ident(i.clone())),
            TokenType::SQuote => {
                use TokenType::*;
                // Parse character.
                let char_val = match self.next_token.as_ref() {
                    Identifier(ref i) => {
                        if i.len() != 1 {
                            bail!("Character should be of length 1");
                        }
                        Rc::clone(i)
                    }

                    // TODO: could use display here.
                    NewLine => Rc::from('\n'.to_string()),
                    Tab => Rc::from('\t'.to_string()),
                    Space => Rc::from(' '.to_string()),
                    BSlash => Rc::from('\\'.to_string()),
                    Minus => Rc::from('-'.to_string()),

                    _ => {
                        bail!("char not found after `'` instead got {:?}", self.next_token);
                    }
                };
                self.bump()?;
                if !self.expect_next_token(&Rc::new(TokenType::SQuote))? {
                    bail!(
                        "character not ended with `'` found {:?} instead.",
                        self.next_token
                    );
                }
                Expr::Literal(Literal::Char(char_val.chars().nth(0).unwrap()))
            }
            TokenType::Minus | TokenType::Bang | TokenType::Plus => {
                // Parse prefix expression.

                // Operator
                let prefix = match self.current_token.as_ref() {
                    TokenType::Bang => Prefix::Not,
                    TokenType::Plus => Prefix::Plus,
                    TokenType::Minus => Prefix::Minus,
                    _ => bail!("not a valid infix operator."),
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
        info!("Left: {:?}", left);

        // Parse the infix
        while !self.next_token_is(&Rc::new(TokenType::SemiColon))
            && precedence < self.next_token_precedence()
        {
            info!(
                "next token while parsing expression right -> {:?}",
                self.next_token
            );
            match self.next_token.as_ref() {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Divide
                | TokenType::Multiply
                | TokenType::Equal
                | TokenType::DoubleEqual
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual => {
                    self.bump()?;

                    let infix = match self.current_token.as_ref() {
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
                            bail!("{:?} is not a valid infix operator.", &self.next_token);
                        }
                    };
                    info!("Infix: {:?}", infix);

                    let precedence = self.current_token_precedence();
                    self.bump()?;

                    left = match self.parse_expression(precedence) {
                        Ok(expr) => Expr::Infix(infix, Box::new(left), Box::new(expr)),
                        Err(e) => {
                            bail!("error while parsing expr: {:?}", e);
                        }
                    }
                }
                TokenType::LParen => {
                    let mut params: Vec<Expr> = vec![];
                    self.bump()?;

                    while !self.current_token_is(&Rc::new(TokenType::RParen)) {
                        self.bump()?;
                        let param = self.parse_expression(Precedence::Lowest)?;
                        info!("param: {:?}", param);
                        params.push(param);

                        info!("{:?}, {:?}", self.current_token, self.next_token);
                        if !(self.next_token_is(&Rc::new(TokenType::Comma))
                            || self.next_token_is(&Rc::new(TokenType::RParen)))
                        {
                            bail!("sep , while param func");
                        }
                        self.bump()?;
                        info!(" -->{:?}, {:?}", self.current_token, self.next_token);
                    }

                    // self.bump()?;
                    info!(" -->{:?}, {:?}", self.current_token, self.next_token);
                    // self.bump()?;

                    if let Expr::Ident(i) = left {
                        left = Expr::Call(FunctionCall {
                            parameters: params,
                            name: i.0,
                        })
                    } else {
                        bail!("left is not an identifier.");
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
        while !self.current_token_is(&Rc::new(TokenType::EOF)) {
            match self.parse_statement() {
                Ok(stmt) => {
                    info!("Statement: {:?}", &stmt);
                    program.push(stmt);
                }
                Err(e) => {
                    bail!("That's it: {:?}", e)
                }
            }
        }
        Ok(program)
    }
}
