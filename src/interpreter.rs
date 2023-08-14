use crate::parser::parser::Parser;
use crate::scope::ScopeStack;
use crate::{ast::*, native::Native};
use anyhow::{bail, Result};
use log::info;
use std::rc::Rc;

pub struct Interpreter {
    parser: Parser,
    env: ScopeStack,
    native: Native,
}

impl Interpreter {
    pub fn new(module: &str, source: &str) -> Self {
        let mut scope_stack = ScopeStack::new();
        scope_stack.push_scope();

        Self {
            parser: Parser::new(module, source),
            env: scope_stack,
            native: Native::new(),
        }
    }

    pub fn get_stack(&self) -> ScopeStack {
        self.env.clone()
    }

    pub fn from_scope(module: &str, source: &str, stack: ScopeStack) -> Self {
        Self {
            parser: Parser::new(module, source),
            env: stack,
            native: Native::new(),
        }
    }

    // pub fn new_repl(source: &str, scope_stack: )

    pub fn module(&self) -> String {
        self.parser.module()
    }

    pub fn get_row(&self) -> usize {
        self.parser.get_row()
    }

    pub fn get_col(&self) -> usize {
        self.parser.get_col()
    }

    fn eval_infix_expr(&mut self, infix: Infix, left: Expr, right: Expr) -> Result<ExprResult> {
        let left_result = self.eval_expr(left)?;
        let right_result = self.eval_expr(right)?;

        match left_result {
            ExprResult::Void => {
                bail!("Left expression can not be void");
            }
            ExprResult::UnsignedInteger(l) => {
                if let ExprResult::UnsignedInteger(r) = right_result {
                    return match infix {
                        Infix::Plus => Ok(ExprResult::UnsignedInteger(l + r)),
                        Infix::Minus => Ok(ExprResult::UnsignedInteger(l - r)),
                        Infix::Multiply => Ok(ExprResult::UnsignedInteger(l * r)),
                        Infix::Divide => Ok(ExprResult::UnsignedInteger(l / r)),

                        // Comparison
                        Infix::GreaterThan => Ok(ExprResult::Bool(l > r)),
                        Infix::GreaterThanEqual => Ok(ExprResult::Bool(l >= r)),
                        Infix::LessThan => Ok(ExprResult::Bool(l < r)),
                        Infix::LessThanEqual => Ok(ExprResult::Bool(l <= r)),
                        Infix::DoubleEqual => Ok(ExprResult::Bool(l == r)),
                        Infix::NotEqual => Ok(ExprResult::Bool(l != r)),

                        Infix::LeftShift => Ok(ExprResult::UnsignedInteger(l << r)),
                        Infix::RightShift => Ok(ExprResult::UnsignedInteger(l >> r)),
                        Infix::BitwiseAnd => Ok(ExprResult::UnsignedInteger(l & r)),
                        Infix::BitwiseXor => Ok(ExprResult::UnsignedInteger(l ^ r)),
                        Infix::BitwiseOr => Ok(ExprResult::UnsignedInteger(l | r)),
                    };
                } else {
                    bail!("Types don't match");
                }
            }

            ExprResult::SignedInteger(l) => {
                if let ExprResult::SignedInteger(r) = right_result {
                    return match infix {
                        Infix::Plus => Ok(ExprResult::SignedInteger(l + r)),
                        Infix::Minus => Ok(ExprResult::SignedInteger(l - r)),
                        Infix::Multiply => Ok(ExprResult::SignedInteger(l * r)),
                        Infix::Divide => Ok(ExprResult::SignedInteger(l / r)),

                        // Comparison
                        Infix::GreaterThan => Ok(ExprResult::Bool(l > r)),
                        Infix::GreaterThanEqual => Ok(ExprResult::Bool(l >= r)),
                        Infix::LessThan => Ok(ExprResult::Bool(l < r)),
                        Infix::LessThanEqual => Ok(ExprResult::Bool(l <= r)),
                        Infix::DoubleEqual => Ok(ExprResult::Bool(l == r)),
                        Infix::NotEqual => Ok(ExprResult::Bool(l != r)),

                        Infix::LeftShift => Ok(ExprResult::SignedInteger(l << r)),
                        Infix::RightShift => Ok(ExprResult::SignedInteger(l >> r)),
                        Infix::BitwiseAnd => Ok(ExprResult::SignedInteger(l & r)),
                        Infix::BitwiseXor => Ok(ExprResult::SignedInteger(l ^ r)),
                        Infix::BitwiseOr => Ok(ExprResult::SignedInteger(l | r)),
                    };
                } else {
                    bail!("Types don't match");
                }
            }

            ExprResult::Bool(l) => {
                if let ExprResult::Bool(r) = right_result {
                    return match infix {
                        Infix::GreaterThan => Ok(ExprResult::Bool(l > r)),
                        Infix::GreaterThanEqual => Ok(ExprResult::Bool(l >= r)),
                        Infix::LessThan => Ok(ExprResult::Bool(l < r)),
                        Infix::LessThanEqual => Ok(ExprResult::Bool(l <= r)),
                        Infix::DoubleEqual => Ok(ExprResult::Bool(l == r)),
                        Infix::NotEqual => Ok(ExprResult::Bool(l != r)),
                        _ => {
                            bail!("The infix operator: {:?} doesn't apply to bool.", infix);
                        }
                    };
                } else {
                    bail!("Types don't match.")
                }
            }
            ExprResult::Char(l) => {
                if let ExprResult::Char(r) = right_result {
                    return match infix {
                        Infix::GreaterThan => Ok(ExprResult::Bool(l > r)),
                        Infix::GreaterThanEqual => Ok(ExprResult::Bool(l >= r)),
                        Infix::LessThan => Ok(ExprResult::Bool(l < r)),
                        Infix::LessThanEqual => Ok(ExprResult::Bool(l <= r)),
                        Infix::DoubleEqual => Ok(ExprResult::Bool(l == r)),
                        Infix::NotEqual => Ok(ExprResult::Bool(l != r)),
                        _ => {
                            bail!("The infix operator: {:?} doesn't apply to char.", infix);
                        }
                    };
                } else {
                    bail!("Types don't match.")
                }
            }
            ExprResult::Return(v) => {
                return Ok(self.eval_infix_expr(
                    infix,
                    self.expr_result_to_expr(*v)?,
                    self.expr_result_to_expr(right_result)?,
                )?);
            }
        }
    }

    fn expr_result_to_expr(&self, expr_result: ExprResult) -> Result<Expr> {
        match expr_result {
            ExprResult::Bool(b) => Ok(Expr::Literal(Literal::Bool(b))),
            ExprResult::UnsignedInteger(i) => Ok(Expr::Literal(Literal::UnsignedInteger(i))),
            ExprResult::Char(c) => Ok(Expr::Literal(Literal::Char(c))),
            ExprResult::Return(v) => Ok(self.expr_result_to_expr(*v)?),
            _ => {
                bail!("Can't convert {:?} to Expr", &expr_result);
            }
        }
    }

    /// TODO: finish this?
    fn eval_prefix_expr(&self, operator: Prefix, expr: Expr) -> Result<ExprResult> {
        Ok(ExprResult::Void)
    }

    fn eval_ident(&mut self, identifier: Ident) -> Result<ExprResult> {
        // TODO: Uff, plz fix diz clone shit.
        match self.env.get_ref(&identifier.0) {
            None => {
                bail!("The identifier {:?} doesn't exists.", &identifier.0);
            }
            Some(v) => {
                if let MemoryObject::ExprResult(res) = v {
                    Ok(res.clone())
                } else {
                    bail!("identifier can't be Function");
                }
            }
        }
    }

    fn eval_literal(&self, literal: Literal) -> Result<ExprResult> {
        match literal {
            Literal::Bool(b) => Ok(ExprResult::Bool(b)),
            Literal::UnsignedInteger(i) => Ok(ExprResult::UnsignedInteger(i)),
            Literal::Char(c) => Ok(ExprResult::Char(c)),
        }
    }

    fn eval_fn_call(&mut self, fn_call: FunctionCall) -> Result<ExprResult> {
        // make sure the function exists in memory.
        if !(self.env.exists(&fn_call.name) || self.native.is_native(&fn_call.name)) {
            bail!("Function {:?} doesn't exists.", fn_call.name);
        }

        let mut expr_results: Vec<ExprResult> = vec![];
        for expr in fn_call.parameters {
            expr_results.push(self.eval_expr(expr)?);
        }

        if self.native.is_native(&fn_call.name) {
            // execute native function.
            return Ok(self.native.execute(&fn_call.name, expr_results)?);
        }

        let fun = match self.env.get_ref(&fn_call.name) {
            None => {
                bail!("Can't get the functon")
            }
            Some(v) => match v {
                MemoryObject::ExprResult(_) => {
                    bail!("it should be a function");
                }
                MemoryObject::Function(fun) => fun.clone(),
            },
        };

        if !(fun.params.len() == expr_results.len()) {
            bail!(
                "expected {:?} parameters but got {:?} instead",
                fun.params.len(),
                expr_results.len()
            );
        }

        // self.env = Rc::new(RefCell::new(scoped_env));
        self.env.push_scope();

        let zipped = std::iter::zip(&fun.params, expr_results);
        let block_scope = self.env.get_mut_scope();
        for val in zipped {
            // Assert the type of parameter.
            if !Self::is_type_expr_result_same(Some(&val.0 .1), &val.1) {
                bail!(
                    "Expr result {:?}, isn't valid for type {:?}",
                    &val.1,
                    &val.0 .1
                );
            }

            block_scope.insert(Rc::clone(&val.0 .0 .0), MemoryObject::ExprResult(val.1));
        }

        let returned_value = self.eval_block_statement((&fun.body).clone())?;

        let popped_scope = self.env.pop_scope();
        info!("Popped scope: {:?}", popped_scope);

        info!("returned_val: {:?} {:?}", &returned_value, &fun.return_type);

        let r = match &fun.return_type {
            Some(v) => Some(v),
            None => None,
        };

        if !Self::is_type_expr_result_same(r, &returned_value) {
            info!("-----");
            bail!(
                "Return value: {:?}, is not of type {:?}",
                &returned_value,
                &fun.return_type
            );
        }

        Ok(returned_value)
    }

    fn eval_block_statement(&mut self, statements: BlockStatement) -> Result<ExprResult> {
        info!("Block: {:?} {}", &statements, statements.len());
        for s in statements {
            let r = self.eval_statement(s)?;
            info!("statement resulted in {:?}", &r);
            if let ExprResult::Return(r) = r {
                info!("Block resulted {:?}", &r);
                return Ok(ExprResult::Return(r));
            }
        }
        Ok(ExprResult::Void)
    }

    pub fn is_type_expr_result_same(type_: Option<&Type>, expr_result: &ExprResult) -> bool {
        if let ExprResult::Return(r) = expr_result {
            return Self::is_type_expr_result_same(type_, r);
        }
        match type_ {
            None => {
                if let ExprResult::Void = expr_result {
                    return true;
                } else {
                    false
                }
            }
            Some(t) => match t {
                Type::Bool => {
                    if let ExprResult::Bool(_) = expr_result {
                        return true;
                    }
                    false
                }
                Type::UnsignedInteger => {
                    if let ExprResult::UnsignedInteger(_) = expr_result {
                        return true;
                    }
                    false
                }
                Type::SignedInteger => {
                    if let ExprResult::SignedInteger(_) = expr_result {
                        return true;
                    }
                    false
                }
                Type::Char => {
                    if let ExprResult::Char(_) = expr_result {
                        return true;
                    }
                    false
                }
            },
        }
    }

    fn eval_if_statement(&mut self, if_: Condition) -> Result<ExprResult> {
        let expr_result = self.eval_expr(if_.condition)?;

        info!("If condition result is {:?}", expr_result);

        if let ExprResult::Bool(is_true) = expr_result {
            if is_true {
                let v = self.eval_block_statement(if_.if_body)?;
                info!("If body resulted in {:?}", &v);
                return Ok(v);
            }
        } else {
            bail!("The condition resulted in {:?} and not bool.", &expr_result);
        }

        if let None = if_.else_body {
            info!("else body isn't there.");
            return Ok(ExprResult::Void);
        }

        let else_body = if_.else_body.unwrap();

        self.env.push_scope();
        let v = self.eval_block_statement(else_body)?;
        self.env.pop_scope();

        info!("else body resulted in {:?}", &v);
        return Ok(v);
    }

    fn eval_expr(&mut self, expr: Expr) -> Result<ExprResult> {
        match expr {
            Expr::Infix(infix, left, right) => Ok(self.eval_infix_expr(infix, *left, *right)?),
            Expr::Prefix(op, expr) => Ok(self.eval_prefix_expr(op, *expr)?),
            Expr::Literal(l) => Ok(self.eval_literal(l)?),
            Expr::Ident(variable) => Ok(self.eval_ident(variable)?),
            Expr::Call(fn_call) => Ok(self.eval_fn_call(fn_call)?),
            _ => {
                todo!()
            }
        }
    }

    fn eval_statement(&mut self, s: Statement) -> Result<ExprResult> {
        info!("Current statement: {:?}", &s);
        match s {
            Statement::Let(ident, type_, expr) => {
                let expr_result = self.eval_expr(expr)?;

                if let Some(v) = type_ {
                    if !Self::is_type_expr_result_same(Some(&v), &expr_result) {
                        bail!(
                            "let statement results in {:?} which is not of type: {:?}",
                            &expr_result,
                            v
                        )
                    }
                }

                let scope = self.env.get_mut_scope();
                scope.insert(ident.0, MemoryObject::ExprResult(expr_result));

                Ok(ExprResult::Void)
            }
            Statement::If(if_) => Ok(self.eval_if_statement(if_)?),
            Statement::Function(fun) => {
                let scope = self.env.get_mut_scope();
                scope.insert(Rc::clone(&fun.name), MemoryObject::Function(fun));
                Ok(ExprResult::Void)
            }
            Statement::Return(expr) => {
                let expr_result = self.eval_expr(expr)?;
                Ok(ExprResult::Return(Box::new(expr_result)))
            }
            Statement::Expr(expr) => {
                let _ = self.eval_expr(expr)?;
                Ok(ExprResult::Void)
            }
            Statement::Mutate(ident, expr) => {
                let expr_result = self.eval_expr(expr)?;

                let val = self.env.get_mut_ref(&ident.0);
                if let None = val {
                    bail!("identifier {:?} isn't in the environment.", &ident.0);
                }

                *val.unwrap() = MemoryObject::ExprResult(expr_result);
                Ok(ExprResult::Void)
            }
            Statement::While(while_) => loop {
                if let ExprResult::Bool(is_true) = self.eval_expr(while_.condition.clone())? {
                    if !is_true {
                        return Ok(ExprResult::Void);
                    }

                    self.env.push_scope();

                    let v = self.eval_block_statement(while_.body.clone())?;
                    if let ExprResult::Return(boxed) = v {
                        return Ok(ExprResult::Return(boxed));
                    }

                    let popped_scope = self.env.pop_scope();
                    info!("Popped scope: {:?}", popped_scope);
                } else {
                    bail!(
                        "while condition must be bool but it's {:?}.",
                        &while_.condition
                    );
                }
            },
        }
    }

    /// Evaluate the AST
    pub fn eval(&mut self) -> Result<()> {
        let program: Program = self.parser.parse()?;

        for statement in program {
            let r = self.eval_statement(statement)?;
        }

        info!("Env: {:#?}", self.env);

        Ok(())
    }
}
