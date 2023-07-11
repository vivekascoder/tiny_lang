use crate::{ast::*, native::Native};
use anyhow::{bail, Result};

use crate::{env::Env, parser::Parser};

pub struct Interpreter {
    parser: Parser,
    env: Env,
    native: Native,
}

impl Interpreter {
    pub fn new(source: &str) -> Self {
        Self {
            parser: Parser::new(source),
            env: Env::new(),
            native: Native::new(),
        }
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
                        _ => {
                            bail!("sf");
                        }
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
        }
    }

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
                MemoryObject::Function(fun) => fun,
            },
        };

        if !(fun.params.len() == expr_results.len()) {
            bail!(
                "expected {:?} parameters but got {:?} instead",
                fun.params.len(),
                expr_results.len()
            );
        }
        let fun_params_clone = fun.params.clone();
        let fun_body = fun.body.clone();
        let fun_return = fun.return_type.clone();

        let scoped_env = Env::new_with_outer(Box::new(self.env.clone()));
        self.env = scoped_env;

        let zipped = std::iter::zip(fun_params_clone, expr_results.clone());
        for val in zipped {
            // Assert the type of parameter.
            if !Self::is_type_expr_result_same(Some(val.0 .1.clone()), &val.1) {
                bail!(
                    "Expr result {:?}, isn't valid for type {:?}",
                    &val.1,
                    &val.0 .1
                );
            }

            self.env
                .insert(val.0 .0 .0, MemoryObject::ExprResult(val.1));
        }

        let returned_value = self.eval_block_statement(fun_body)?;
        println!("returned_val: {:?} {:?}", &returned_value, &fun_return);

        if !Self::is_type_expr_result_same(fun_return.clone(), &returned_value) {
            println!("-----");
            bail!(
                "Return value: {:?}, is not of type {:?}",
                &returned_value,
                &fun_return
            );
        }

        self.env = self.env.cloned_outer();

        Ok(returned_value)
    }

    fn eval_block_statement(&mut self, statements: BlockStatement) -> Result<ExprResult> {
        println!("Block: {:?} {}", &statements, statements.len());
        let mut should_return = false;
        for s in statements {
            if let Statement::Return(_) = &s {
                should_return = true;
            }
            let r = self.eval_statement(s)?;
            if should_return {
                return Ok(r);
            }
        }
        Ok(ExprResult::Void)
    }

    pub fn is_type_expr_result_same(type_: Option<Type>, expr_result: &ExprResult) -> bool {
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
                    } else {
                        return false;
                    }
                }
                Type::UnsignedInteger => {
                    if let ExprResult::UnsignedInteger(_) = expr_result {
                        return true;
                    } else {
                        false
                    }
                }
            },
        }
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
        println!("Current statement: {:?}", &s);
        match s {
            Statement::Let(ident, expr) => {
                let expr_result = self.eval_expr(expr)?;
                self.env
                    .insert(ident.0, MemoryObject::ExprResult(expr_result));
                Ok(ExprResult::Void)
            }
            Statement::Function(fun) => {
                self.env
                    .insert(fun.name.clone(), MemoryObject::Function(fun));
                Ok(ExprResult::Void)
            }
            Statement::Return(expr) => {
                let expr_result = self.eval_expr(expr)?;
                Ok(expr_result)
            }
            Statement::Expr(expr) => {
                let _ = self.eval_expr(expr)?;
                Ok(ExprResult::Void)
            }
            _ => {
                bail!("Statement {:?}, can't be evaluated.", &s);
            }
        }
    }

    /// Evaluate the AST
    pub fn eval(&mut self) -> Result<()> {
        let program: Program = self.parser.parse()?;

        for statement in program {
            let r = self.eval_statement(statement)?;
        }

        println!("Env: {:#?}", self.env);

        Ok(())
    }
}
