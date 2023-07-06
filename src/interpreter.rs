use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    ast::{Expr, ExprResult, Ident, Infix, Literal, Prefix, Statement},
    environment::Environment,
    parser::Parser,
};

pub struct Interpreter {
    parser: Parser,
    env: Environment,
}

impl Interpreter {
    fn new(source: &str) -> Self {
        Self {
            parser: Parser::new(source),
            env: Environment::new(),
        }
    }

    fn eval_infix_expr(&self, infix: Infix, left: Expr, right: Expr) -> Result<ExprResult> {
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

    fn eval_ident(&self, identifier: Ident) -> Result<ExprResult> {
        Ok(ExprResult::Void)
    }

    fn eval_literal(&self, literal: Literal) -> Result<ExprResult> {
        match literal {
            Literal::Bool(b) => Ok(ExprResult::Bool(b)),
            Literal::UnsignedInteger(i) => Ok(ExprResult::UnsignedInteger(i)),
        }
    }

    fn eval_expr(&self, expr: Expr) -> Result<ExprResult> {
        match expr {
            Expr::Infix(infix, left, right) => Ok(self.eval_infix_expr(infix, *left, *right)?),
            Expr::Prefix(op, expr) => Ok(self.eval_prefix_expr(op, *expr)?),
            Expr::Literal(l) => Ok(self.eval_literal(l)?),
            Expr::Ident(variable) => Ok(self.eval_ident(variable)?),
        }
    }

    /// Evaluate the AST
    fn eval(&mut self) -> Result<()> {
        let program = self.parser.parse()?;

        for statement in program {
            match statement {
                Statement::Let(ident, expr) => {
                    let expr_result = self.eval_expr(expr)?;
                    println!("Expression result: {:?}", expr_result);
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn does_interpreter_starts() {
        let source = "let some_var = 4 + 4 + 5;";
        let mut i = Interpreter::new(source);
        i.eval().unwrap();
    }
}
