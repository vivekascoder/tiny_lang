use crate::ast::*;
use anyhow::{bail, Result};
use log::info;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Native {
    functions: HashMap<String, fn(Vec<ExprResult>) -> Result<ExprResult>>,
    output: Rc<RefCell<String>>,
}

/// TODO: Add no. of params too so that we don't have to execute to yield parameter mismatch error.
impl Native {
    pub fn new() -> Self {
        let mut native = Native {
            functions: HashMap::new(),
            output: Rc::new(RefCell::new(String::new())),
        };
        native.insert_native("print".to_string(), Self::print);
        native.insert_native("printf".to_string(), Self::printf);
        native
    }

    pub fn output(&self) -> String {
        self.output.borrow().clone()
    }

    pub fn execute(&self, name: &str, params: Vec<ExprResult>) -> Result<ExprResult> {
        info!("Executing with {:?}", &params);
        if name == "print" {
            return self.execute_print(params);
        }
        if name == "printf" {
            return self.execute_printf(params);
        }
        let fun = self.functions.get(name).unwrap();
        Ok(fun(params)?)
    }

    pub fn is_native(&self, s: &str) -> bool {
        self.functions.contains_key(s)
    }

    pub fn print(params: Vec<ExprResult>) -> Result<ExprResult> {
        if !(params.len() == 1) {
            bail!("print() native function takes only one parameter.");
        }
        print!("{}", params[0]);
        Ok(ExprResult::Void)
    }

    pub fn printf(params: Vec<ExprResult>) -> Result<ExprResult> {
        if params.is_empty() {
            bail!("printf() native function expects a format string.");
        }
        Ok(ExprResult::Void)
    }

    pub fn insert_native(&mut self, name: String, fun: fn(Vec<ExprResult>) -> Result<ExprResult>) {
        self.functions.insert(name, fun);
    }

    fn execute_print(&self, params: Vec<ExprResult>) -> Result<ExprResult> {
        if !(params.len() == 1) {
            bail!("print() native function takes only one parameter.");
        }
        let rendered = params[0].to_string();
        print!("{}", rendered);
        self.output.borrow_mut().push_str(&rendered);
        Ok(ExprResult::Void)
    }

    fn execute_printf(&self, params: Vec<ExprResult>) -> Result<ExprResult> {
        if params.is_empty() {
            bail!("printf() native function expects a format string.");
        }
        let format = match &params[0] {
            ExprResult::String(s) => s.as_str(),
            other => bail!(
                "printf() first parameter must be a string, got {:?}.",
                other
            ),
        };
        let rendered = format_printf(format, &params[1..])?;
        print!("{}", rendered);
        self.output.borrow_mut().push_str(&rendered);
        Ok(ExprResult::UnsignedInteger(rendered.len()))
    }
}

fn format_printf(format: &str, args: &[ExprResult]) -> Result<String> {
    let mut rendered = String::new();
    let mut chars = format.chars().peekable();
    let mut arg_index = 0usize;

    while let Some(ch) = chars.next() {
        if ch != '%' {
            rendered.push(ch);
            continue;
        }

        match chars.next() {
            Some('%') => rendered.push('%'),
            Some('d') | Some('u') | Some('i') => {
                let arg = args
                    .get(arg_index)
                    .ok_or_else(|| anyhow::anyhow!("printf() missing integer argument."))?;
                match arg {
                    ExprResult::UnsignedInteger(v) => rendered.push_str(&v.to_string()),
                    ExprResult::SignedInteger(v) => rendered.push_str(&v.to_string()),
                    ExprResult::Return(v) => match v.as_ref() {
                        ExprResult::UnsignedInteger(v) => rendered.push_str(&v.to_string()),
                        ExprResult::SignedInteger(v) => rendered.push_str(&v.to_string()),
                        other => bail!("printf() expected integer argument, got {:?}.", other),
                    },
                    other => bail!("printf() expected integer argument, got {:?}.", other),
                }
                arg_index += 1;
            }
            Some('c') => {
                let arg = args
                    .get(arg_index)
                    .ok_or_else(|| anyhow::anyhow!("printf() missing char argument."))?;
                match arg {
                    ExprResult::Char(v) => rendered.push(*v),
                    other => bail!("printf() expected char argument, got {:?}.", other),
                }
                arg_index += 1;
            }
            Some('s') => {
                let arg = args
                    .get(arg_index)
                    .ok_or_else(|| anyhow::anyhow!("printf() missing string argument."))?;
                match arg {
                    ExprResult::String(v) => rendered.push_str(v),
                    other => bail!("printf() expected string argument, got {:?}.", other),
                }
                arg_index += 1;
            }
            Some(other) => {
                rendered.push('%');
                rendered.push(other);
            }
            None => rendered.push('%'),
        }
    }

    Ok(rendered)
}
