use crate::ast::*;
use anyhow::{bail, Result};
use log::info;
use std::collections::HashMap;

pub struct Native {
    functions: HashMap<String, fn(Vec<ExprResult>) -> Result<ExprResult>>,
}

/// TODO: Add no. of params too so that we don't have to execute to yield parameter mismatch error.
impl Native {
    pub fn new() -> Self {
        let mut native = Native {
            functions: HashMap::new(),
        };
        native.insert_native("print".to_string(), Self::print);
        native
    }

    pub fn execute(&self, name: &str, params: Vec<ExprResult>) -> Result<ExprResult> {
        info!("Executing with {:?}", &params);
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

    pub fn insert_native(&mut self, name: String, fun: fn(Vec<ExprResult>) -> Result<ExprResult>) {
        self.functions.insert(name, fun);
    }
}
