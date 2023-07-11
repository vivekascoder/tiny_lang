use crate::ast::*;
use anyhow::{bail, Result};
use std::collections::HashMap;

pub struct Native {
    functions: HashMap<String, fn(Vec<ExprResult>) -> Result<ExprResult>>,
}

/// TODO: Add no. of params too so that we don't have to execute to yield parameter mismatch error.
impl Native {
    // pub fn form_fn_call(fn_call: &NativeFunctionCall) -> Result<Self> {
    //     match fn_call.name.as_str() {
    //         "print" => {
    //             if !(fn_call.parameters.len() == 1) {
    //                 bail!(
    //                     "Expected 1 parameter for native function `print` but got {}",
    //                     fn_call.parameters.len()
    //                 );
    //             }
    //             Ok(Native::Print(fn_call.parameters[0].clone()))
    //         }
    //         _ => {
    //             bail!("`{} is not a valid native function`", fn_call.name.as_str());
    //         }
    //     }
    // }
    pub fn new() -> Self {
        let mut native = Native {
            functions: HashMap::new(),
        };
        native.insert_native("print".to_string(), Self::print);
        native
    }

    pub fn execute(&self, name: &str, params: Vec<ExprResult>) -> Result<ExprResult> {
        println!("Executing with {:?}", &params);
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
