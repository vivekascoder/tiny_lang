use std::collections::HashMap;

use crate::ast::ExprResult;

pub struct Environment {
    storage: HashMap<String, ExprResult>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            storage: HashMap::new(),
        }
    }
}
