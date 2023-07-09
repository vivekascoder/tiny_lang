use crate::ast::ExprResult;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Env {
    store: HashMap<String, ExprResult>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn insert(&mut self, key: String, val: ExprResult) {
        self.store.insert(key, val);
    }

    pub fn get(&mut self, key: &str) -> Option<&mut ExprResult> {
        return match self.store.get_mut(key) {
            Some(v) => Some(v),
            None => match self.outer {
                None => None,
                Some(ref mut outer) => outer.get(key),
            },
        };
    }

    pub fn new_with_outer(outer: Box<Env>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
}
