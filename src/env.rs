use crate::ast::MemoryObject;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Env {
    store: HashMap<String, MemoryObject>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn insert(&mut self, key: String, val: MemoryObject) {
        self.store.insert(key, val);
    }

    pub fn get(&mut self, key: &str) -> Option<&mut MemoryObject> {
        return match self.store.get_mut(key) {
            Some(v) => Some(v),
            None => match self.outer {
                None => None,
                Some(ref mut outer) => outer.get(key),
            },
        };
    }

    pub fn exists(&mut self, key: &str) -> bool {
        return match self.store.contains_key(key) {
            true => true,
            false => match self.outer {
                None => false,
                Some(ref mut outer) => outer.exists(key),
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
