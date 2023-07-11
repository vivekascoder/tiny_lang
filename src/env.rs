use crate::ast::MemoryObject;
use std::collections::HashMap;

#[derive(Debug, Clone)]
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

    pub fn cloned_outer(&mut self) -> Env {
        if let Some(ref outer) = self.outer {
            *outer.clone()
        } else {
            Self::new()
        }
    }

    pub fn insert(&mut self, key: String, val: MemoryObject) {
        self.store.insert(key, val);
    }

    pub fn get_ref_mut(&mut self, key: &str) -> Option<&mut MemoryObject> {
        return match self.store.get_mut(key) {
            Some(v) => Some(v),
            None => match self.outer {
                None => None,
                Some(ref mut outer) => outer.get_ref_mut(key),
            },
        };
    }

    pub fn get_ref(&self, key: &str) -> Option<&MemoryObject> {
        return match self.store.get(key) {
            Some(v) => Some(v),
            None => match self.outer {
                None => None,
                Some(ref outer) => outer.get_ref(key),
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
