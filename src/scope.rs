use crate::ast::MemoryObject;
use std::{collections::HashMap, rc::Rc};

pub type Scope = HashMap<Rc<str>, MemoryObject>;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    stack: Vec<Scope>,
}

// TODO: Make it generic so that we can store arbitrary value.
// type Scope<V> = HasMpa<Rc<str>, V>
impl ScopeStack {
    pub fn new() -> Self {
        Self { stack: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.stack.pop()
    }

    pub fn get_mut_scope(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }

    pub fn get_scope(&self) -> &Scope {
        self.stack.last().unwrap()
    }

    pub fn exists(&self, key: &str) -> bool {
        self.stack
            .iter()
            .rev()
            .find_map(|s| Some(s.get(key)))
            .is_some()
    }

    pub fn get_ref(&self, key: &str) -> Option<&MemoryObject> {
        self.stack.iter().rev().find_map(|s| s.get(key))
    }

    pub fn get_mut_ref(&mut self, key: &str) -> Option<&mut MemoryObject> {
        self.stack.iter_mut().rev().find_map(|s| s.get_mut(key))
    }
}
