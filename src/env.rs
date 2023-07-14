use crate::ast::MemoryObject;
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Env {
    store: HashMap<Rc<str>, MemoryObject>,
    outer: Option<Rc<RefCell<Env>>>,
}

// enum R<'a> {
//     A { val: &'a mut MemoryObject },
//     B { val: RefMut<'a, Env> },
// }

impl Env {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn insert(&mut self, key: Rc<str>, val: MemoryObject) {
        self.store.insert(key, val);
    }

    pub fn set(&mut self, key: &str, value: MemoryObject) {
        match self.store.get_mut(key) {
            Some(v) => {
                *v = value;
            }
            None => match self.outer {
                None => {}
                Some(ref v) => {
                    v.borrow_mut().set(key, value);
                }
            },
        };
    }

    pub fn get_ref(&self, key: &str) -> Option<MemoryObject> {
        return match self.store.get(key) {
            Some(v) => Some(v.clone()),
            None => match self.outer {
                None => None,
                Some(ref outer) => {
                    let a = outer.borrow();
                    a.get_ref(key)
                }
            },
        };
    }

    // pub fn get_ref_mut(&mut self, key: &str) -> Option<&mut MemoryObject> {
    //     return match self.store.get_mut(key) {
    //         Some(v) => Some(v),
    //         None => match self.outer {
    //             None => None,
    //             Some(ref outer) => outer.borrow_mut().get_ref_mut(key),
    //         },
    //     };
    // }

    pub fn exists(&self, key: &str) -> bool {
        return match self.store.contains_key(key) {
            true => true,
            false => match self.outer {
                None => false,
                Some(ref outer) => outer.borrow().exists(key),
            },
        };
    }

    pub fn new_with_outer(outer: Rc<RefCell<Env>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
}
