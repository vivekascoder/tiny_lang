use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub struct Env {
    store: HashMap<String, String>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new(store: HashMap<String, String>) -> Self {
        Self { store, outer: None }
    }

    pub fn insert(&mut self, key: String, val: String) {
        self.store.insert(key, val);
    }

    pub fn get(&mut self, key: String) -> Option<&mut String> {
        return match self.store.get_mut(&key) {
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

fn main() -> anyhow::Result<()> {
    let mut env = Env::new(HashMap::new());
    env.insert("outer_a".to_string(), "awesome".to_string());
    env.insert("outer_b".to_string(), "ball".to_string());

    let mut scoped_env = Env::new_with_outer(Box::new(env));

    scoped_env.insert("scoped_a".to_string(), "scoped_awesome".to_string());
    scoped_env.insert("scoped_b".to_string(), "scoped_ball".to_string());

    let mut sub_scoped_env = Env::new_with_outer(Box::new(scoped_env));

    sub_scoped_env.insert("i".to_string(), "i".to_string());
    sub_scoped_env.insert("j".to_string(), "j".to_string());

    println!("Env: {:#?}", &sub_scoped_env);
    println!(
        "Scoped VAr: {:?}",
        sub_scoped_env.get("outer_b".to_string())
    );

    Ok(())
}
