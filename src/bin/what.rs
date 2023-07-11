use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
struct SomeStruct {
    name: String,
    age: Vec<usize>,
}

fn main() -> anyhow::Result<()> {
    let s = Rc::new(RefCell::new(SomeStruct {
        name: "something".to_string(),
        age: vec![35, 363],
    }));
    let s1 = Rc::clone(&s);
    s1.borrow_mut().name = "changed".to_string();

    assert!(s.borrow().name == "changed");

    // s.age = vec![];
    Ok(())
}
