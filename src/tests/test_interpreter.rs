#[cfg(test)]
mod tests {
    use crate::interpreter::Interpreter;

    #[test]
    fn does_interpreter_starts() {
        let source = r#"
        let a = 454 + 3636 * 3; 
        let b = 45 / 3;
        "#;
        let mut i = Interpreter::new(source);
        i.eval().unwrap();
    }
}
