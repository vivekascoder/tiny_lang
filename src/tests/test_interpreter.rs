#[cfg(test)]
mod tests {
    use crate::interpreter::Interpreter;

    #[test]
    fn does_interpreter_starts() {
        let source = "let some_var = 4 + 4 + 5;";
        let mut i = Interpreter::new(source);
        i.eval().unwrap();
    }
}
