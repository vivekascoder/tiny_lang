#[cfg(test)]
pub mod tests {
    use crate::ast::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_lexing_let_syntax() {
        let source = "let some_var: usize = 345 + 35353;";
        insta::assert_debug_snapshot!(Lexer::new(source).into_iter().collect::<Vec<Token>>());
    }

    #[test]
    fn test_function_declaration() {
        let source = r#"
        fun calculate_something(a: usize, b: usize) => bool {
            if (a > b) {
                return true;
            } else {
                return false;
            }
        }
        "#;
        insta::assert_debug_snapshot!(Lexer::new(source).into_iter().collect::<Vec<Token>>());
    }
}
