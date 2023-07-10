#[cfg(test)]
pub mod tests {
    use crate::ast::*;
    use crate::parser::Parser;

    #[test]
    fn test_let_statement_parsing() {
        let code = "let something = 454 + 3 * 4 - 35; ";
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_multiple_let_statement_parsing() {
        let code = r#"
        let something = 454 + 3 * 4 - 35;
        let a = 35 / 3;
        "#;
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_prefix_expression_parsing() {
        let code = "let val = +454;";
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    fn assert_eq_ast(code: &str, ast: Program) {
        assert_eq!(Parser::new(code).parse().unwrap(), ast);
    }

    #[test]
    fn test_function_parsing() {
        let code = r#"
        fun return_something(a: usize) => bool {
            let something = false;
            let another_var = 3535 + 35;
            return something;
        }
        "#;

        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_if_statement_parsing() {
        let code = r#"
        if (54 > 34) {
            let something = 446;
        } else {
            let somethine = 0;
        }
        "#;
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_function_call_parsing() {
        let code = r#"
        function_call(35 + 3435, some_var);
        "#;
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_nested_function_call_parsing() {
        let code = r#"
        function_call(35 + 3435, another_func(34535, 355));
        "#;
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }

    #[test]
    fn test_expression_statement_infix() {
        let code = r#"
        5456 + 33563 + 34;
        "#;
        insta::assert_debug_snapshot!(Parser::new(code).parse());
    }
}
