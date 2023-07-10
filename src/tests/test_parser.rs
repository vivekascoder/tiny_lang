#[cfg(test)]
pub mod tests {
    use crate::ast::*;
    use crate::parser::Parser;

    #[test]
    fn test_let_statement_parsing() {
        let code = "let something = 454 + 3 * 4 - 35; ";
        let mut parser = Parser::new(code);
        // println!("Parsed statements: {:#?}", parser.parse());
        assert_eq!(
            parser.parse().unwrap(),
            [Statement::Let(
                Ident("something".to_string()),
                Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::UnsignedInteger(454,))),
                        Box::new(Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::UnsignedInteger(3))),
                            Box::new(Expr::Literal(Literal::UnsignedInteger(4))),
                        )),
                    )),
                    Box::new(Expr::Literal(Literal::UnsignedInteger(35,)))
                ),
            )]
        );
    }

    #[test]
    fn test_multiple_let_statement_parsing() {
        let code = r#"
        let something = 454 + 3 * 4 - 35;
        let a = 35 / 3;
        "#;
        let mut parser = Parser::new(code);
        println!("Parsed statements: {:#?}", parser.parse());
        assert_eq_ast(
            code,
            vec![
                Statement::Let(
                    Ident("something".to_string()),
                    Expr::Infix(
                        Infix::Minus,
                        Box::new(Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Literal(Literal::UnsignedInteger(454))),
                            Box::new(Expr::Infix(
                                Infix::Multiply,
                                Box::new(Expr::Literal(Literal::UnsignedInteger(3))),
                                Box::new(Expr::Literal(Literal::UnsignedInteger(4))),
                            )),
                        )),
                        Box::new(Expr::Literal(Literal::UnsignedInteger(35))),
                    ),
                ),
                Statement::Let(
                    Ident("a".to_string()),
                    Expr::Infix(
                        Infix::Divide,
                        Box::new(Expr::Literal(Literal::UnsignedInteger(35))),
                        Box::new(Expr::Literal(Literal::UnsignedInteger(3))),
                    ),
                ),
            ],
        )
    }

    #[test]
    fn test_prefix_expression_parsing() {
        let code = "let val = +454;";
        let mut parser = Parser::new(code);
        // println!("Parsed statement: {:#?}", parser.parse());
        assert_eq!(
            parser.parse().unwrap(),
            [Statement::Let(
                Ident("val".to_string()),
                Expr::Prefix(
                    Prefix::Plus,
                    Box::new(Expr::Literal(Literal::UnsignedInteger(454)))
                )
            )]
        )
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
        let mut parser = Parser::new(code);

        // println!("Parsed: {:#?}", parser.parse());
        assert_eq!(
            parser.parse().unwrap(),
            [Statement::Function(Function {
                name: "return_something".to_string(),
                params: vec![(Ident("a".to_string()), Type::UnsignedInteger)],
                return_type: Some(Type::Bool),
                body: vec![
                    Statement::Let(
                        Ident("something".to_string()),
                        Expr::Literal(Literal::Bool(false))
                    ),
                    Statement::Let(
                        Ident("another_var".to_string()),
                        Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Literal(Literal::UnsignedInteger(3535))),
                            Box::new(Expr::Literal(Literal::UnsignedInteger(35)))
                        )
                    ),
                    Statement::Return(Expr::Ident(Ident("something".to_string()))),
                ],
            })]
        )
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
        let ast: Program = vec![Statement::If(Condition {
            condition: Expr::Infix(
                Infix::GreaterThan,
                Box::new(Expr::Literal(Literal::UnsignedInteger(54))),
                Box::new(Expr::Literal(Literal::UnsignedInteger(34))),
            ),
            if_body: vec![Statement::Let(
                Ident("something".to_string()),
                Expr::Literal(Literal::UnsignedInteger(446)),
            )],
            else_body: Some(vec![Statement::Let(
                Ident("somethine".to_string()),
                Expr::Literal(Literal::UnsignedInteger(0)),
            )]),
        })];
        assert_eq_ast(code, ast);
    }

    #[test]
    fn test_function_call_parsing() {
        let code = r#"
        function_call(35 + 3435, some_var);
        "#;
        println!("{:#?}", Parser::new(code).parse().unwrap());
        assert_eq_ast(
            code,
            vec![Statement::Expr(Expr::Call(FunctionCall {
                parameters: vec![
                    Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::UnsignedInteger(35))),
                        Box::new(Expr::Literal(Literal::UnsignedInteger(3435))),
                    ),
                    Expr::Ident(Ident("some_var".to_string())),
                ],
                name: "function_call".to_string(),
            }))],
        )
    }

    #[test]
    fn test_nested_function_call_parsing() {
        let code = r#"
        function_call(35 + 3435, another_func(34535, 355));
        "#;
        println!("{:#?}", Parser::new(code).parse().unwrap());
        //     assert_eq_ast(
        //         code,
        //         vec![Statement::Expr(Expr::Call(FunctionCall {
        //             parameters: vec![
        //                 Expr::Infix(
        //                     Infix::Plus,
        //                     Box::new(Expr::Literal(Literal::UnsignedInteger(35))),
        //                     Box::new(Expr::Literal(Literal::UnsignedInteger(3435))),
        //                 ),
        //                 Expr::Ident(Ident("some_var".to_string())),
        //             ],
        //             name: "function_call".to_string(),
        //         }))],
        //     )
    }

    #[test]
    fn test_expression_statement_infix() {
        let code = r#"
        5456 + 33563 + 34;
        "#;
        println!("{:#?}", Parser::new(code).parse().unwrap());
    }
}
