use anyhow::bail;

use crate::ast::Token;
use crate::interpreter::Interpreter;
use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser as AST;
use crate::scope::ScopeStack;
use std::io;
use std::io::Write;

pub fn start_repl(with: &Option<String>) -> anyhow::Result<()> {
    let stdin = io::stdin();
    println!("\n# Tiny Lang Repl.");
    println!("Press <C-c> to exit.\n");
    let mut scope = ScopeStack::new();
    scope.push_scope();
    loop {
        let mut code = String::new();
        print!("|> ");
        io::stdout().flush()?;
        stdin.read_line(&mut code)?;
        match with.as_ref().unwrap_or(&"ast".to_string()).as_str() {
            "ast" => {
                let val = AST::new("", &code).parse();
                match val {
                    Ok(ast) => {
                        println!("{:#?}", ast);
                    }
                    Err(e) => {
                        eprintln!("Error: {:?}", e);
                    }
                };
            }
            "lex" => {
                let val = Lexer::new("", &code).into_iter().collect::<Vec<Token>>();
                println!("{:#?}", val);
            }
            "interpret" => {
                let mut i = Interpreter::from_scope("", &code, scope.clone());

                if let Err(e) = i.eval() {
                    eprintln!("{}", e);
                }

                scope = i.get_stack();
            }
            _ => {
                bail!("som")
            }
        };
    }
}
