use anyhow::{bail, Result};
use clap::{Parser, Subcommand};
use std::fs;
use tiny_lang::parser::Parser as AST;
use tiny_lang::{ast::Token, interpreter::Interpreter, lexer::Lexer};

#[derive(Parser)]
#[command(
    name="Tiny Lang", version, about, author,
    long_about=None
)]
struct TinyLang {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Interpret a tiny_lang program, by default it looks for `main.tiny` file.
    Interpret { file: Option<String> },

    /// Generate lexer for the given tiny_lang program.
    Lex { file: Option<String> },

    /// Prints AST generated for given tiny_lang program.
    Ast { file: Option<String> },
}

fn main() -> Result<()> {
    let cli = TinyLang::parse();

    match &cli.command {
        Commands::Interpret { file } => {
            let file_name = file.clone().unwrap_or("./main.tiny".to_string());

            if let Err(_) = fs::metadata(&file_name) {
                bail!("Filename {:?} doesn't exists.", &file_name);
            }

            let program = fs::read_to_string(&file_name)?;
            let mut i = Interpreter::new(&file_name, &program);

            if let Err(e) = i.eval() {
                // TODO: track last token position
                eprintln!("{}:{}:{} {}", i.module(), i.get_row(), i.get_col(), e);
            }
        }
        Commands::Lex { file } => {
            let file_name = file.clone().unwrap_or("./main.tiny".to_string());

            if let Err(_) = fs::metadata(&file_name) {
                bail!("Filename {:?} doesn't exists.", &file_name);
            }

            let program = fs::read_to_string(&file_name)?;
            println!(
                "{:#?}",
                Lexer::new(&file_name, &program)
                    .into_iter()
                    .collect::<Vec<Token>>()
            );
        }
        Commands::Ast { file } => {
            let file_name = file.clone().unwrap_or("./main.tiny".to_string());

            if let Err(_) = fs::metadata(&file_name) {
                bail!("Filename {:?} doesn't exists.", &file_name);
            }

            let program = fs::read_to_string(&file_name)?;
            println!("{:#?}", AST::new(&file_name, &program).parse()?);
        }
    }

    Ok(())
}
