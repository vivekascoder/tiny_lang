use anyhow::{bail, Result};
use clap::{Parser, Subcommand};
use std::fs;
use tiny_lang::interpreter::Interpreter;

#[derive(Parser)]
#[command(
    author="Vivek K. [vivekascoder@gmail.com]", 
    version = "0.0", 
    name = "Tiny Lang",
    about="Tiny Lang is a type safe general purpose programming language", 
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
}

fn main() -> Result<()> {
    let cli = TinyLang::parse();

    match &cli.command {
        Commands::Interpret { file } => {
            let file_name = file.clone().unwrap_or("./main.tiny".to_string());

            if let Err(_) = fs::metadata(&file_name) {
                bail!("Filename {:?} doesn't exists.", &file_name);
            }

            let program = fs::read_to_string(file_name)?;
            Interpreter::new(&program).eval()?;
        }
    }

    Ok(())
}
