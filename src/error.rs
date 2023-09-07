use crate::ast::Token;
use std::{fmt::Display, rc::Rc};

#[derive(Debug)]
pub enum TinyError {
    ParserError(ParserError),
    CompilationError(CompilationError),
}

impl TinyError {
    pub fn new_parser_error(token: Rc<Token>, file_path: Rc<str>, info: String) -> Self {
        TinyError::ParserError(ParserError {
            token: token,
            file_path: file_path,
            info: info,
        })
    }
    pub fn new_compilation_error(file_path: Rc<str>, info: String) -> Self {
        TinyError::CompilationError(CompilationError {
            file_path: file_path,
            info: info,
        })
    }
}

#[derive(Debug)]
pub struct ParserError {
    token: Rc<Token>,
    file_path: Rc<str>,
    info: String,
}

#[derive(Debug)]
pub struct CompilationError {
    file_path: Rc<str>,
    info: String,
}

impl Display for TinyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TinyError::ParserError(t) => write!(
                f,
                "ParserError: {}:{}:{}\nInfo: {}",
                t.file_path,
                t.token.as_ref().row,
                t.token.as_ref().col,
                t.info,
            ),
            TinyError::CompilationError(c) => {
                write!(f, "CompilationError in {}: {}", c.file_path, c.info)
            }
        }
    }
}
