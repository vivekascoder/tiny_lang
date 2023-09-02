use std::{fmt::Display, rc::Rc};

use crate::ast::Token;

#[derive(Debug)]
pub enum TinyError {
    ParserError(ParserError),
}

impl TinyError {
    pub fn new_parser_error(token: Rc<Token>, file_path: Rc<str>, info: String) -> Self {
        TinyError::ParserError(ParserError {
            token: token,
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
        }
    }
}
