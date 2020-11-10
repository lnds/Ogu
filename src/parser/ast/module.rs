use crate::lexer::tokens::{Symbol, Token};
use crate::parser::{ParseError, Parser};
use std::path::PathBuf;

use crate::backend::OguError;
use anyhow::{Error, Result};

#[derive(Debug)]
pub struct Module {
    name: String,
}

impl<'a> Module {

    pub fn parse(parser: &'a mut Parser<'a>, filename: &PathBuf) -> Result<Self> {
        let name = if !parser.peek(Symbol::MODULE) {
            name_from_filename(filename)
        } else {
            match parser.skip(Symbol::MODULE) {
                Some(Token { symbol: Symbol::TID(s), line: _, }) => s.to_string(),

                Some(Token { symbol: Symbol::ID(s), line: l, }) => {
                    return Err(Error::new(OguError::ParserError(
                        ParseError::TypeIdExpected,

                    )).context(
                        format!("Error at line: {}, module name '{}' must start with upper case",l, s)
                    ))
                },

                t => {
                    println!("t = {:?}", t);
                    return Err(Error::new(OguError::ParserError(
                        ParseError::TypeIdExpected,
                    )).context("Expecting module name"))
                }
            }
        };
        Ok(Module { name })
    }
}

fn name_from_filename(filename: &PathBuf) -> String {
    match filename.as_path().file_stem() {
        None => String::new(),
        Some(s) => s.to_str().unwrap().to_string(),
    }
}
