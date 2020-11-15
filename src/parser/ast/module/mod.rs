pub mod body;
pub mod exposing;

use crate::lexer::tokens::{Symbol, Token};
use crate::parser::{ParseError, Parser};
use std::path::PathBuf;

use crate::backend::OguError;
use crate::parser::ast::module::body::Body;
use crate::parser::ast::module::exposing::Exposing;
use anyhow::{Error, Result};

#[derive(Debug)]
pub struct Module {
    name: String,
    exposing: Option<Exposing>,
    body: Body,
}

impl<'a> Module {
    pub fn parse(parser: &'a Parser<'a>, filename: &PathBuf, pos: usize) -> Result<Self> {
        let (name, pos) = if parser.peek(pos, Symbol::Module) {
            name_from_parser(parser, pos + 1)?
        } else {
            (name_from_filename(filename), pos)
        };
        let (exposing, pos) = Exposing::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let body = Body::parse(parser, pos)?;
        Ok(Module {
            name,
            exposing,
            body,
        })
    }
}

fn name_from_filename(filename: &PathBuf) -> String {
    match filename.as_path().file_stem() {
        None => String::new(),
        Some(s) => capitalize(s.to_str().unwrap()),
    }
}

fn capitalize(s: &str) -> String {
    if s.is_empty() {
        String::new()
    } else {
        let mut v: Vec<char> = s.chars().collect();
        v[0] = v[0].to_uppercase().next().unwrap();
        v.into_iter().collect()
    }
}

fn name_from_parser<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(String, usize)> {
    match parser.get(pos) {
        Some(Token {
            symbol: Symbol::TypeId(s),
            line: _,
        }) => Ok((s.to_string(), pos + 1)),

        Some(Token {
            symbol: Symbol::Id(s),
            line: l,
        }) => Err(
            Error::new(OguError::ParserError(ParseError::TypeIdExpected)).context(format!(
                "Error at line: {}, module name '{}' must start with upper case",
                l, s
            )),
        ),

        _ => Err(
            Error::new(OguError::ParserError(ParseError::TypeIdExpected))
                .context("Expecting module name"),
        ),
    }
}
