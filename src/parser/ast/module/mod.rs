pub mod body;
pub mod exposing;
pub mod imports;

use crate::lexer::tokens::Symbol;
use crate::parser::{consume_qualified_type_id, Parser};
use std::path::PathBuf;

use crate::parser::ast::module::body::Body;
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::imports::Import;
use anyhow::Result;

#[derive(Debug, Clone)]
pub enum ModuleName {
    Anonymous,
    Simple(String),
    Qualified(String, Vec<String>),
}

#[derive(Debug)]
pub struct Module {
    name: ModuleName,
    exposing: Option<Exposing>,
    imports: Option<Vec<Import>>,
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
        let (imports, pos) = Import::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let body = Body::parse(parser, pos)?;
        Ok(Module {
            name,
            exposing,
            imports,
            body,
        })
    }
}

fn name_from_filename(filename: &PathBuf) -> ModuleName {
    match filename.as_path().file_stem() {
        None => ModuleName::Anonymous,
        Some(s) => ModuleName::Simple(capitalize(s.to_str().unwrap())),
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

fn name_from_parser<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(ModuleName, usize)> {
    let (t_id, names, pos) = consume_qualified_type_id(parser, pos)?;
    if names.is_empty() {
        Ok((ModuleName::Simple(t_id), pos))
    } else {
        Ok((ModuleName::Qualified(t_id, names), pos))
    }
}
