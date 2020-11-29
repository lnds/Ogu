pub mod body;
pub mod exposing;
pub mod externs;
pub mod imports;

use crate::lexer::tokens::Token;
use crate::parser::{consume_qualified_type_id, consume_symbol, Parser};
use std::path::PathBuf;

use crate::parser::ast::module::body::{Body, Declaration};
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::externs::Extern;
use crate::parser::ast::module::imports::Import;
use anyhow::Result;

#[derive(Debug, Clone)]
pub enum ModuleName {
    Anonymous,
    Simple(String),
    Qualified(String, Vec<String>),
}

#[derive(Debug)]
pub(crate) struct Module {
    name: ModuleName,
    exposing: Option<Exposing>,
    imports: Option<Vec<Import>>,
    externs: Option<Extern>,
    body: Body,
}

impl Module {
    pub(crate) fn get_module_name(&self) -> String {
        match &self.name {
            ModuleName::Anonymous => String::new(),
            ModuleName::Simple(s) => s.to_string(),
            ModuleName::Qualified(s, sl) => format!("{}.{}", s, sl.join(".")),
        }
    }

    pub(crate) fn get_exposed_names(&mut self) -> Vec<String> {
        match &self.exposing {
            None => vec![],
            Some(Exposing::All) => {
                let mut result = vec![];
                for decl in self.get_decls().iter() {
                    result.push(decl.get_name())
                }
                result
            }
            Some(Exposing::List(v)) => v.clone(),
        }
    }

    fn get_decls(&mut self) -> Vec<Declaration> {
        self.body.get_decls()
    }

    pub(crate) fn get_decl_by_name(&mut self, name: &String) -> Option<Declaration> {
        for decl in self.body.get_decls().iter() {
            if decl.get_name() == *name {
                return Some(decl.clone());
            }
        }
        None
    }
}

impl<'a> Module {
    pub fn parse(parser: &'a Parser<'a>, filename: &PathBuf, pos: usize) -> Result<Self> {
        let (name, pos) = if parser.peek(pos, Token::Module) {
            name_from_parser(parser, pos)?
        } else {
            (name_from_filename(filename), pos)
        };
        let pos = parser.skip_nl(pos);
        let (exposing, pos) = Exposing::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let (imports, pos) = Import::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let (externs, pos) = Extern::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let body = Body::parse(parser, pos)?;
        Ok(Module {
            name,
            exposing,
            imports,
            externs,
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

fn name_from_parser(parser: &Parser, pos: usize) -> Result<(ModuleName, usize)> {
    let pos = consume_symbol(parser, pos, Token::Module)?;
    let (t_id, names, pos) = consume_qualified_type_id(parser, pos)?;
    if names.is_empty() {
        Ok((ModuleName::Simple(t_id), pos))
    } else {
        Ok((ModuleName::Qualified(t_id, names), pos))
    }
}
