use crate::lexer::tokens::Symbol;
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::ModuleName;
use crate::parser::{consume_qualified_type_id, consume_symbol, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Import(ModuleName, Option<Exposing>);

impl Import {
    pub fn parse(parser: &Parser, pos: usize) -> Result<(Option<Vec<Import>>, usize)> {
        let mut imports = vec![];
        let mut pos = pos;
        while let Some((import, new_pos)) = Import::parse_import(parser, pos)? {
            imports.push(import);
            pos = new_pos;
        }
        if imports.is_empty() {
            Ok((None, pos))
        } else {
            Ok((Some(imports), pos))
        }
    }

    fn parse_import(parser: &Parser, pos: usize) -> Result<Option<(Import, usize)>> {
        if !parser.peek(pos, Symbol::Import) {
            Ok(None)
        } else {
            let pos = consume_symbol(parser, pos, Symbol::Import)?;
            let (tid, names, pos) = consume_qualified_type_id(parser, pos)?;
            let mod_name = if names.is_empty() {
                ModuleName::Simple(tid)
            } else {
                ModuleName::Qualified(tid, names)
            };
            let (exposing, pos) = Exposing::parse(parser, pos)?;
            Ok(Some((Import(mod_name, exposing), pos)))
        }
    }
}
