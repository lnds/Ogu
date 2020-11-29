use crate::lexer::tokens::Token;
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::ModuleName;
use crate::parser::{consume_qualified_type_id, consume_symbol, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Import(ModuleName, Option<ModuleName>, Option<Exposing>);

impl Import {
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<(Option<Vec<Import>>, usize)> {
        let mut imports = vec![];
        let mut pos = pos;
        while let Some((import, new_pos)) = Import::parse_import(parser, pos)? {
            imports.push(import);
            pos = parser.skip_nl(new_pos);
        }
        if imports.is_empty() {
            Ok((None, pos))
        } else {
            Ok((Some(imports), pos))
        }
    }

    fn parse_import(parser: &Parser, pos: usize) -> Result<Option<(Import, usize)>> {
        if !parser.peek(pos, Token::Import) {
            Ok(None)
        } else {
            let pos = consume_symbol(parser, pos, Token::Import)?;
            let (mod_name, pos) = Self::parse_module_name(parser, pos)?;

            let (alias, pos) = if parser.peek(pos, Token::As) {
                let pos = consume_symbol(parser, pos, Token::As)?;
                let (m, pos) = Self::parse_module_name(parser, pos)?;
                (Some(m), pos)
            } else {
                (None, pos)
            };
            let (exposing, pos) = Exposing::parse(parser, pos)?;
            Ok(Some((Import(mod_name, alias, exposing), pos)))
        }
    }

    fn parse_module_name(parser: &Parser, pos: usize) -> Result<(ModuleName, usize)> {
        let (tid, names, pos) = consume_qualified_type_id(parser, pos)?;
        if names.is_empty() {
            Ok((ModuleName::Simple(tid), pos))
        } else {
            Ok((ModuleName::Qualified(tid, names), pos))
        }
    }
}
