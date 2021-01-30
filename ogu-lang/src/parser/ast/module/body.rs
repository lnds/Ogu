use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::module::decls::{DeclParseResult, DeclVec, DeclarationAst};
use crate::parser::{consume_symbol, raise_parser_error, Parser};

#[derive(Debug, Clone)]
pub(crate) struct BodyAst<'a> {
    pub(crate) declarations: Vec<DeclarationAst<'a>>,
}

impl<'a> BodyAst<'a> {
    pub(crate) fn get_decls(self) -> Vec<DeclarationAst<'a>> {
        self.declarations.to_vec()
    }

    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> Result<BodyAst<'a>> {
        let declarations = BodyAst::parse_decls(parser, pos)?;
        Ok(BodyAst { declarations })
    }

    fn parse_decls(parser: &'a Parser<'a>, pos: usize) -> Result<DeclVec<'a>> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((decl, new_pos)) = BodyAst::parse_decl(parser, pos)? {
            result.push(decl);
            pos = new_pos;
        }
        BodyAst::merge_functions(result)
    }

    fn merge_functions(decls: DeclVec<'a>) -> Result<DeclVec<'a>> {
        let mut funcs = HashMap::new();
        for decl in decls.iter() {
            match decl {
                DeclarationAst::Function(name, _, _, _)
                | DeclarationAst::FunctionWithGuards(name, _, _, _)
                | DeclarationAst::FunctionPrototype(name, _) => funcs
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(decl.clone()),
                _ => {}
            }
        }
        let mut result = vec![];
        for decl in decls.iter() {
            match decl {
                DeclarationAst::Function(name, _, _, _)
                | DeclarationAst::FunctionWithGuards(name, _, _, _)
                | DeclarationAst::FunctionPrototype(name, _) => {
                    if let Some((_, vec_of_func)) = funcs.remove_entry(&name) {
                        result.push(BodyAst::merge_vec_of_functions(&name, &vec_of_func)?);
                    }
                }
                _ => result.push(decl.clone()),
            }
        }
        Ok(result)
    }

    pub fn parse_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = parser.skip_nl(pos);
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Lexeme::Macro) => {
                let pos = consume_symbol(parser, pos, Lexeme::Macro)?;
                if let Some((decl, pos)) = BodyAst::parse_decl(parser, pos)? {
                    Ok(Some((DeclarationAst::MacroDecl(Box::new(decl)), pos)))
                } else {
                    raise_parser_error("expecting macro declaration", parser, pos, false)
                }
            }
            Some(Lexeme::Id(_)) if parser.peek(pos + 1, Lexeme::Colon) => {
                let (name, ty, pos) = DeclarationAst::parse_func_prototype(parser, pos)?;
                Ok(Some((DeclarationAst::FunctionPrototype(name, ty), pos)))
            }
            Some(Lexeme::Id(_)) => DeclarationAst::parse_func_or_val(parser, pos),
            Some(Lexeme::Type) => DeclarationAst::parse_type_decl(parser, pos),
            Some(Lexeme::Alias) => DeclarationAst::parse_type_alias_decl(parser, pos),
            Some(Lexeme::Trait) => DeclarationAst::parse_trait_decl(parser, pos),
            Some(Lexeme::Handler) => DeclarationAst::parse_handler_decl(parser, pos),
            Some(Lexeme::Effect) => {
                let (name, ty, pos) = DeclarationAst::parse_effect_func_prototype(parser, pos)?;
                Ok(Some((DeclarationAst::EffectPrototype(name, ty), pos)))
            }
            Some(Lexeme::Extends) => DeclarationAst::parse_extends(parser, pos),
            Some(Lexeme::LargeString(i)) => Ok(Some((
                DeclarationAst::DocString(parser.get_large_string(i)),
                pos + 1,
            ))),
            _ => raise_parser_error("expecting a declaration", parser, pos, true),
        }
    }
}
