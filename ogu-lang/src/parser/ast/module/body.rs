use crate::lexer::tokens::Lexeme;
use crate::parser::{
     consume_symbol,
    raise_parser_error, Parser,
};
use anyhow::Result;
use crate::parser::ast::module::decls::{Declaration, DeclVec, DeclParseResult};

#[derive(Debug, Clone)]
pub(crate) struct BodyAst<'a> {
    pub(crate) declarations: Vec<Declaration<'a>>,
}


impl<'a> BodyAst<'a> {
    pub(crate) fn get_decls(self) -> Vec<Declaration<'a>> {
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
        Ok(result)
    }

    pub fn parse_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = parser.skip_nl(pos);
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Lexeme::Macro) => {
                let pos = consume_symbol(parser, pos, Lexeme::Macro)?;
                if let Some((decl, pos)) = BodyAst::parse_decl(parser, pos)? {
                    Ok(Some((Declaration::MacroDecl(Box::new(decl)), pos)))
                } else {
                    raise_parser_error("expecting macro declaration", parser, pos, false)
                }
            }
            Some(Lexeme::Id(_)) if parser.peek(pos + 1, Lexeme::Colon) => {
                let (proto, pos) = Declaration::parse_func_prototype(parser, pos)?;
                Ok(Some((Declaration::FunctionPrototype(proto), pos)))
            }
            Some(Lexeme::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            Some(Lexeme::Type) => Declaration::parse_type_decl(parser, pos),
            Some(Lexeme::Alias) => Declaration::parse_type_alias_decl(parser, pos),
            Some(Lexeme::Trait) => Declaration::parse_trait_decl(parser, pos),
            Some(Lexeme::Handler) => Declaration::parse_handler_decl(parser, pos),
            Some(Lexeme::Effect) => {
                let (prot, pos) = Declaration::parse_effect_func_prototype(parser, pos)?;
                Ok(Some((Declaration::Effect(prot), pos)))
            }
            Some(Lexeme::Extends) => Declaration::parse_extends(parser, pos),
            Some(Lexeme::LargeString(i)) => Ok(Some((
                Declaration::DocString(parser.get_large_string(i)),
                pos + 1,
            ))),
            _ => raise_parser_error("expecting a declaration", parser, pos, true),
        }
    }
}




