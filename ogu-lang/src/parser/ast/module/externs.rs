use anyhow::Result;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::module::body::BodyAst;
use crate::parser::ast::module::decls::DeclVec;
use crate::parser::{consume_string, consume_symbol, raise_parser_error, Parser};

#[derive(Debug, Clone)]
pub(crate) struct Extern<'a>(&'a str, DeclVec<'a>);

impl<'a> Extern<'a> {
    pub(crate) fn parse(parser: &'a Parser, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Lexeme::Extern) {
            Extern::parse_extern(parser, pos)
        } else {
            Ok((None, pos))
        }
    }

    fn parse_extern(parser: &'a Parser<'a>, pos: usize) -> Result<(Option<Extern<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Extern)?;
        let (lang, pos) = consume_string(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let mut pos = parser.skip_nl(pos);
        let mut decls = vec![];
        while !parser.peek(pos, Lexeme::Dedent) {
            if let Some((decl, new_pos)) = BodyAst::parse_decl(parser, pos)? {
                decls.push(decl);
                pos = parser.skip_nl(new_pos);
            } else {
                return raise_parser_error(
                    "unexpected eof parsing extern clause",
                    parser,
                    pos,
                    false,
                );
            }
        }
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok((Some(Extern(lang, decls)), pos))
    }
}
