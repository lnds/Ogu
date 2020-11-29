use crate::lexer::tokens::Token;
use crate::parser::ast::module::body::{Body, Declaration};
use crate::parser::{consume_string, consume_symbol, raise_parser_error, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct Extern(String, Vec<Declaration>);

impl Extern {
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Token::Extern) {
            Extern::parse_extern(parser, pos)
        } else {
            Ok((None, pos))
        }
    }

    fn parse_extern(parser: &Parser, pos: usize) -> Result<(Option<Extern>, usize)> {
        let pos = consume_symbol(parser, pos, Token::Extern)?;
        let (lang, pos) = consume_string(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Where)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut pos = parser.skip_nl(pos);
        let mut decls = vec![];
        while !parser.peek(pos, Token::Dedent) {
            if let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
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
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        Ok((Some(Extern(lang, decls)), pos))
    }
}
