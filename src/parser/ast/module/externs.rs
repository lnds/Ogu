use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::module::body::{Body, Declaration};
use crate::parser::{consume_string, consume_symbol, ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub struct Extern(String, Vec<Declaration>);

impl Extern {
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Symbol::Extern) {
            Extern::parse_extern(parser, pos)
        } else {
            Ok((None, pos))
        }
    }

    fn parse_extern(parser: &Parser, pos: usize) -> Result<(Option<Extern>, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::Extern)?;
        let (lang, pos) = consume_string(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut pos = parser.skip_nl(pos);
        let mut decls = vec![];
        while !parser.peek(pos, Symbol::Dedent) {
            if let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
                decls.push(decl);
                pos = parser.skip_nl(new_pos);
            } else {
                return Err(Error::new(OguError::ParserError(ParseError::EofUnexpected))).context(
                    format!("unexpected eof @{}", parser.pos_to_line(pos).unwrap_or(0)),
                );
            }
        }
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        Ok((Some(Extern(lang, decls)), pos))
    }
}
