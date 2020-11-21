use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::{consume_symbol, ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Exposing {
    All,
    List(Vec<String>),
}

impl<'a> Exposing {
    pub(crate) fn parse(parser: &Parser<'a>, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Symbol::Exposing) {
            Exposing::exposing(parser, pos + 1)
        } else {
            Ok((None, pos))
        }
    }

    fn exposing(parser: &Parser<'a>, pos: usize) -> Result<(Option<Exposing>, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::LeftParen)?;
        let (expo, pos) = if parser.peek(pos, Symbol::DotDot) {
            (Exposing::All, pos + 1)
        } else {
            Exposing::exposing_ids(parser, pos)?
        };
        if !parser.peek(pos, Symbol::RightParen) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExposingExpectOpenParenthesis,
            )))
            .context("expecting )");
        }
        Ok((Some(expo), pos + 1))
    }

    fn exposing_ids(parser: &Parser<'a>, pos: usize) -> Result<(Exposing, usize)> {
        let mut ids = vec![];
        let mut pos = pos;
        loop {
            match parser.get_symbol(pos) {
                Some(Symbol::Id(s)) => {
                    ids.push(s.to_string());
                    pos += 1;
                }
                Some(Symbol::TypeId(s)) => {
                    ids.push(s.to_string());
                    pos += 1;
                }
                sym => {
                    return Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingIdentifier,
                    )))
                    .context(format!("Expecting identifier but found: {:?}", sym))
                }
            };
            if !parser.peek(pos, Symbol::Comma) {
                break;
            } else {
                pos += 1;
            }
        }
        Ok((Exposing::List(ids), pos))
    }
}
