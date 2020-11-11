use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::{ParseError, Parser};
use anyhow::{Error, Result};

#[derive(Debug)]
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
        if !parser.peek(pos, Symbol::LeftParen) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExposingExpectOpenParenthesis,
            )));
        }
        let pos = pos + 1;
        let (expo, pos) = if parser.peek(pos, Symbol::DotDot) {
            (Exposing::All, pos + 1)
        } else {
            Exposing::exposing_ids(parser, pos)?
        };
        if !parser.peek(pos, Symbol::RightParen) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExposingExpectOpenParenthesis,
            )));
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
                _ => {
                    return Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingIdentifier,
                    )))
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
