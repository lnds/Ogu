use crate::parser::{Parser, ParseError};
use crate::lexer::tokens::Symbol;
use anyhow::{Result, Error};
use crate::backend::OguError;
use crate::parser::ParseError::ExpectingSymbol;

#[derive(Debug)]
pub enum Exposing {
    Nothing,
    All,
    List(Vec<String>),
}

impl<'a> Exposing {

    pub(crate) fn parse(parser: &Parser<'a>, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Symbol::EXPOSING) {
            Exposing::exposing(parser, pos+1)
        }
        else {
            Ok((None, pos))
        }
    }

    fn exposing(parser: &Parser<'a>, pos: usize) -> Result<(Option<Exposing>, usize)> {
        if !parser.peek(pos, Symbol::LPAREN) {
            return Err(Error::new(OguError::ParserError(ParseError::ExposingExpectOpenParenthesis)));
        }
        let pos = pos + 1;
        let (expo, pos) = if parser.peek(pos, Symbol::DOTDOT) {
            (Exposing::All, pos+1)
        } else {
            Exposing::exposing_ids(parser, pos)?
        };
        if !parser.peek(pos, Symbol::RPAREN) {
            return Err(Error::new(OguError::ParserError(ParseError::ExposingExpectOpenParenthesis)));
        }
        Ok((Some(expo), pos+1))
    }

    fn exposing_ids(parser: &Parser<'a>, pos: usize) -> Result<(Exposing, usize)> {
        let mut ids = vec![];
        let mut pos = pos;
        loop {
            let id = match parser.get_symbol(pos) {
                Some(Symbol::ID(s)) => {
                    ids.push(s.to_string());
                    pos += 1;
                }
                Some(Symbol::TID(s)) => {
                    ids.push(s.to_string());
                    pos += 1;
                }
                _ => return Err(Error::new(OguError::ParserError(ParseError::ExpectingIdentifier)))
            };
            if !parser.peek(pos, Symbol::COMMA) {
                break;
            }
            else {
                pos += 1;
            }
        }

        Ok((Exposing::List(ids), pos))
    }
}