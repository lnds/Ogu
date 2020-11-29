use crate::backend::OguError;
use crate::lexer::tokens::Token;
use crate::parser::{consume_symbol, ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Exposing {
    All,
    List(Vec<String>),
}

impl<'a> Exposing {
    pub(crate) fn parse(parser: &Parser<'a>, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Token::Exposing) {
            Exposing::exposing(parser, pos + 1)
        } else {
            Ok((None, pos))
        }
    }

    fn exposing(parser: &Parser<'a>, pos: usize) -> Result<(Option<Exposing>, usize)> {
        let pos = consume_symbol(parser, pos, Token::LeftParen)?;
        let (expo, pos) = if parser.peek(pos, Token::DotDot) {
            (Exposing::All, pos + 1)
        } else {
            Exposing::exposing_ids(parser, pos)?
        };
        if !parser.peek(pos, Token::RightParen) {
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
            match parser.get_token(pos) {
                Some(Token::Id(s)) => {
                    ids.push(s.to_string());
                    pos += 1;
                }
                Some(Token::TypeId(s)) => {
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
            if !parser.peek(pos, Token::Comma) {
                break;
            } else {
                pos += 1;
            }
        }
        Ok((Exposing::List(ids), pos))
    }
}
