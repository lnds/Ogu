use crate::lexer::tokens::Token;
use crate::parser::{consume_symbol, raise_parser_error, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) enum Exposing<'a> {
    All,
    List(Vec<&'a str>),
}

impl<'a> Exposing<'a> {
    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> Result<(Option<Self>, usize)> {
        if parser.peek(pos, Token::Exposing) {
            Exposing::exposing(parser, pos + 1)
        } else {
            Ok((None, pos))
        }
    }

    fn exposing(parser: &'a Parser<'a>, pos: usize) -> Result<(Option<Exposing<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Token::LeftParen)?;
        let (expo, pos) = if parser.peek(pos, Token::DotDot) {
            (Exposing::All, pos + 1)
        } else {
            Exposing::exposing_ids(parser, pos)?
        };
        if !parser.peek(pos, Token::RightParen) {
            return raise_parser_error("expecting ')' in exposing clause", parser, pos, false);
        }
        Ok((Some(expo), pos + 1))
    }

    fn exposing_ids(parser: &'a Parser<'a>, pos: usize) -> Result<(Exposing<'a>, usize)> {
        let mut ids = vec![];
        let mut pos = pos;
        loop {
            match parser.get_token(pos) {
                Some(Token::Id(s)) => {
                    ids.push(s);
                    pos += 1;
                }
                Some(Token::TypeId(s)) => {
                    ids.push(s);
                    pos += 1;
                }
                _ => {
                    return raise_parser_error(
                        "expecting identifier in list of exposing",
                        parser,
                        pos,
                        true,
                    );
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
