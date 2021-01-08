use anyhow::{Error, Result};

use crate::backend::errors::OguError;
use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::{Lexeme, LineNumber, LineWidth};

pub(crate) mod ast;

#[derive(Clone)]
pub(crate) struct Parser<'a> {
    tokens: TokenStream<'a>,
    large_strings: Vec<String>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: TokenStream<'a>, large_strings: Vec<String>) -> Result<Parser<'a>> {
        Ok(Parser {
            tokens,
            large_strings,
        })
    }

    pub(crate) fn peek(&self, pos: usize, symbol: Lexeme) -> bool {
        match self.tokens.peek(pos) {
            None => false,
            Some(tok) => tok.lexeme == symbol,
        }
    }

    pub(crate) fn get_token(&self, pos: usize) -> Option<Lexeme> {
        self.tokens.peek(pos).map(|t| t.lexeme)
    }

    pub(crate) fn skip_nl(&self, pos: usize) -> usize {
        match self.get_token(pos) {
            Some(Lexeme::NewLine) => self.skip_nl(pos + 1),
            _ => pos,
        }
    }

    pub(crate) fn pos_to_line_col(&self, pos: usize) -> Option<(LineNumber, LineWidth)> {
        self.tokens.peek(pos).map(|t| (t.line, t.col))
    }

    pub(crate) fn get_large_string(&self, index: usize) -> Option<String> {
        self.large_strings.get(index).cloned()
    }
}

pub(crate) fn consume_symbol(parser: &Parser, pos: usize, token: Lexeme) -> Result<usize> {
    if !parser.peek(pos, token) {
        raise_parser_error(&format!("Expecting {:?}", token), parser, pos, true)
    } else {
        Ok(pos + 1)
    }
}

pub(crate) fn consume_opt_symbol(parser: &Parser, pos: usize, token: Lexeme) -> Result<usize> {
    if parser.peek(pos, token) {
        consume_symbol(parser, pos, token)
    } else {
        Ok(pos)
    }
}

pub(crate) fn parse_opt_indent<'a>(parser: &'a Parser<'a>, pos: usize) -> (bool, usize) {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Lexeme::Indent) {
        (true, pos + 1)
    } else {
        (false, pos)
    }
}

pub(crate) fn parse_opt_dedent<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    in_indent: bool,
) -> Result<usize> {
    let mut pos = parser.skip_nl(pos);
    if in_indent {
        pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
    }
    Ok(pos)
}

pub(crate) fn parse_opt_where_or_dedent(
    parser: &Parser,
    pos: usize,
    in_indent: bool,
) -> Result<usize> {
    let mut pos = parser.skip_nl(pos);
    if in_indent {
        if parser.peek(pos, Lexeme::Dedent) {
            pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        } else if !parser.peek(pos, Lexeme::Where) {
            return raise_parser_error("Expecting 'where'", parser, pos, true);
        }
    }
    Ok(pos)
}

pub(crate) fn look_ahead_where(parser: &Parser, pos: usize) -> Option<usize> {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Lexeme::Where) {
        Some(pos)
    } else if parser.peek(pos, Lexeme::Indent) {
        let pos = parser.skip_nl(pos + 1);
        if parser.peek(pos, Lexeme::Where) {
            Some(pos)
        } else {
            None
        }
    } else {
        None
    }
}

pub(crate) fn consume_string<'a>(parser: &'a Parser, pos: usize) -> Result<(&'a str, usize)> {
    match parser.get_token(pos) {
        Some(Lexeme::String(s)) => Ok((s, pos + 1)),
        _ => raise_parser_error("Expecting an string", parser, pos, true),
    }
}

pub(crate) fn consume_type_id<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(&'a str, usize)> {
    match parser.get_token(pos) {
        Some(Lexeme::TypeId(type_id)) => Ok((type_id, pos + 1)),
        _ => raise_parser_error("expecting a type identifier", parser, pos, true),
    }
}

pub(crate) fn consume_qualified_type_id<'a>(
    parser: &'a Parser,
    pos: usize,
) -> Result<(&'a str, Vec<&'a str>, usize)> {
    let (t_id, mut pos) = consume_type_id(parser, pos)?;
    let mut names = vec![];
    while parser.peek(pos, Lexeme::Dot) {
        pos = consume_symbol(parser, pos, Lexeme::Dot)?;
        let (t_id, new_pos) = consume_type_id(parser, pos)?;
        names.push(t_id);
        pos = new_pos;
    }
    Ok((t_id, names, pos))
}

pub(crate) fn raise_parser_error<T>(
    msg: &str,
    parser: &Parser,
    pos: usize,
    show_token: bool,
) -> Result<T> {
    let position = if let Some((line, col)) = parser.pos_to_line_col(pos) {
        format!("@ line = {}, col = {}", line, col)
    } else {
        "@ EOF".to_string()
    };
    if show_token {
        Err(Error::new(OguError::ParserError).context(format!(
            "Error {} {}, token found = {}",
            msg,
            position,
            parser.get_token(pos).unwrap_or(Lexeme::Error)
        )))
    } else {
        Err(Error::new(OguError::ParserError).context(format!("Error: {} {}", msg, position)))
    }
}
