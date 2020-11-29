use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::{LineNumber, LineWidth, Token, TokenContext};
use crate::parser::ast::module::Module;
use std::path::PathBuf;

use crate::backend::OguError;
use anyhow::{Context, Error, Result};
use thiserror::Error;

pub mod ast;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected a Type ID")]
    TypeIdExpected,
    #[error("Expecting symbol")]
    ExpectingSymbol(String),
    #[error("Expecting '(' after exposing keyword")]
    ExposingExpectOpenParenthesis,
    #[error("Expecting identifier")]
    ExpectingIdentifier,
    #[error("Expecting module declartion")]
    ExpectingDeclaration,
    #[error("Expecting =")]
    ExpectingAssignation,
    #[error("Expecting valid arg")]
    ExpectingValidArg,
    #[error("Expecting indentation")]
    ExpectingIndentation,
    #[error("Expecting end of indentation")]
    ExpectingIndentationEnd,
    #[error("Expecting ,")]
    ExpectingComma,
    #[error("ExpressionExpected")]
    ExpressionExpected,
    #[error("Expecting a valid arg")]
    InvalidArg,
    #[error("Expecting a lambda arg")]
    ExpectingLambdaArg,
    #[error("Expecting ->")]
    ExpectingArrow,
    #[error("Expecting (")]
    ExpectingLeftParenthesis,
    #[error("Expecting )")]
    ExpectingRightParenthesis,
    #[error("Expecting do")]
    ExpectingDo,
    #[error("Expecting let")]
    ExpectingLet,
    #[error("Expecting in")]
    ExpectingIn,
    #[error("Expecting where")]
    ExpectingWhere,
    #[error("EOF unexpected")]
    EofUnexpected,
    #[error("Expecting operator")]
    ExpectingOperator,
    #[error("Invalid declaration")]
    InvalidDeclaration,
    #[error("Expecting type identifier")]
    ExpectingTypeIdentifier,
    #[error("Expecting string")]
    ExpectingString,
    #[error("Unexpected token")]
    UnexpectedToken,
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
    large_strings: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream<'a>, large_strings: Vec<String>) -> Result<Parser<'a>> {
        Ok(Parser {
            tokens,
            large_strings,
        })
    }

    pub fn parse(&mut self, filename: &PathBuf) -> Result<Module> {
        Module::parse(self, filename, 0)
    }

    pub fn peek(&self, pos: usize, symbol: Token) -> bool {
        match self.tokens.peek(pos) {
            None => false,
            Some(tok) => tok.token == symbol,
        }
    }

    pub fn get(&self, pos: usize) -> Option<TokenContext> {
        self.tokens.peek(pos)
    }

    pub fn get_token(&self, pos: usize) -> Option<Token> {
        self.tokens.peek(pos).map(|t| t.token)
    }

    pub fn skip_nl(&self, pos: usize) -> usize {
        match self.get_token(pos) {
            Some(Token::NewLine) => self.skip_nl(pos + 1),
            _ => pos,
        }
    }

    pub fn pos_to_line_col(&self, pos: usize) -> Option<(LineNumber, LineWidth)> {
        self.tokens.peek(pos).map(|t| (t.line, t.col))
    }

    pub fn get_large_string(&self, index: usize) -> Option<String> {
        self.large_strings.get(index).cloned()
    }

    pub fn set_large_strings(&mut self, strs: Vec<String>) {
        self.large_strings = strs;
    }
}

pub fn consume_symbol(parser: &Parser, pos: usize, symbol: Token) -> Result<usize> {
    if !parser.peek(pos, symbol) {
        Err(Error::new(OguError::ParserError(
            ParseError::ExpectingSymbol(symbol.to_string()),
        )))
        .context(format!(
            "expecting: {:?} @ {:?}, found = {:?}",
            symbol,
            parser.pos_to_line_col(pos),
            parser.get_token(pos)
        ))
    } else {
        Ok(pos + 1)
    }
}

pub fn consume_opt_symbol(parser: &Parser, pos: usize, symbol: Token) -> Result<usize> {
    if parser.peek(pos, symbol) {
        consume_symbol(parser, pos, symbol)
    } else {
        Ok(pos)
    }
}

pub fn parse_opt_indent(parser: &Parser, pos: usize) -> (bool, usize) {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Token::Indent) {
        (true, pos + 1)
    } else {
        (false, pos)
    }
}

pub fn parse_opt_dedent(parser: &Parser, pos: usize, in_indent: bool) -> Result<usize> {
    let mut pos = parser.skip_nl(pos);
    if in_indent {
        pos = consume_symbol(parser, pos, Token::Dedent)?;
    }
    Ok(pos)
}

pub fn parse_opt_where_or_dedent(parser: &Parser, pos: usize, in_indent: bool) -> Result<usize> {
    let mut pos = parser.skip_nl(pos);
    if in_indent {
        if parser.peek(pos, Token::Dedent) {
            pos = consume_symbol(parser, pos, Token::Dedent)?;
        } else if !parser.peek(pos, Token::Where) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingWhere,
            )))
            .context(format!(
                "Expecting where @{:?}",
                parser.pos_to_line_col(pos)
            ));
        }
    }
    Ok(pos)
}

pub fn look_ahead_where(parser: &Parser, pos: usize) -> Option<usize> {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Token::Where) {
        Some(pos)
    } else if parser.peek(pos, Token::Indent) {
        let pos = parser.skip_nl(pos + 1);
        if parser.peek(pos, Token::Where) {
            Some(pos)
        } else {
            None
        }
    } else {
        None
    }
}

pub fn consume_string(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    match parser.get_token(pos) {
        Some(Token::String(s)) => Ok((s.to_string(), pos + 1)),
        sym => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingString,
        )))
        .context(format!(
            "Expecting string, but found: {:?} @{:?}",
            sym,
            parser.pos_to_line_col(pos)
        )),
    }
}

pub fn consume_type_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    match parser.get_token(pos) {
        Some(Token::TypeId(type_id)) => Ok((type_id.to_string(), pos + 1)),
        sym => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingTypeIdentifier,
        )))
        .context(format!(
            "Expecting type id found: {:?} @{:?}",
            sym,
            parser.pos_to_line_col(pos)
        )),
    }
}

pub fn consume_qualified_type_id(
    parser: &Parser,
    pos: usize,
) -> Result<(String, Vec<String>, usize)> {
    let (t_id, mut pos) = consume_type_id(parser, pos)?;
    let mut names = vec![];
    while parser.peek(pos, Token::Dot) {
        pos = consume_symbol(parser, pos, Token::Dot)?;
        let (t_id, new_pos) = consume_type_id(parser, pos)?;
        names.push(t_id);
        pos = new_pos;
    }
    Ok((t_id, names, pos))
}
pub fn consume_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    match parser.get_token(pos) {
        Some(Token::Id(id)) => Ok((id.to_string(), pos + 1)),
        sym => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingIdentifier,
        )))
        .context(format!(
            "Expecting id found: {:?} @{:?}",
            sym,
            parser.pos_to_line_col(pos)
        )),
    }
}
