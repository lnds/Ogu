use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::{LineNumber, Symbol, Token};
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
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream) -> Result<Parser> {
        Ok(Parser { tokens })
    }

    pub fn parse(&mut self, filename: &PathBuf) -> Result<Module> {
        Module::parse(self, filename, 0)
    }

    pub fn peek(&self, pos: usize, symbol: Symbol) -> bool {
        match self.tokens.peek(pos) {
            None => false,
            Some(tok) => tok.symbol == symbol,
        }
    }

    pub fn get(&self, pos: usize) -> Option<Token> {
        self.tokens.peek(pos)
    }

    pub fn get_symbol(&self, pos: usize) -> Option<Symbol> {
        self.tokens.peek(pos).map(|t| t.symbol)
    }

    pub fn skip_nl(&self, pos: usize) -> usize {
        match self.get_symbol(pos) {
            Some(Symbol::NewLine) => self.skip_nl(pos + 1),
            _ => pos,
        }
    }

    pub fn pos_to_line(&self, pos: usize) -> Option<LineNumber> {
        self.tokens.peek(pos).map(|t| t.line)
    }
}

pub fn consume_symbol(parser: &Parser, pos: usize, symbol: Symbol) -> Result<usize> {
    if !parser.peek(pos, symbol) {
        Err(Error::new(OguError::ParserError(
            ParseError::ExpectingSymbol(symbol.to_string()),
        )))
        .context(format!(
            "expecting: {:?} @ {}",
            symbol,
            parser.pos_to_line(pos).unwrap_or(0)
        ))
    } else {
        Ok(pos + 1)
    }
}

pub fn parse_opt_indent(parser: &Parser, pos: usize) -> (bool, usize) {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Symbol::Indent) {
        (true, pos + 1)
    } else {
        (false, pos)
    }
}

pub fn parse_opt_dedent(parser: &Parser, pos: usize, in_indent: bool) -> Result<usize> {
    let mut pos = parser.skip_nl(pos);
    if in_indent {
        if !parser.peek(pos, Symbol::Dedent) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIndentationEnd,
            )))
            .context("esperando fin de indentación");
        }
        pos += 1;
    }
    Ok(pos)
}

pub fn look_ahead_where(parser: &Parser, pos: usize) -> Option<usize> {
    let pos = parser.skip_nl(pos);
    if !parser.peek(pos, Symbol::Indent) {
        None
    } else {
        let pos = parser.skip_nl(pos + 1);
        if parser.peek(pos, Symbol::Where) {
            Some(pos)
        } else {
            None
        }
    }
}
