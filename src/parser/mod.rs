use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::{Symbol, Token};
use crate::parser::ast::module::Module;
use std::path::PathBuf;

use anyhow::Result;
use thiserror::Error;

mod ast;

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
}
