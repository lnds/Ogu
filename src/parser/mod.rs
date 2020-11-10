use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::{LineNumber, Symbol, Token};
use crate::parser::ast::module::Module;
use std::path::PathBuf;

use anyhow::{Result, Error};
use thiserror::Error;
use crate::backend::OguError;
use crate::parser::ParseError::ExpectingSymbol;

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
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
    current_line: LineNumber,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream) -> Result<Parser> {
        Ok(Parser {
            tokens,
            current_line: 0,
        })
    }

    pub fn parse(&'a mut self, filename: &PathBuf) -> Result<Module> {
        Module::parse(self, filename, 0)
    }

    pub fn peek(&'a self, pos: usize, symbol: Symbol) -> bool {
        match self.tokens.peek(pos) {
            None => false,
            Some(tok) => tok.symbol == symbol,
        }
    }

    pub fn get(&'a self, pos: usize) -> Option<Token> {
        self.tokens.peek(pos)
    }

    pub fn get_symbol(&'a self, pos: usize) -> Option<Symbol> {
        self.tokens.peek(pos).map(|t| t.symbol)
    }


}
