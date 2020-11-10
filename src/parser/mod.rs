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
        Module::parse(self, filename)
    }

    pub fn peek(&'a self, symbol: Symbol) -> bool {
        match self.tokens.peek() {
            None => false,
            Some(tok) => tok.symbol == symbol,
        }
    }

    pub fn current_line(&'a mut self) -> LineNumber {
        self.current_line
    }

    pub fn next_symbol(&'a mut self) -> Option<Symbol> {
        self.tokens.next().map(|t| t.symbol)
    }

    pub fn next_token(&'a mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn consume(&'a mut self, symbol: Symbol) -> Result<()> {
        match self.tokens.next() {
            Some(Token{symbol:s, line:_}) =>
                if s != symbol {
                    Err(Error::new(OguError::ParserError(ExpectingSymbol(symbol.to_string()))))
                } else {
                    Ok(())
                }
            _ => Err(Error::new(OguError::ParserError(ExpectingSymbol(symbol.to_string()))))
        }
    }

    pub fn skip(&'a mut self, symbol: Symbol<'a>) -> Option<Token> {
        self.tokens.skip(symbol)
    }
}
