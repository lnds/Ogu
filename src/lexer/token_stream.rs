use std::io::Result as IOResult;
use crate::lexer::tokens::Token;


#[derive(Debug)]
pub struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>) -> IOResult<TokenStream> {
        Ok(TokenStream { tokens })
    }
}
