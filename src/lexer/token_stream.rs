use std::io::Result as IOResult;
use crate::lexer::tokens::Token;


#[derive(Debug)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

impl<'a> TokenStream {
    pub fn new(tokens: Vec<Token>) -> IOResult<TokenStream> {
        Ok(TokenStream { tokens })
    }
}
