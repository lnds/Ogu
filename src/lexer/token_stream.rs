use crate::lexer::tokens::{Token, Symbol};

#[derive(Debug)]
pub struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>) -> TokenStream {
        TokenStream { tokens }
    }

    pub fn iter(&'a self) -> std::slice::Iter<'a, Token<'a>> {
        self.tokens.iter()
    }

    pub fn len(&'a self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&'a self) -> bool {
        self.len() == 0
    }

    pub fn peek(&'a self, pos: usize) -> Option<Token<'a>> {
        self.tokens.get(pos).cloned()
    }


}
