use crate::lexer::tokens::TokenContext;

#[derive(Debug)]
pub struct TokenStream<'a> {
    tokens: Vec<TokenContext<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<TokenContext>) -> TokenStream {
        TokenStream { tokens }
    }

    pub fn iter(&'a self) -> std::slice::Iter<'a, TokenContext<'a>> {
        self.tokens.iter()
    }

    pub fn len(&'a self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&'a self) -> bool {
        self.len() == 0
    }

    pub fn peek(&'a self, pos: usize) -> Option<TokenContext<'a>> {
        self.tokens.get(pos).cloned()
    }
}
