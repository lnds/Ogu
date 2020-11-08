use crate::lexer::tokens::Token;

#[derive(Debug)]
pub struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>) -> TokenStream {
        TokenStream { tokens }
    }

    pub fn iter(&'a self) -> std::slice::Iter<'a, Token<'a>> {
        return self.tokens.iter();
    }

    pub fn len(&'a self) -> usize {
        self.tokens.len()
    }
}
