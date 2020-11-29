use crate::lexer::tokens::TokenContext;

#[derive(Debug)]
pub(crate) struct TokenStream<'a> {
    tokens: Vec<TokenContext<'a>>,
}

impl<'a> TokenStream<'a> {
    pub(crate) fn new(tokens: Vec<TokenContext>) -> TokenStream {
        TokenStream { tokens }
    }

    pub(crate) fn iter(&'a self) -> std::slice::Iter<'a, TokenContext<'a>> {
        self.tokens.iter()
    }

    pub(crate) fn len(&'a self) -> usize {
        self.tokens.len()
    }

    pub(crate) fn is_empty(&'a self) -> bool {
        self.len() == 0
    }

    pub(crate) fn peek(&'a self, pos: usize) -> Option<TokenContext<'a>> {
        self.tokens.get(pos).cloned()
    }
}
