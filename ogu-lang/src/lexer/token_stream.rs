use crate::lexer::tokens::Token;

#[derive(Debug, Clone)]
pub(crate) struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub(crate) fn new(tokens: Vec<Token>) -> TokenStream {
        TokenStream { tokens }
    }

    pub(crate) fn iter(&'a self) -> std::slice::Iter<'a, Token<'a>> {
        self.tokens.iter()
    }

    #[allow(dead_code)]
    pub(crate) fn len(&'a self) -> usize {
        self.tokens.len()
    }

    pub(crate) fn peek(&'a self, pos: usize) -> Option<Token<'a>> {
        self.tokens.get(pos).cloned()
    }
}
