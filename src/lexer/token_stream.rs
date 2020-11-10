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

    pub fn peek(&'a self) -> Option<Token<'a>> {
        self.tokens.first().cloned()
    }

    pub fn next(&'a mut self) -> Option<Token<'a>> {
        let result = self.tokens.first().cloned();
        if result.is_some() {
            self.tokens.remove(0);
        }
        result
    }

    pub fn skip(&'a mut self, symbol: Symbol<'a>) -> Option<Token<'a>> {
        let head = &self.tokens[0..2];
        match head.first() {
            None => None,
            Some(token) =>
                if token.symbol == symbol {
                    head.get(1).cloned()
                } else {
                    None
                }
        }
    }

    pub fn consume(&'a mut self)  {
        if !self.tokens.is_empty() {
            self.tokens.remove(0);
        }
    }
}
