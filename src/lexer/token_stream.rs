use std::io::Result as IOResult;

enum Symbol {
    Oper,
}

pub type Int = usize;

pub type IntList = Vec<Int>;

pub struct Token {
    symbol: Symbol,
    line: Int,
}

pub type TokenList = Vec<Token>;

pub struct TokenStream {
    tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> IOResult<TokenStream> {
        Ok(TokenStream { tokens })
    }
}
