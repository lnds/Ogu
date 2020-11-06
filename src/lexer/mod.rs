mod token_stream;

use crate::lexer::token_stream::{Int, IntList, TokenList, TokenStream};
use std::fs::File;
use std::io::{self, BufRead, Error, ErrorKind, Result as IOResult};
use std::path::PathBuf;

pub struct Lexer {
    path: PathBuf,
}

impl Lexer {
    pub fn new(path: &PathBuf) -> IOResult<Lexer> {
        if !path.exists() {
            return Err(Error::new(ErrorKind::NotFound, format!("{:?}", path)));
        }
        Ok(Lexer { path: path.clone() })
    }

    pub fn scan(&mut self) -> IOResult<TokenStream> {
        let file = File::open(&self.path)?;
        let reader = io::BufReader::new(file);
        let mut lines = reader
            .lines()
            .enumerate()
            .filter_map(|a| match a {
                (i, Ok(line)) => {
                    if String::is_empty(&line) {
                        None
                    } else {
                        Some((i, line))
                    }
                }
                _ => None,
            })
            .collect();
        self.scan_lines(&mut lines)
    }

    fn scan_lines(&mut self, mut lines: &mut Vec<(Int, String)>) -> IOResult<TokenStream> {
        let mut tokens = vec![];
        let mut indent_stack = vec![0];
        self.map_lines(&mut lines, &mut indent_stack, &mut tokens, 0);
        let stream = TokenStream::new(tokens)?;
        Ok(stream)
    }

    fn map_lines(
        &mut self,
        mut lines: &mut Vec<(Int, String)>,
        mut indent_stack: &mut IntList,
        tokens: &mut TokenList,
        paren_level: Int,
    ) {
        if let Some((line, text)) = lines.pop() {
            let (mut line_tokens, new_paren_level) = self.scan_line(text, line, &mut indent_stack, paren_level);
            tokens.append(&mut line_tokens);
            self.map_lines(&mut lines,  &mut indent_stack, tokens, new_paren_level)
        }
    }

    fn scan_line(&mut self, line: String, line_num: Int, _indent_stack: &mut Vec<Int>, paren_level: Int) -> (TokenList, Int) {
        (vec![], paren_level)
    }
}
