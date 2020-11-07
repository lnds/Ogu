mod tokens;
mod token_stream;

use std::fs::File;
use std::io::{self, BufRead, Error, ErrorKind, Result as IOResult};
use std::path::PathBuf;

use crate::lexer::tokens::{IntList, Int, TokenList};
use crate::lexer::token_stream::TokenStream;

type Line = (Int, String);
type LineList = Vec<Line>;

pub struct Lexer {
    path: PathBuf,
    current_string: String,
    parse_multi_line_string: bool,
    indent_stack: IntList,
    lines: LineList,
    paren_level: Int,
    tokens: TokenList,
}

impl<'a> Lexer {
    pub fn new(path: &PathBuf) -> IOResult<Lexer> {
        if !path.exists() {
            return Err(Error::new(ErrorKind::NotFound, format!("{:?}", path)));
        }
        Ok(Lexer {
            path: path.clone(),
            current_string: String::new(),
            parse_multi_line_string: false,
            indent_stack: vec![],
            lines: vec![],
            paren_level: 0,
            tokens: vec![],
        })
    }

    pub fn scan(&mut self) -> IOResult<TokenStream> {
        let file = File::open(&self.path)?;
        let reader = io::BufReader::new(file);
        self.lines = reader
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
        self.scan_lines()
    }

    fn scan_lines(&mut self) -> IOResult<TokenStream> {
        self.tokens = vec![];
        self.indent_stack = vec![0];
        self.map_lines();
        Ok(TokenStream::new(self.tokens.to_vec())?)
    }

    fn map_lines(&mut self) {
        if let Some((line_number, text)) = self.lines.pop() {
            let mut line_tokens = self.scan_line(&text[..], &line_number);
            self.tokens.append(&mut line_tokens);
            self.map_lines()
        }
    }

    fn scan_line(&mut self, text: &str, _line_number: &Int) -> TokenList {
        let p = find_comment_pos(text);
        let (text, comment) = text.split_at(p);
        let tokens = text.split_whitespace();
        vec![]
    }
}


/// a comment start with --
fn find_comment_pos(text: &str) -> usize {
    let mut inside_string = false;
    let mut iter = text.chars().enumerate();
    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => { iter.next(); }
            '\"' => { inside_string = !inside_string; }
            '-' => if let Some((_, q)) = iter.next() {
                if q == '-' && !inside_string {
                    return i;
                }
            }
            _ => continue
        }
    }
    return 0;
}

#[cfg(test)]
mod test_lexer {
    use crate::lexer::find_comment_pos;

    #[test]
    fn test_find_coment_post() {
        assert_eq!(find_comment_pos(""), 0);
        assert_eq!(find_comment_pos("--"), 0);
        assert_eq!(find_comment_pos("-- comentario"), 0);
        assert_eq!(find_comment_pos("a = a + 1 -- incrementa ah, duh!"), 10);
        assert_eq!(find_comment_pos("s = \" -- incrementa ah, duh! -- \" "), 0);
        assert_eq!(find_comment_pos("str = \" -- incrementa ah, duh! -- \" -- comentario"), 36);
        assert_eq!(find_comment_pos("str =  -- \" -- incrementa ah, duh! -- \" -- comentario"), 7);
        assert_eq!(find_comment_pos("str =  \\\" -- "), 10);
        assert_eq!(find_comment_pos("str =  \\\"\" -- \"--"), 15);
    }

    #[test]
    fn test_scan_line() {}
}

