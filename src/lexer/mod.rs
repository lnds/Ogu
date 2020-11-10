pub mod token_stream;
pub mod tokens;

use logos::Logos;

use crate::backend::OguError;
use anyhow::{Error, Result};
use std::fs::File;
use std::io::{self, BufRead, Cursor};
use std::path::PathBuf;

use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::Symbol::NewLine;
use crate::lexer::tokens::{
    IndentStack, LineNumber, LineSize, Symbol, SymbolList, Token, TokenList,
};

type Line = (LineSize, String);
type LineList = Vec<Line>;

enum LexerSource {
    File(PathBuf),
    Text(String),
}

pub struct Lexer {
    source: LexerSource,
    paren_level: LineSize,
    lines: LineList,
}

impl<'a> Lexer {
    pub fn new(path: &PathBuf) -> Result<Lexer> {
        if !path.exists() {
            return Err(Error::new(OguError::NotFound(format!("{:?}", path))));
        }
        Ok(Lexer {
            source: LexerSource::File(path.clone()),
            paren_level: 0,
            lines: vec![],
        })
    }

    pub fn from(str: &str) -> Self {
        Lexer {
            source: LexerSource::Text(str.to_string()),
            paren_level: 0,
            lines: vec![],
        }
    }

    pub fn scan(&'a mut self) -> Result<TokenStream<'a>> {
        self.lines = match &self.source {
            LexerSource::File(path) => {
                let file = File::open(&path)?;
                let reader = io::BufReader::new(file);
                reader
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
                    .collect()
            }
            LexerSource::Text(text) => {
                let reader = Cursor::new(text.as_bytes());
                reader
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
                    .collect()
            }
        };

        self.scan_lines()
    }

    fn scan_lines(&'a mut self) -> Result<TokenStream<'a>> {
        let tokens = self.map_lines();
        Ok(TokenStream::new(tokens))
    }

    fn map_lines(&'a mut self) -> TokenList<'a> {
        let mut tokens = vec![];
        let mut indent_stack = vec![0];
        for (line_number, text) in self.lines.iter() {
            let mut line_tokens = scan_line(
                &text[..],
                &line_number,
                &mut self.paren_level,
                &mut indent_stack,
            );
            tokens.append(&mut line_tokens);
        }
        tokens
    }
}

fn scan_line<'a>(
    text: &'a str,
    line_number: &LineNumber,
    paren_level: &mut LineSize,
    mut indent_stack: &mut IndentStack,
) -> TokenList<'a> {
    let line_len = text.len();
    let text = text.trim_start();
    let indentation = line_len - text.len();
    let mut line_symbols = vec![];
    if *paren_level == 0 {
        line_symbols = scan_indentation(indentation, &mut indent_stack);
    }
    let lexer = Symbol::lexer(text);
    for sym in lexer {
        if sym.is_open_paren() {
            *paren_level += 1;
        } else if sym.is_close_paren() {
            if *paren_level == 0 {
                line_symbols.push(Symbol::Error);
            } else {
                *paren_level -= 1;
            }
        }
        line_symbols.push(sym);
    }
    if *paren_level == 0 {
        line_symbols.push(NewLine)
    }
    line_symbols
        .iter()
        .map(|s| Token::new(s.clone(), *line_number + 1))
        .collect::<TokenList<'a>>()
}

fn scan_indentation<'a>(start_pos: LineSize, indent_stack: &mut IndentStack) -> SymbolList<'a> {
    match indent_stack.last() {
        None => vec![],
        Some(&pos) => {
            if start_pos > pos {
                indent_stack.push(start_pos);
                return vec![Symbol::Indent];
            }
            if start_pos < pos {
                let indent_length = indent_stack.len();
                while let Some(&p) = indent_stack.last() {
                    if start_pos < p && p > 0 {
                        indent_stack.pop();
                    } else {
                        break;
                    }
                }
                vec![Symbol::Dedent; indent_length - indent_stack.len()]
            } else {
                vec![]
            }
        }
    }
}

#[cfg(test)]
mod test_lexer {
    use crate::lexer::tokens::{Symbol, Token};
    use crate::lexer::Lexer;
    use std::path::PathBuf;
    use walkdir::WalkDir;

    #[test]
    fn test_scan_indentation() {
        let mut lexer = Lexer::from("func =\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let stream = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("func"),
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Assign,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Indent,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("this"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Is,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("a"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("function"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Dedent,
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("end"),
                line: 3
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_scan_paren() {
        let mut lexer = Lexer::from("func = (1, \n  2, 3, 4)\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let stream = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("func"),
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Assign,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::LeftParen,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("1"),
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("2"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("3"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("4"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::RightParen,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Indent,
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("this"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Is,
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("a"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("function"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Dedent,
                line: 4
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("end"),
                line: 4
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_scan_unbalanced_paren() {
        let mut lexer = Lexer::from("func = (1, \n  2, 3, 4))\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let stream = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("func"),
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Assign,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::LeftParen,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("1"),
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("2"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("3"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Comma,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Integer("4"),
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::RightParen,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Error,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::RightParen,
                line: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Indent,
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("this"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Is,
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("a"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("function"),
                line: 3
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Dedent,
                line: 4
            })
        );
        assert_eq!(
            iter.next(),
            Some(&Token {
                symbol: Symbol::Id("end"),
                line: 4
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn scan_test_files() {
        for entry in WalkDir::new("./tests/") {
            let entry = entry.unwrap();
            if entry.file_type().is_file() {
                let path = entry.path();
                let spath = entry.path().display().to_string();
                if spath.ends_with(".ogu") {
                    let lex = Lexer::new(&PathBuf::from(path));
                    assert!(lex.is_ok());
                    let mut lexer = lex.unwrap();
                    let stream = lexer.scan();
                    assert!(stream.is_ok());
                    let stream = stream.unwrap();
                    assert!(stream.len() > 0);
                }
            }
        }
    }

    #[test]
    fn scan_demos() {
        for entry in WalkDir::new("./demos/") {
            let entry = entry.unwrap();
            if entry.file_type().is_file() {
                let path = entry.path();
                let spath = entry.path().display().to_string();
                if spath.ends_with(".ogu") {
                    let lex = Lexer::new(&PathBuf::from(path));
                    assert!(lex.is_ok());
                    let mut lexer = lex.unwrap();
                    let stream = lexer.scan();
                    assert!(stream.is_ok());
                    let stream = stream.unwrap();
                    assert!(stream.len() > 0);
                }
            }
        }
    }
}
