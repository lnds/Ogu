pub mod token_stream;
pub mod tokens;

use logos::Logos;

use crate::backend::OguError;
use anyhow::{Error, Result};
use std::fs::File;
use std::io::{self, BufRead, Cursor};
use std::path::PathBuf;

use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::Token::NewLine;
use crate::lexer::tokens::{
    IndentStack, LineCount, LineNumber, LineWidth, Token, TokenContext, TokenContextList,
};

type Line = (LineCount, String);
type LineList = Vec<Line>;

#[derive(Clone)]
enum LexerSource {
    File(PathBuf),
    #[allow(dead_code)]
    Text(String),
}

#[derive(Clone)]
pub(crate) struct Lexer {
    source: LexerSource,
    paren_level: LineCount,
    lines: LineList,
}

impl<'a> Lexer {



    pub(crate) fn new(path: &'a PathBuf) -> Result<Lexer> {
        if !path.exists() {
            return Err(Error::new(OguError::NotFound(format!("{:?}", path))));
        }
        Ok(Lexer {
            source: LexerSource::File(path.clone()),
            paren_level: 0,
            lines: vec![],
        })
    }

    pub(crate) fn scan(&'a mut self) -> Result<(TokenStream<'a>, Vec<String>)> {
        self.lines = match &self.source {
            LexerSource::File(path) => {
                let file = File::open(&path)?;
                let mut reader = io::BufReader::new(file);
                read_lines(&mut reader)
            }
            LexerSource::Text(text) => {
                let mut reader = Cursor::new(text.as_bytes());
                read_lines(&mut reader)
            }
        };

        self.scan_lines()
    }

    fn scan_lines(&'a mut self) -> Result<(TokenStream<'a>, Vec<String>)> {
        let mut large_strings = vec![];
        let tokens = self.map_lines(&mut large_strings);
        Ok((TokenStream::new(tokens), large_strings))
    }

    fn map_lines(&'a mut self, mut large_strings: &mut Vec<String>) -> TokenContextList<'a> {
        let mut tokens = vec![];
        let mut indent_stack = vec![0];
        let mut large_string = String::new();
        for (line_number, text) in self.lines.iter() {
            let mut line_tokens = scan_line(
                &text[..],
                &line_number,
                &mut self.paren_level,
                &mut large_string,
                &mut indent_stack,
                &mut large_strings,
            );
            tokens.append(&mut line_tokens);
        }
        if !indent_stack.is_empty() {
            let line_num = self.lines.len() + 1;
            while let Some(&p) = indent_stack.last() {
                if p == 0usize {
                    break;
                }
                indent_stack.pop();
                tokens.push(TokenContext {
                    token: Token::Dedent,
                    line: line_num,
                    col: 0,
                });
            }
        }
        tokens
    }
}

fn read_lines(reader: &mut dyn BufRead) -> LineList {
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

fn scan_line<'a>(
    text: &'a str,
    line_number: &LineNumber,
    paren_level: &mut LineCount,
    large_string: &mut String,
    mut indent_stack: &mut IndentStack,
    large_strings: &mut Vec<String>,
) -> TokenContextList<'a> {
    let line_len = text.len();
    let text = text.trim_start();

    let mut in_string = !large_string.is_empty();

    let indentation = line_len - text.len();

    let mut line_tokens = vec![];
    if *paren_level == 0 && !in_string {
        line_tokens = scan_indentation(indentation, &mut indent_stack);
    }

    let (text, rest) = match text.find("\"\"\"") {
        Some(pos) => text.split_at(pos),
        None => (text, ""),
    };
    let lexer = if in_string {
        large_string.push_str(text);
        large_string.push('\n');
        if rest.contains("\"\"\"") {
            large_strings.push(large_string.clone());
            line_tokens.push((Token::LargeString(large_strings.len() - 1), 0));
            large_string.clear();
            Token::lexer(&rest[3..])
        } else {
            Token::lexer(rest)
        }
    } else {
        if !rest.is_empty() {
            large_string.clone_from(&rest[3..].to_string());
            if large_string.is_empty() {
                large_string.push('\n');
            }
            in_string = true;
        }
        Token::lexer(text)
    };

    for (tok, span) in lexer.spanned() {
        if tok.is_open_paren() {
            *paren_level += 1;
        } else if tok.is_close_paren() {
            if *paren_level == 0 {
                line_tokens.push((Token::Error, 0));
            } else {
                *paren_level -= 1;
            }
        }
        line_tokens.push((tok, span.start + 1));
    }
    if *paren_level == 0 && !in_string {
        line_tokens.push((NewLine, text.len()))
    }
    line_tokens
        .iter()
        .map(|(s, col)| TokenContext::new(*s, *line_number + 1, *col))
        .collect::<TokenContextList<'a>>()
}

fn scan_indentation<'a>(
    start_pos: LineCount,
    indent_stack: &mut IndentStack,
) -> Vec<(Token<'a>, LineWidth)> {
    match indent_stack.last() {
        None => vec![],
        Some(&pos) => {
            if start_pos > pos {
                indent_stack.push(start_pos);
                return vec![(Token::Indent, start_pos)];
            }
            if start_pos < pos {
                let indent_length = indent_stack.len();
                while let Some(&p) = indent_stack.last() {
                    if start_pos >= p || p == 0 {
                        break;
                    }
                    indent_stack.pop();
                }
                vec![(Token::Dedent, pos); indent_length - indent_stack.len()]
            } else {
                vec![]
            }
        }
    }
}

#[cfg(test)]
mod test_lexer {
    use crate::lexer::tokens::{Token, TokenContext};
    use crate::lexer::{Lexer, LexerSource};
    use std::path::PathBuf;
    use walkdir::{DirEntry, WalkDir};

    impl Lexer {
        pub(crate) fn from(str: &str) -> Self {
            Lexer {
                source: LexerSource::Text(str.to_string()),
                paren_level: 0,
                lines: vec![],
            }
        }
    }

    #[test]
    fn test_scan_indentation() {
        let mut lexer = Lexer::from("func =\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let (stream, _) = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("func"),
                line: 1,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Assign,
                line: 1,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 1,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Indent,
                line: 2,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("this"),
                line: 2,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Is,
                line: 2,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("a"),
                line: 2,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("function"),
                line: 2,
                col: 11
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 2,
                col: 18
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Dedent,
                line: 3,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("end"),
                line: 3,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 3,
                col: 3
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_scan_paren() {
        let mut lexer = Lexer::from("func = (1, \n  2, 3, 4)\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let (stream, _) = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("func"),
                line: 1,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Assign,
                line: 1,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::LeftParen,
                line: 1,
                col: 8
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("1"),
                line: 1,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 1,
                col: 10
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("2"),
                line: 2,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 2,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("3"),
                line: 2,
                col: 4
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 2,
                col: 5
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("4"),
                line: 2,
                col: 7
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::RightParen,
                line: 2,
                col: 8
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 2,
                col: 8
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Indent,
                line: 3,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("this"),
                line: 3,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Is,
                line: 3,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("a"),
                line: 3,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("function"),
                line: 3,
                col: 11
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 3,
                col: 18
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Dedent,
                line: 4,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("end"),
                line: 4,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 4,
                col: 3
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_scan_unbalanced_paren() {
        let mut lexer = Lexer::from("func = (1, \n  2, 3, 4))\n  this is a function\nend");
        let stream_result = lexer.scan();
        assert!(stream_result.is_ok());
        let (stream, _) = stream_result.unwrap();
        let mut iter = stream.iter();
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("func"),
                line: 1,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Assign,
                line: 1,
                col: 6,
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::LeftParen,
                line: 1,
                col: 8,
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("1"),
                line: 1,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 1,
                col: 10
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("2"),
                line: 2,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 2,
                col: 2,
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("3"),
                line: 2,
                col: 4
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Comma,
                line: 2,
                col: 5
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Integer("4"),
                line: 2,
                col: 7
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::RightParen,
                line: 2,
                col: 8
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Error,
                line: 2,
                col: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::RightParen,
                line: 2,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 2,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Indent,
                line: 3,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("this"),
                line: 3,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Is,
                line: 3,
                col: 6
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("a"),
                line: 3,
                col: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("function"),
                line: 3,
                col: 11
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 3,
                col: 18
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Dedent,
                line: 4,
                col: 2
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::Id("end"),
                line: 4,
                col: 1
            })
        );
        assert_eq!(
            iter.next(),
            Some(&TokenContext {
                token: Token::NewLine,
                line: 4,
                col: 3
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn scan_test_files() {
        for entry in WalkDir::new("./tests/") {
            let entry = entry.unwrap();
            scan_test_file(&entry);
        }
    }

    fn scan_test_file(entry: &DirEntry) {
        if entry.file_type().is_file() {
            let path = entry.path();
            let spath = entry.path().display().to_string();
            if spath.ends_with(".ogu") {
                let lex = Lexer::new(&PathBuf::from(path));
                assert!(lex.is_ok());
                let mut lexer = lex.unwrap();
                let stream = lexer.scan();
                assert!(stream.is_ok());
                let (stream, _) = stream.unwrap();
                assert!(stream.len() > 0);
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
                    let (stream, _) = stream.unwrap();
                    assert!(stream.len() > 0);
                }
            }
        }
    }
}
