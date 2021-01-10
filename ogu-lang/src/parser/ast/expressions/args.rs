use anyhow::Result;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{consume_symbol, raise_parser_error, Parser};

#[derive(Debug, Clone)]
pub(crate) enum Arg<'a> {
    Simple(&'a str),
    SimpleStr(String),
    // Internal
    Tuple(Vec<Arg<'a>>),
    Expr(Box<Expression<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum Args<'a> {
    Void,
    Many(Vec<Arg<'a>>),
}

impl<'a> Arg<'a> {
    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> Result<(Args<'a>, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((arg, new_pos)) = Arg::parse_arg(parser, pos)? {
            if let Arg::Tuple(v) = &arg {
                if v.is_empty() {
                    return Ok((Args::Void, new_pos));
                }
            }
            result.push(arg);
            pos = new_pos;
        }
        pos = parser.skip_nl(pos);
        if result.is_empty() {
            Ok((Args::Void, pos))
        } else {
            Ok((Args::Many(result), pos))
        }
    }

    fn parse_arg(parser: &'a Parser<'a>, pos: usize) -> Result<Option<(Arg<'a>, usize)>> {
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Lexeme::LeftParen) => Arg::parse_tuple(parser, pos),
            Some(Lexeme::Assign) | Some(Lexeme::Guard) | Some(Lexeme::NewLine) => Ok(None),
            Some(Lexeme::Id(id)) => Ok(Some((Arg::Simple(id), pos + 1))),
            _ => {
                let (expr, pos) = Expression::parse_lambda_expr(parser, pos)?;
                if let Expression::Name(id) = expr {
                    Ok(Some((Arg::Simple(id), pos)))
                } else {
                    Ok(Some((Arg::Expr(Box::new(expr)), pos)))
                }
            }
        }
    }

    fn parse_tuple(parser: &'a Parser<'a>, pos: usize) -> Result<Option<(Arg<'a>, usize)>> {
        if parser.peek(pos + 1, Lexeme::RightParen) {
            return Ok(Some((Arg::Tuple(vec![]), pos + 2)));
        }
        let mut pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
        let mut args = vec![];
        match Arg::parse_arg(parser, pos)? {
            Some((arg, new_pos)) => {
                args.push(arg);
                pos = new_pos;
            }
            None => {
                return raise_parser_error("unexpected token parsing tuple", parser, pos, true);
            }
        }
        while !parser.peek(pos, Lexeme::RightParen) {
            if !parser.peek(pos, Lexeme::Comma) {
                return raise_parser_error("Expecting ','", parser, pos, true);
            }
            match Arg::parse_arg(parser, pos + 1)? {
                Some((new_arg, new_pos)) => {
                    pos = new_pos;
                    args.push(new_arg.clone())
                }
                None => {
                    return raise_parser_error("Unexpected token", parser, pos, true);
                }
            }
        }
        let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
        Ok(Some((Arg::Tuple(args), pos)))
    }
}
