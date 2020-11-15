use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Arg {
    Void,
    SimpleArg(String),
    TupleArg(Vec<Arg>),
    ExprArg(Box<Expression>),
}

pub type VecArg = Vec<Arg>;

impl Arg {
    pub fn parse(parser: &Parser, pos: usize) -> Result<(VecArg, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((arg, new_pos)) = Arg::parse_arg(parser, pos)? {
            result.push(arg);
            pos = new_pos;
        }
        pos = parser.skip_nl(pos);
        Ok((result, pos))
    }

    fn parse_arg(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::LeftParen) => Arg::parse_tuple(parser, pos),
            Some(Symbol::Id(id)) => Ok(Some((Arg::SimpleArg(id.to_string()), pos + 1))),
            Some(Symbol::Assign) => Ok(None),
            Some(Symbol::NewLine) => Ok(None),
            _ => {
                let (expr, pos) = Expression::parse_lambda_expr(parser, pos)?;
                Ok(Some((Arg::ExprArg(Box::new(expr)), pos)))
            }
        }
    }

    fn parse_tuple(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        if parser.peek(pos + 1, Symbol::RightParen) {
            return Ok(Some((Arg::Void, pos + 2)));
        }
        let mut args = vec![];
        let mut pos = pos + 1;
        match Arg::parse_arg(parser, pos + 1)? {
            Some((arg, new_pos)) => {
                args.push(arg);
                pos = new_pos;
            }
            None => {
                return Err(Error::new(OguError::ParserError(ParseError::InvalidArg)))
                    .context("unexpected token");
            }
        }
        while !parser.peek(pos, Symbol::RightParen) {
            if !parser.peek(pos, Symbol::Comma) {
                return Err(Error::new(OguError::ParserError(
                    ParseError::ExpectingComma,
                )))
                .context("expecting comma");
            }
            match Arg::parse_arg(parser, pos + 1)? {
                Some((new_arg, new_pos)) => {
                    pos = new_pos;
                    args.push(new_arg.clone())
                }
                None => {
                    return Err(Error::new(OguError::ParserError(ParseError::InvalidArg)))
                        .context("unexpected token");
                }
            }
        }
        Ok(Some((Arg::TupleArg(args), pos + 1)))
    }
}
