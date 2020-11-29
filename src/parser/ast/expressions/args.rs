use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{consume_symbol, raise_parser_error, Parser};
use anyhow::Result;

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
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Token::LeftParen) => Arg::parse_tuple(parser, pos),
            Some(Token::Assign) => Ok(None),
            Some(Token::NewLine) => Ok(None),
            Some(Token::Guard) => Ok(None),
            _ => {
                let (expr, pos) = Expression::parse_lambda_expr(parser, pos)?;
                if let Expression::Identifier(id) = expr {
                    Ok(Some((Arg::SimpleArg(id), pos)))
                } else {
                    Ok(Some((Arg::ExprArg(Box::new(expr)), pos)))
                }
            }
        }
    }

    fn parse_tuple(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        if parser.peek(pos + 1, Token::RightParen) {
            return Ok(Some((Arg::Void, pos + 2)));
        }
        let mut pos = consume_symbol(parser, pos, Token::LeftParen)?;
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
        while !parser.peek(pos, Token::RightParen) {
            if !parser.peek(pos, Token::Comma) {
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
        Ok(Some((Arg::TupleArg(args), pos + 1)))
    }
}
