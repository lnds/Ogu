use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::{ParseError, Parser};
use anyhow::{Error, Result};

#[derive(Debug, Clone)]
pub enum Expression {
    IndentedExpression(Box<Expression>),
    Identifier(String),
    FunctionCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
}

impl Expression {
    pub fn parse(parser: &Parser, pos: usize) -> Result<(Expression, usize)> {
        Expression::parse_pipe_func_call_expr(parser, pos)
    }

    pub fn parse_pipe_func_call_expr(parser: &Parser, pos: usize) -> Result<(Expression, usize)> {
        parse_left_assoc_expr(
            parser,
            pos,
            Symbol::PipeRight,
            Expression::parse_pipe_first_arg_func_call_expr,
            |name, (args, pos)| {
                Ok((
                    Expression::PipeFuncCall {
                        name: Box::new(name),
                        args: args,
                    },
                    pos,
                ))
            },
        )
    }

    pub fn parse_pipe_first_arg_func_call_expr(
        parser: &Parser,
        pos: usize,
    ) -> Result<(Expression, usize)> {
        todo!()
    }
}

fn parse_left_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> Result<(Expression, usize)>,
    build: fn(Expression, (Vec<Expression>, usize)) -> Result<(Expression, usize)>,
) -> Result<(Expression, usize)> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        build(expr, consume_args(parser, pos, op, next_level)?)
    }
}

fn consume_args(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> Result<(Expression, usize)>,
) -> Result<(Vec<Expression>, usize)> {
    let mut args = vec![];
    if !parser.peek(pos, op) {
        Ok((args, pos))
    } else {
        let pos = parser.skip_nl(pos + 1);
        let (expr, mut pos) = next_level(parser, pos)?;
        while parser.peek(pos, op) {
            pos = parser.skip_nl(pos + 1);
            args.push(expr.clone());
        }
        Ok((args, pos))
    }
}
