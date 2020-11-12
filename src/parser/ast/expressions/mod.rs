use crate::lexer::tokens::Symbol;
use crate::parser::Parser;
use anyhow::Result;

struct LeftAssocExpr<'a>(Symbol<'a>, Box<Expression>, Vec<Expression>);

#[derive(Debug, Clone)]
pub enum Expression {
    Error,
    PipeFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeFirstArgFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeBackFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeBackFirstArgFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
}

fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
    let LeftAssocExpr(sym, name, args) = la_expr;
    match sym {
        Symbol::PipeRight => Expression::PipeFuncCall { name, args },
        Symbol::PipeRightFirstArg => Expression::PipeFirstArgFuncCall { name, args },
        Symbol::PipeLeft => Expression::PipeBackFuncCall { name, args },
        Symbol::PipeLeftFirstArg => Expression::PipeBackFirstArgFuncCall { name, args },
        _ => Expression::Error,
    }
}

type ParseResult = Result<(Expression, usize)>;

macro_rules! parse_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &Parser, pos: usize) -> ParseResult {
            parse_left_assoc_expr(parser, pos, $op, $next_level, |name, (args, pos)| {
                let la_expr = LeftAssocExpr($op, Box::new(name), args);
                Ok((left_assoc_expr_to_expr(la_expr), pos))
            })
        }
    };
}

impl Expression {
    pub fn parse(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_pipe_func_call_expr(parser, pos)
    }

    parse_assoc!(
        parse_pipe_func_call_expr,
        Symbol::PipeRight,
        Expression::parse_pipe_first_arg_func_call_expr
    );

    parse_assoc!(
        parse_pipe_first_arg_func_call_expr,
        Symbol::PipeRightFirstArg,
        Expression::parse_backpipe_func_call_expr
    );

    parse_assoc!(
        parse_backpipe_func_call_expr,
        Symbol::PipeLeft,
        Expression::parse_backpipe_first_arg_func_call_expr
    );

    parse_assoc!(
        parse_backpipe_first_arg_func_call_expr,
        Symbol::PipeLeftFirstArg,
        Expression::parse_doto_func_call_expr
    );

    parse_assoc!(
        parse_doto_func_call_expr,
        Symbol::Doto,
        Expression::parse_backdoto_func_call_expr
    );

    parse_assoc!(
        parse_backdoto_func_call_expr,
        Symbol::DotoBack,
        Expression::parse_dollar_func_call_expr
    );

    pub fn parse_dollar_func_call_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }
}

fn parse_left_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    build: fn(Expression, (Vec<Expression>, usize)) -> ParseResult,
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
    next_level: fn(&Parser, usize) -> ParseResult,
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
