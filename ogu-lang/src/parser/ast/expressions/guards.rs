use std::ops::Deref;

use anyhow::{bail, Result};

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{
    consume_symbol, parse_opt_dedent, parse_opt_indent, parse_opt_where_or_dedent, Parser,
};
use crate::parser::ast::expressions::args::{Args, Arg};

#[derive(Debug, Clone)]
pub(crate) struct Guard<'a>(
    pub Option<Box<Expression<'a>>>, // otherwise
    pub Box<Expression<'a>>,
);

pub(crate) type GuardVec<'a> = Vec<Guard<'a>>;

impl<'a> Guard<'a> {
    pub fn guards_to_cond(guards: &[Guard<'a>]) -> Result<Expression<'a>> {
        let mut pairs: Vec<(Option<Expression<'a>>, Expression<'a>)> = vec![];
        for guard in guards.iter() {
            pairs.push((
                guard.0.as_ref().map(|e| e.deref().clone()),
                guard.1.deref().clone(),
            ));
        }
        Expression::if_from(&pairs)
    }

    pub fn guards_to_case(
        args: Args<'a>,
        guards: &[Guard<'a>],
    ) ->  Expression<'a> {
        let e_args: Vec<Expression> = match args {
            Args::Void => vec![],
            Args::Many(v) => v.iter().map(|a| a.into()).collect(),
        };
        if e_args.is_empty() {
            return Expression::Unit;
        }
        let expr = if e_args.len() == 1 {
            e_args[0].clone()
        } else {
            Expression::TupleExpr(e_args.clone())
        };
        let case_guards = guards
            .iter()
            .map(|g| (g.deref().0.as_ref().map(|e| e.deref().clone()), g.1.deref().clone()))
            .collect();
        Expression::CaseExpr(Box::new(expr), case_guards)
    }
}

pub(crate) fn parse_guards<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
) -> Result<(GuardVec<'a>, usize)> {
    let (in_indent, pos) = parse_opt_indent(parser, pos);
    let (guard, mut pos) = parse_guard(parser, pos)?;
    let mut guards = vec![guard];
    let mut inner_indent = false;
    if parser.peek(pos, Lexeme::Indent) && parser.peek(pos + 1, Lexeme::Guard) {
        pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        inner_indent = true;
    }
    while parser.peek(pos, Lexeme::Guard) {
        let (guard, new_pos) = parse_guard(parser, pos)?;
        guards.push(guard);
        pos = parser.skip_nl(new_pos);
    }
    if inner_indent {
        pos = parse_opt_dedent(parser, pos, inner_indent)?;
    }
    let pos = parse_opt_where_or_dedent(parser, pos, in_indent)?;
    Ok((guards, pos))
}

fn parse_guard<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(Guard<'a>, usize)> {
    let pos = consume_symbol(parser, pos, Lexeme::Guard)?;
    let (guard, pos) = if parser.peek(pos, Lexeme::Otherwise) {
        (None, pos + 1)
    } else {
        let (expr, new_pos) = Expression::parse(parser, pos)?;
        (Some(Box::new(expr)), new_pos)
    };
    let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
    let pos = parser.skip_nl(pos);
    let (guard_value, pos) = Expression::parse(parser, pos)?;
    let pos = parser.skip_nl(pos);
    Ok((Guard(guard, Box::new(guard_value)), pos))
}


impl<'a> From<&Arg<'a>> for Expression<'a> {
    fn from(a: &Arg<'a>) -> Self {
        match a {
            Arg::Expr(e) => *e.clone(),
            Arg::Simple(s) => Expression::Name(s),
            Arg::SimpleStr(s) => Expression::NameStr(s.to_string()),
            Arg::Tuple(t) => Expression::TupleExpr(t.iter().map(|i| i.into()).collect()),
        }
    }
}
