use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{
    consume_symbol, parse_opt_dedent, parse_opt_indent, parse_opt_where_or_dedent, Parser,
};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct Guard<'a>(
    pub Option<Box<Expression<'a>>>, // otherwise
    pub Box<Expression<'a>>,
);

pub(crate) type GuardVec<'a> = Vec<Guard<'a>>;

pub(crate) fn parse_guards<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(GuardVec<'a>, usize)> {
    let (in_indent, pos) = parse_opt_indent(parser, pos);
    let (guard, mut pos) = parse_guard(parser, pos)?;
    let mut guards = vec![guard];
    let mut inner_indent = false;
    if parser.peek(pos, Token::Indent) && parser.peek(pos + 1, Token::Guard) {
        pos = consume_symbol(parser, pos, Token::Indent)?;
        inner_indent = true;
    }
    while parser.peek(pos, Token::Guard) {
        let (guard, new_pos) = parse_guard(parser, pos)?;
        guards.push(guard);
        pos = parser.skip_nl(new_pos);
    }
    if inner_indent {
        pos = parse_opt_dedent(parser, pos, inner_indent)?;
    }
    // println!("GUARDS = {:#?}", guards);
    // println!("IN INDENT = {} => {:?}", in_indent, parser.get_symbol(pos));
    let pos = parse_opt_where_or_dedent(parser, pos, in_indent)?;
    Ok((guards, pos))
}

fn parse_guard<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(Guard<'a>   , usize)> {
    let pos = consume_symbol(parser, pos, Token::Guard)?;
    let (guard, pos) = if parser.peek(pos, Token::Otherwise) {
        (None, pos + 1)
    } else {
        let (expr, new_pos) = Expression::parse(parser, pos)?;
        (Some(Box::new(expr)), new_pos)
    };
    let pos = consume_symbol(parser, pos, Token::Assign)?;
    let pos = parser.skip_nl(pos);
    let (guard_value, pos) = Expression::parse(parser, pos)?;
    let pos = parser.skip_nl(pos);
    Ok((Guard(guard, Box::new(guard_value)), pos))
}
