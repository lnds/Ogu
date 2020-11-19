use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{consume_symbol, parse_opt_dedent, parse_opt_indent, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Guard(
    pub Option<Box<Expression>>, // otherwise
    pub Box<Expression>,
);

pub type GuardVec = Vec<Guard>;

pub fn parse_guards(parser: &Parser, pos: usize) -> Result<(GuardVec, usize)> {
    let (in_indent, pos) = parse_opt_indent(parser, pos);
    let (guard, mut pos) = parse_guard(parser, pos)?;
    let mut guards = vec![guard];

    while parser.peek(pos, Symbol::Guard) {
        let (guard, new_pos) = parse_guard(parser, pos)?;
        guards.push(guard);
        pos = parser.skip_nl(new_pos);
    }
    let pos = parse_opt_dedent(parser, pos, in_indent)?;
    Ok((guards, pos))
}

fn parse_guard(parser: &Parser, pos: usize) -> Result<(Guard, usize)> {
    let pos = consume_symbol(parser, pos, Symbol::Guard)?;
    let (guard, pos) = if parser.peek(pos, Symbol::Otherwise) {
        (None, pos + 1)
    } else {
        let (expr, new_pos) = Expression::parse(parser, pos)?;
        (Some(Box::new(expr)), new_pos)
    };
    let pos = consume_symbol(parser, pos, Symbol::Assign)?;
    let pos = parser.skip_nl(pos);
    let (guard_value, pos) = Expression::parse(parser, pos)?;
    let pos = parser.skip_nl(pos);
    Ok((Guard(guard, Box::new(guard_value)), pos))
}
