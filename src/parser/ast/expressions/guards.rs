use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::{consume_symbol, parse_opt_dedent, parse_opt_indent, Parser};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct Guard {
    pub guard: Option<Box<Expression>>,
    // otherwise
    pub value: Box<Expression>,
}

pub type GuardVec = Vec<Guard>;

pub fn parse_guards(parser: &Parser, pos: usize) -> Result<(GuardVec, usize)> {
    let mut guards = vec![];
    let (in_indent, mut pos) = parse_opt_indent(parser, pos);
    while parser.peek(pos, Symbol::Guard) {
        let (guard, new_pos) = if parser.peek(pos + 1, Symbol::Otherwise) {
            (None, pos + 2)
        } else {
            let (expr, new_pos) = Expression::parse(parser, pos + 1)?;
            (Some(Box::new(expr)), new_pos)
        };
        let new_pos = consume_symbol(parser, new_pos, Symbol::Assign)?;
        let new_pos = parser.skip_nl(new_pos);
        let (guard_value, new_pos) = Expression::parse(parser, new_pos)?;

        guards.push(Guard {
            guard,
            value: Box::new(guard_value),
        });
        pos = parser.skip_nl(new_pos);
    }
    let pos = parse_opt_dedent(parser, pos, in_indent)?;
    Ok((guards, pos))
}