use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::args::{Arg, VecArg};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::{parse_guards, Guard};
use crate::parser::{parse_opt_dedent, parse_opt_indent, ParseError, Parser};
use anyhow::{Error, Result};

#[derive(Debug, Clone)]
pub enum Equation {
    Value(String, Expression),
    Function(String, Vec<Arg>, Expression),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
}

impl Equation {
    pub fn parse(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        if let Some(Symbol::Id(id)) = parser.get_symbol(pos) {
            Equation::parse_func_or_val(id, parser, pos)
        } else {
            Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIdentifier,
            )))
        }
    }

    fn parse_func_or_val(id: &str, parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        let name = id.to_string();
        if parser.peek(pos, Symbol::Assign) {
            Equation::parse_val(name, parser, pos + 1)
        } else {
            Equation::parse_func(name, parser, pos)
        }
    }

    fn parse_val(name: String, parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        // we already parsed a =
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Equation::Value(name, expr), pos))
    }

    fn parse_func(name: String, parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Symbol::Assign) {
            Equation::parse_func_no_guards(name, args, parser, pos + 1)
        } else {
            Equation::parse_func_guards(name, args, parser, pos)
        }
    }

    fn parse_func_no_guards(
        name: String,
        args: VecArg,
        parser: &Parser,
        pos: usize,
    ) -> Result<(Equation, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let eq = Equation::Function(name, args, expr);
        Ok((eq, pos))
    }

    fn parse_func_guards(
        name: String,
        args: VecArg,
        parser: &Parser,
        pos: usize,
    ) -> Result<(Equation, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        let eq = Equation::FunctionWithGuards(name, args, guards);
        Ok((eq, pos))
    }
}
