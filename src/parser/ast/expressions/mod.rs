use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::Expression::Identifier;
use crate::parser::{ParseError, Parser};
use anyhow::{Error, Result};

#[derive(Debug)]
pub enum Expression {
    IndentedExpression(Box<Expression>),
    Identifier(String),
    FunctionCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
}

impl Expression {
    pub fn parse(parser: &Parser, pos: usize) -> Result<(Expression, usize)> {
        match parser.get_symbol(pos) {
            Some(Symbol::Indent) => Expression::parse_indented_expr(parser, pos),
            Some(Symbol::Id(id)) => {
                Expression::parse_function_call(parser, id.to_string(), pos + 1)
            }
            _ => todo!(),
        }
    }

    fn parse_indented_expr(parser: &Parser, pos: usize) -> Result<(Expression, usize)> {
        println!("parse indented");
        let (expr, pos) = Expression::parse(parser, pos + 1)?; // skip indent
        if !parser.peek(pos, Symbol::Dedent) {
            Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIndentationEnd,
            )))
        } else {
            Ok(((Expression::IndentedExpression(Box::new(expr))), pos))
        }
    }

    fn parse_function_call(parser: &Parser, id: String, pos: usize) -> Result<(Expression, usize)> {
        todo!()
    }
}
