use crate::backend::OguError;
use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::args::{Arg, VecArg};
use crate::parser::ast::expressions::consume_ids_sep_by;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::{parse_guards, Guard};
use crate::parser::{
    consume_symbol, parse_opt_dedent, parse_opt_indent, parse_opt_where_or_dedent, ParseError,
    Parser,
};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Equation {
    Value(String, Expression),
    TupleValue(Vec<String>, Expression),
    LValue(Expression, Expression),
    LValueWithGuards(Expression, Vec<Guard>),
    Function(String, Vec<Arg>, Expression),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
}

impl Equation {
    pub fn parse(parser: &Parser, pos: usize, inner: bool) -> Result<(Equation, usize)> {
        if let Some(Token::Id(id)) = parser.get_token(pos) {
            Equation::parse_func_or_val(id, parser, pos + 1)
        } else if inner {
            let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(pos, Token::Assign) {
                Equation::parse_lval_no_guards(expr, parser, pos + 1)
            } else {
                Equation::parse_lval_guards(expr, parser, pos)
            }
        } else {
            Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIdentifier,
            )))
            .context(format!(
                "Expecting id, but found: {:?} at {:?}",
                parser.get_token(pos),
                parser.pos_to_line_col(pos)
            ))
        }
    }

    pub fn parse_back_arrow_or_assign_eq(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        Equation::parse_value_assign2(parser, pos, Token::BackArrow, Token::Assign)
    }
    pub fn parse_back_arrow_eq(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        Equation::parse_value_assign(parser, pos, Token::BackArrow)
    }

    pub fn parse_value(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        Equation::parse_value_assign(parser, pos, Token::Assign)
    }

    fn parse_value_assign(parser: &Parser, pos: usize, symbol: Token) -> Result<(Equation, usize)> {
        if let Some(Token::Id(id)) = parser.get_token(pos) {
            let pos = consume_symbol(parser, pos + 1, symbol)?;
            Equation::parse_val(id.to_string(), parser, pos)
        } else {
            let (l_expr, pos) = Expression::parse_primary_expr(parser, pos)?;
            let pos = consume_symbol(parser, pos, symbol)?;
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((Equation::LValue(l_expr, expr), pos))
        }
    }

    fn parse_value_assign2(
        parser: &Parser,
        pos: usize,
        symbol1: Token,
        symbol2: Token,
    ) -> Result<(Equation, usize)> {
        if let Some(Token::Id(id)) = parser.get_token(pos) {
            let pos = pos + 1;
            let pos = if parser.peek(pos, symbol1) {
                consume_symbol(parser, pos, symbol1)?
            } else {
                consume_symbol(parser, pos, symbol2)?
            };
            Equation::parse_val(id.to_string(), parser, pos)
        } else if parser.peek(pos, Token::LeftParen) {
            let pos = consume_symbol(parser, pos, Token::LeftParen)?;
            let (ids, pos) = consume_ids_sep_by(parser, pos, Token::Comma)?;
            let pos = consume_symbol(parser, pos, Token::RightParen)?;
            let pos = if parser.peek(pos, symbol1) {
                consume_symbol(parser, pos, symbol1)?
            } else {
                consume_symbol(parser, pos, symbol2)?
            };
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((Equation::TupleValue(ids, expr), pos))
        } else {
            Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIdentifier,
            )))
            .context(format!(
                "Expecting identifier but found: {:?} @{:?}",
                parser.get_token(pos),
                parser.pos_to_line_col(pos)
            ))
        }
    }

    fn parse_func_or_val(id: &str, parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        let name = id.to_string();
        if parser.peek(pos, Token::Assign) {
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
        if parser.peek(pos, Token::Assign) {
            Equation::parse_func_no_guards(name, args, parser, pos + 1)
        } else {
            Equation::parse_func_guards(name, args, parser, pos)
        }
    }

    fn parse_lval_no_guards(
        expr_left: Expression,
        parser: &Parser,
        pos: usize,
    ) -> Result<(Equation, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr_val, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let eq = Equation::LValue(expr_left, expr_val);
        Ok((eq, pos))
    }

    fn parse_func_no_guards(
        name: String,
        args: VecArg,
        parser: &Parser,
        pos: usize,
    ) -> Result<(Equation, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_where_or_dedent(parser, pos, indent)?;
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

    fn parse_lval_guards(
        left_expr: Expression,
        parser: &Parser,
        pos: usize,
    ) -> Result<(Equation, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        let eq = Equation::LValueWithGuards(left_expr, guards);
        Ok((eq, pos))
    }
}
