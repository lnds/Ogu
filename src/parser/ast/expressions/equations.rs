use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::consume_ids_sep_by;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::{parse_guards, Guard};
use crate::parser::{
    consume_symbol, parse_opt_dedent, parse_opt_indent, parse_opt_where_or_dedent,
    raise_parser_error, Parser,
};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) enum Equation<'a> {
    Value(&'a str, Expression<'a>),
    TupleValue(Vec<&'a str>, Expression<'a>),
    LValue(Expression<'a>, Expression<'a>),
    LValueWithGuards(Expression<'a>, Vec<Guard<'a>>),
    Function(&'a str, Args<'a>, Expression<'a>),
    FunctionWithGuards(&'a str, Args<'a>, Vec<Guard<'a>>),
}

impl<'a> Equation<'a> {
    pub(crate) fn parse(
        parser: &'a Parser<'a>,
        pos: usize,
        inner: bool,
    ) -> Result<(Equation<'a>, usize)> {
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
            raise_parser_error("Expecting id", parser, pos, true)
        }
    }

    pub(crate) fn parse_back_arrow_or_assign_eq(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        Equation::parse_value_assign2(parser, pos, Token::BackArrow, Token::Assign)
    }

    pub(crate) fn parse_back_arrow_eq(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        Equation::parse_value_assign(parser, pos, Token::BackArrow)
    }

    pub(crate) fn parse_value(parser: &'a Parser<'a>, pos: usize) -> Result<(Equation<'a>, usize)> {
        Equation::parse_value_assign(parser, pos, Token::Assign)
    }

    fn parse_value_assign(
        parser: &'a Parser<'a>,
        pos: usize,
        symbol: Token,
    ) -> Result<(Equation<'a>, usize)> {
        if let Some(Token::Id(id)) = parser.get_token(pos) {
            let pos = consume_symbol(parser, pos + 1, symbol)?;
            Equation::parse_val(id, parser, pos)
        } else {
            let (l_expr, pos) = Expression::parse_primary_expr(parser, pos)?;
            let pos = consume_symbol(parser, pos, symbol)?;
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((Equation::LValue(l_expr, expr), pos))
        }
    }

    fn parse_value_assign2(
        parser: &'a Parser<'a>,
        pos: usize,
        symbol1: Token,
        symbol2: Token,
    ) -> Result<(Equation<'a>, usize)> {
        if let Some(Token::Id(id)) = parser.get_token(pos) {
            let pos = pos + 1;
            let pos = if parser.peek(pos, symbol1) {
                consume_symbol(parser, pos, symbol1)?
            } else {
                consume_symbol(parser, pos, symbol2)?
            };
            Equation::parse_val(id, parser, pos)
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
            raise_parser_error("Expecting identifier", parser, pos, true)
        }
    }

    fn parse_func_or_val(
        name: &'a str,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        if parser.peek(pos, Token::Assign) {
            Equation::parse_val(name, parser, pos + 1)
        } else {
            Equation::parse_func(name, parser, pos)
        }
    }

    fn parse_val(
        name: &'a str,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        // we already parsed a =
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Equation::Value(name, expr), pos))
    }

    fn parse_func(
        name: &'a str,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Token::Assign) {
            Equation::parse_func_no_guards(name, args, parser, pos + 1)
        } else {
            Equation::parse_func_guards(name, args, parser, pos)
        }
    }

    fn parse_lval_no_guards(
        expr_left: Expression<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr_val, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let eq = Equation::LValue(expr_left, expr_val);
        Ok((eq, pos))
    }

    fn parse_func_no_guards(
        name: &'a str,
        args: Args<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_where_or_dedent(parser, pos, indent)?;
        let eq = Equation::Function(name, args, expr);
        Ok((eq, pos))
    }

    fn parse_func_guards(
        name: &'a str,
        args: Args<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        let eq = Equation::FunctionWithGuards(name, args, guards);
        Ok((eq, pos))
    }

    fn parse_lval_guards(
        left_expr: Expression<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        let eq = Equation::LValueWithGuards(left_expr, guards);
        Ok((eq, pos))
    }
}
