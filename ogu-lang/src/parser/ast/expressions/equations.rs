use anyhow::Result;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::consume_ids_sep_by;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::{parse_guards, Guard, GuardVec};
use crate::parser::{
    consume_symbol, parse_opt_dedent, parse_opt_indent, parse_opt_where_or_dedent,
    raise_parser_error, Parser,
};

#[derive(Debug, Clone)]
pub(crate) enum Equation<'a> {
    EqVal(Expression<'a>, Expression<'a>),
    EqFunc(&'a str, Args<'a>, Expression<'a>),
    EqFuncWithGuards(&'a str, Args<'a>, GuardVec<'a>),
}

impl<'a> Equation<'a> {
    pub(crate) fn parse(
        parser: &'a Parser<'a>,
        pos: usize,
        inner: bool,
    ) -> Result<(Equation<'a>, usize)> {
        if let Some(Lexeme::Id(id)) = parser.get_token(pos) {
            Equation::parse_func_or_val(id, parser, pos + 1, inner)
        } else {
            let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(pos, Lexeme::Assign) {
                let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
                Equation::parse_lval_no_guards(expr, parser, pos)
            } else {
                Equation::parse_lval_guards(expr, parser, pos)
            }
        }
    }

    pub(crate) fn parse_back_arrow_or_assign_eq(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        Equation::parse_value_assign2(parser, pos, Lexeme::BackArrow, Lexeme::Assign)
    }

    fn parse_value_assign2(
        parser: &'a Parser<'a>,
        pos: usize,
        symbol1: Lexeme,
        symbol2: Lexeme,
    ) -> Result<(Equation<'a>, usize)> {
        if let Some(Lexeme::Id(id)) = parser.get_token(pos) {
            let pos = pos + 1;
            let pos = if parser.peek(pos, symbol1) {
                consume_symbol(parser, pos, symbol1)?
            } else {
                consume_symbol(parser, pos, symbol2)?
            };
            Equation::parse_val(id, parser, pos)
        } else if parser.peek(pos, Lexeme::LeftParen) {
            let pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
            let (ids, pos) = consume_ids_sep_by(parser, pos, Lexeme::Comma)?;
            let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
            let pos = if parser.peek(pos, symbol1) {
                consume_symbol(parser, pos, symbol1)?
            } else {
                consume_symbol(parser, pos, symbol2)?
            };
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((
                Equation::EqVal(
                    Expression::TupleExpr(ids.iter().map(|id| Expression::Name(id)).collect()),
                    expr,
                ),
                pos,
            ))
        } else {
            raise_parser_error("Expecting identifier", parser, pos, true)
        }
    }

    fn parse_func_or_val(
        name: &'a str,
        parser: &'a Parser<'a>,
        pos: usize,
        inner: bool,
    ) -> Result<(Equation<'a>, usize)> {
        if parser.peek(pos, Lexeme::Assign) {
            Equation::parse_val(name, parser, pos + 1)
        } else {
            Equation::parse_func(name, parser, pos, inner)
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
        Ok((Equation::EqVal(Expression::Name(name), expr), pos))
    }

    fn parse_func(
        name: &'a str,
        parser: &'a Parser<'a>,
        pos: usize,
        inner: bool,
    ) -> Result<(Equation<'a>, usize)> {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Lexeme::Assign) {
            let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
            Equation::parse_func_no_guards(name, args, parser, pos)
        } else {
            Equation::parse_func_guards(name, args, parser, pos, inner)
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
        let eq = Equation::EqVal(expr_left, expr_val);
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
        let eq = Equation::EqFunc(name, args, expr);
        Ok((eq, pos))
    }

    fn parse_func_guards(
        name: &'a str,
        args: Args<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
        inner: bool,
    ) -> Result<(Equation<'a>, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        if inner {
            let expr = Guard::guards_to_case(args.clone(), &guards);
            let eq = Equation::EqFunc(name, args, expr);
            Ok((eq, pos))
        } else {
            let eq = Equation::EqFuncWithGuards(name, args, guards);
            Ok((eq, pos))
        }
    }

    fn parse_lval_guards(
        left_expr: Expression<'a>,
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Equation<'a>, usize)> {
        let (guards, pos) = parse_guards(parser, pos)?;
        let expr = Guard::guards_to_cond(&guards)?;
        let eq = Equation::EqVal(left_expr, expr);
        Ok((eq, pos))
    }


}
