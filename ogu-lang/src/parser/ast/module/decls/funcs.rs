use crate::parser::ast::module::decls::{Declaration, DeclParseResult};
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::{Parser, consume_symbol, look_ahead_where, raise_parser_error, parse_opt_indent, parse_opt_dedent};
use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::Guard;
use anyhow::Result;

impl<'a> Declaration<'a> {
    pub fn parse_func_or_val(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let (opt_where, pos) = if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, mut pos) = Declaration::parse_where(parser, where_pos)?;
            if parser.peek(pos, Lexeme::Dedent) {
                pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
            }
            (Some(where_decl), pos)
        } else {
            (None, pos)
        };
        match eq {
            Equation::Value(name, expr) => {
                if let Some(where_decl) = opt_where {
                    let expr = Expression::LetExpr(where_decl, Box::new(expr));
                    Ok(Some((
                        Declaration::Value(name, expr),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Value(name, expr), pos)))
                }
            }
            Equation::Function(name, args, expr) => {
                if let Some(where_decl) = opt_where {
                    let expr = Expression::LetExpr(where_decl, Box::new(expr));
                    Ok(Some((
                        Declaration::Function(name, args, expr),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Function(name, args, expr), pos)))
                }
            }
            Equation::FunctionWithGuards(name, args, guards) => {
                if let Some(where_decl) = opt_where {
                    let expr = Expression::LetExpr(where_decl, Box::new(Guard::guards_to_cond(&guards)));
                    Ok(Some((
                        Declaration::Function(name, args, expr),
                        pos,
                    )))
                } else {
                    Ok(Some((
                        Declaration::Function(name, args,  Guard::guards_to_cond(&guards)),
                        pos,
                    )))
                }
            }
            _ => raise_parser_error("invalid declaration", parser, pos, true),
        }
    }

    fn parse_where(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<Equation<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let (indent, mut pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        if indent {
            while !parser.peek(pos, Lexeme::Dedent) {
                pos = parser.skip_nl(pos);
                let (eq, new_pos) = Equation::parse(parser, pos, true)?;
                eqs.push(eq);
                pos = parser.skip_nl(new_pos);
            }
        } else {
            let (eq, new_pos) = Equation::parse(parser, pos, true)?;
            eqs.push(eq);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eqs, pos))
    }
}