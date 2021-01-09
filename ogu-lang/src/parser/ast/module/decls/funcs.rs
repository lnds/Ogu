use anyhow::Result;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::decls::{DeclParseResult, DeclarationAst};
use crate::parser::{consume_symbol, look_ahead_where, parse_opt_dedent, parse_opt_indent, Parser};

impl<'a> DeclarationAst<'a> {
    pub fn parse_func_or_val(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let (opt_where, pos) = if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, mut pos) = DeclarationAst::parse_where(parser, where_pos)?;
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
                    Ok(Some((DeclarationAst::Value(name, expr), pos)))
                } else {
                    Ok(Some((DeclarationAst::Value(name, expr), pos)))
                }
            }
            Equation::Function(name, args, expr) => {
                if let Some(where_decl) = opt_where {
                    let expr = Expression::LetExpr(where_decl, Box::new(expr));
                    Ok(Some((DeclarationAst::Function(name, args, expr, None), pos)))
                } else {
                    Ok(Some((DeclarationAst::Function(name, args, expr, None), pos)))
                }
            }
            Equation::FunctionWithGuards(name, args, guards) => Ok(Some((
                DeclarationAst::FunctionWithGuards(name, args, guards, opt_where),
                pos,
            ))),
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
