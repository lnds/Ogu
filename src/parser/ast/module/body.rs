use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::args::Arg;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::Guard;
use crate::parser::{
    consume_symbol, look_ahead_where, parse_opt_dedent, parse_opt_indent, ParseError, Parser,
};
use anyhow::{Context, Error, Result};

#[derive(Debug)]
pub enum Declaration {
    Value(String, Expression),
    ValueWithWhere(String, Expression, Vec<Equation>),
    Function(String, Vec<Arg>, Expression),
    FunctionWithWhere(String, Vec<Arg>, Expression, Vec<Equation>),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
    FunctionWithGuardsAndWhere(String, Vec<Arg>, Vec<Guard>, Vec<Equation>),
    // TypeDecl, // TODO
}

type DeclVec = Vec<Declaration>;

#[derive(Debug)]
pub struct Body {
    declarations: Vec<Declaration>,
}

type DeclParseResult = Result<Option<(Declaration, usize)>>;

impl Body {
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<Self> {
        let declarations = Body::parse_decls(parser, pos)?;
        Ok(Body { declarations })
    }

    fn parse_decls(parser: &Parser, pos: usize) -> Result<DeclVec> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
            result.push(decl);
            pos = new_pos;
        }
        Ok(result)
    }

    fn parse_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = parser.skip_nl(pos);
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            sym => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingDeclaration,
            )))
            .context(format!(
                "Expecting declaration at {}, @{}, found: {:?}",
                parser.pos_to_line(pos).unwrap_or(0),
                pos,
                sym
            )),
        }
    }
}

impl Declaration {
    pub fn parse_func_or_val(parser: &Parser, pos: usize) -> DeclParseResult {
        let (eq, pos) = Equation::parse(parser, pos, false)?;
        let (opt_where, pos) = if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, pos) = Declaration::parse_where(parser, where_pos)?;
            (Some(where_decl), pos)
        } else {
            (None, pos)
        };
        match eq {
            Equation::Value(name, expr) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::ValueWithWhere(name, expr, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Value(name, expr), pos)))
                }
            }
            Equation::Function(name, args, expr) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::FunctionWithWhere(name, args, expr, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Function(name, args, expr), pos)))
                }
            }
            Equation::FunctionWithGuards(name, args, guards) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::FunctionWithGuardsAndWhere(name, args, guards, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((
                        Declaration::FunctionWithGuards(name, args, guards),
                        pos,
                    )))
                }
            }
            _ => Err(Error::new(OguError::ParserError(
                ParseError::InvalidDeclaration,
            )))
            .context(format!(
                "Invalid declaration @{}",
                parser.pos_to_line(pos).unwrap_or(0)
            )),
        }
    }

    fn parse_where(parser: &Parser, pos: usize) -> Result<(Vec<Equation>, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::Where)?;
        let pos = parser.skip_nl(pos + 1);
        let (indent, pos) = parse_opt_indent(parser, pos);
        // allows a single inline decl
        let (eq, mut pos) = Equation::parse(parser, pos, false)?;
        let mut eqs = vec![eq];
        if indent {
            while !parser.peek(pos, Symbol::Dedent) {
                pos = parser.skip_nl(pos);
                let (eq, new_pos) = Equation::parse(parser, pos, false)?;
                eqs.push(eq);
                pos = parser.skip_nl(new_pos);
            }
        }
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eqs, pos))
    }
}
