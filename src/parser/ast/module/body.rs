use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::args::{Arg, VecArg};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::{parse_guards, Guard};
use crate::parser::{
    consume_symbol, look_ahead_where, parse_opt_dedent, parse_opt_indent, ParseError, Parser,
};
use anyhow::{Context, Error, Result};

#[derive(Debug)]
pub enum Declaration {
    Value(String, Expression),
    ValueWithWhere(String, Expression, Vec<Declaration>),
    Function(String, Vec<Arg>, Expression),
    FunctionWithWhere(String, Vec<Arg>, Expression, Vec<Declaration>),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
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
            Some(Symbol::Id(id)) => Declaration::parse_func_or_val(id, parser, pos + 1),
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
    pub fn parse_func_or_val(id: &str, parser: &Parser, pos: usize) -> DeclParseResult {
        let name = id.to_string();
        if parser.peek(pos, Symbol::Assign) {
            Declaration::parse_val(name, parser, pos + 1)
        } else {
            Declaration::parse_func(name, parser, pos)
        }
    }

    fn parse_val(name: String, parser: &Parser, pos: usize) -> DeclParseResult {
        // we already parsed a =
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, pos) = Declaration::parse_where(parser, where_pos)?;
            Ok(Some((
                Declaration::ValueWithWhere(name, expr, where_decl),
                pos,
            )))
        } else {
            Ok(Some((Declaration::Value(name, expr), pos)))
        }
    }

    fn parse_func(name: String, parser: &Parser, pos: usize) -> DeclParseResult {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Symbol::Assign) {
            Declaration::parse_func_no_guards(name, args, parser, pos + 1)
        } else {
            Declaration::parse_func_with_guards(name, args, parser, pos)
        }
    }

    fn parse_func_no_guards(
        name: String,
        args: VecArg,
        parser: &Parser,
        pos: usize,
    ) -> DeclParseResult {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, pos) = Declaration::parse_where(parser, where_pos)?;
            Ok(Some((
                Declaration::FunctionWithWhere(name, args, expr, where_decl),
                pos,
            )))
        } else {
            Ok(Some((Declaration::Function(name, args, expr), pos)))
        }
    }

    fn parse_func_with_guards(
        name: String,
        args: VecArg,
        parser: &Parser,
        pos: usize,
    ) -> DeclParseResult {
        let (guards, pos) = parse_guards(parser, pos)?;
        Ok(Some((
            Declaration::FunctionWithGuards(name, args, guards),
            pos,
        )))
    }

    fn parse_where(parser: &Parser, pos: usize) -> Result<(Vec<Declaration>, usize)> {
        if !parser.peek(pos, Symbol::Where) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingWhere,
            )))
            .context("expecting where");
        }
        let pos = parser.skip_nl(pos + 1);
        if parser.peek(pos, Symbol::Indent) {
            todo!()
        } else {
            // allows a single inline decl
            println!("expecting decl..");
            let (decl, pos) = Declaration::parse_where_func_or_val(parser, pos)?;
            println!(
                "parsed decl {:?} @ {} next_sym = {:?}",
                decl,
                pos,
                parser.get_symbol(pos)
            );

            let pos = parser.skip_nl(pos);
            let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
            Ok((vec![decl], pos))
        }
    }

    fn parse_where_func_or_val(parser: &Parser, pos: usize) -> Result<(Declaration, usize)> {
        match parser.get_symbol(pos) {
            None => Err(Error::new(OguError::ParserError(ParseError::EofUnexpected))),
            Some(Symbol::Id(id)) => match Declaration::parse_func_or_val(id, parser, pos + 1)? {
                None => Err(Error::new(OguError::ParserError(
                    ParseError::ExpectingDeclaration,
                )))
                .context(format!(
                    "Expecting declaration at {}, @{}",
                    parser.pos_to_line(pos).unwrap_or(0),
                    pos
                )),
                Some(result) => Ok(result),
            },
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
