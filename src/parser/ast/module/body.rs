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

#[derive(Debug, Clone)]
pub enum AlgebraicElement {
    Type(String),
    Param(String),
}

#[derive(Debug, Clone)]
pub enum AlgebraicType {
    Simple(String),
    Complex(String, Vec<AlgebraicElement>),
    Record(String, Vec<(String, AlgebraicElement)>),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Value(String, Expression),
    ValueWithWhere(String, Expression, Vec<Equation>),
    Function(String, Vec<Arg>, Expression),
    FunctionWithWhere(String, Vec<Arg>, Expression, Vec<Equation>),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
    FunctionWithGuardsAndWhere(String, Vec<Arg>, Vec<Guard>, Vec<Equation>),
    TypeDecl(String, Option<Vec<String>>, Vec<AlgebraicType>),
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
            Some(Symbol::Type) => Declaration::parse_type_decl(parser, pos),
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
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        // allows a single inline decl
        let (eq, mut pos) = Equation::parse(parser, pos, true)?;
        let mut eqs = vec![eq];
        if indent {
            while !parser.peek(pos, Symbol::Dedent) {
                pos = parser.skip_nl(pos);
                let (eq, new_pos) = Equation::parse(parser, pos, true)?;
                eqs.push(eq);
                pos = parser.skip_nl(new_pos);
            }
        }
        let mut pos = parse_opt_dedent(parser, pos, indent)?;
        while parser.peek(pos, Symbol::Dedent) {
            pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        }
        Ok((eqs, pos))
    }
}

impl Declaration {
    fn parse_type_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Type)?;
        if parser.peek(pos, Symbol::Alias) {
            todo!()
        }

        let (type_id, pos) = consume_type_id(parser, pos)?;
        println!(
            "type id = {} @ {} pos => {:?}",
            type_id,
            pos,
            parser.get_symbol(pos),
        );
        let (type_args, pos) = if parser.peek(pos, Symbol::Assign) {
            (None, pos)
        } else {
            let (args, pos) = Declaration::parse_type_args(parser, pos)?;
            (Some(args), pos)
        };
        let pos = consume_symbol(parser, pos, Symbol::Assign)?;
        let (type_decl, pos) = Declaration::parse_algebraic_type(parser, pos)?;
        let mut algebraic_elements = vec![type_decl];
        let pos = parser.skip_nl(pos);
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Symbol::Guard) {
            let new_pos = consume_symbol(parser, pos, Symbol::Guard)?;
            let (type_decl, new_pos) = Declaration::parse_algebraic_type(parser, new_pos)?;
            algebraic_elements.push(type_decl);
            pos = new_pos
        }
        println!("=> {} {:?} {:?}", type_id, type_args, algebraic_elements);
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            Declaration::TypeDecl(type_id, type_args, algebraic_elements),
            pos,
        )))
    }

    fn parse_algebraic_type(parser: &Parser, pos: usize) -> Result<(AlgebraicType, usize)> {
        let (type_id, pos) = consume_type_id(parser, pos)?;
        if parser.peek(pos, Symbol::LeftCurly) {
            println!("record!");
            todo!()
        } else {
            let mut params = vec![];
            let mut pos = pos;
            while let Some((alg_elem, new_pos)) = consume_alg_type_param(parser, pos)? {
                params.push(alg_elem);
                pos = new_pos;
            }
            if params.is_empty() {
                Ok((AlgebraicType::Simple(type_id), pos))
            } else {
                Ok((AlgebraicType::Complex(type_id, params), pos))
            }
        }
    }

    fn parse_type_args(parser: &Parser, pos: usize) -> Result<(Vec<String>, usize)> {
        let (id, mut pos) = consume_id(parser, pos)?;
        let mut args = vec![id];
        while !parser.peek(pos, Symbol::Assign) {
            let (id, new_pos) = consume_id(parser, pos)?;
            pos = new_pos;
            args.push(id);
        }
        Ok((args, pos))
    }
}

fn consume_alg_type_param(
    parser: &Parser,
    pos: usize,
) -> Result<Option<(AlgebraicElement, usize)>> {
    match parser.get_symbol(pos) {
        Some(Symbol::TypeId(type_id)) => {
            Ok(Some((AlgebraicElement::Type(type_id.to_string()), pos + 1)))
        }
        Some(Symbol::Id(id)) => Ok(Some((AlgebraicElement::Param(id.to_string()), pos + 1))),
        Some(Symbol::NewLine) => Ok(None),
        Some(Symbol::Indent) => Ok(None),
        Some(Symbol::Dedent) => Ok(None),
        Some(Symbol::Guard) => Ok(None),
        sym => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingTypeIdentifier,
        )))
        .context(format!("Expecting a type or a param, found: {:?}", sym)),
    }
}

fn consume_type_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    match parser.get_symbol(pos) {
        Some(Symbol::TypeId(type_id)) => Ok((type_id.to_string(), pos + 1)),
        _ => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingTypeIdentifier,
        ))),
    }
}

fn consume_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    match parser.get_symbol(pos) {
        Some(Symbol::Id(id)) => Ok((id.to_string(), pos + 1)),
        _ => Err(Error::new(OguError::ParserError(
            ParseError::ExpectingIdentifier,
        ))),
    }
}
