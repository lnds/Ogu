use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::args::Arg;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::Guard;
use crate::parser::{
    consume_id, consume_symbol, consume_type_id, look_ahead_where, parse_opt_dedent,
    parse_opt_indent, ParseError, Parser,
};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum FuncType {
    Void,
    Type(String),
    Param(String),
    Chain(Box<FuncType>, Box<FuncType>),
}

#[derive(Debug, Clone)]
pub struct FuncPrototype(String, FuncType);

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
    TraitDecl(String, Vec<FuncPrototype>),
    ExtensionDecl(String, String, Vec<Declaration>),
    FunctionPrototype(FuncPrototype),
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
            Some(Symbol::Id(_)) if parser.peek(pos + 1, Symbol::Colon) => {
                let (proto, pos) = Declaration::parse_func_prototype(parser, pos)?;
                Ok(Some((Declaration::FunctionPrototype(proto), pos)))
            }
            Some(Symbol::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            Some(Symbol::Type) => Declaration::parse_type_decl(parser, pos),
            Some(Symbol::Trait) => Declaration::parse_trait_decl(parser, pos),
            Some(Symbol::Extends) => Declaration::parse_extends(parser, pos),
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
            let (where_decl, mut pos) = Declaration::parse_where(parser, where_pos)?;
            if parser.peek(pos, Symbol::Dedent) {
                pos = consume_symbol(parser, pos, Symbol::Dedent)?;
            }
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
        let (indent, mut pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        if indent {
            while !parser.peek(pos, Symbol::Dedent) {
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

impl Declaration {
    fn parse_type_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Type)?;
        if parser.peek(pos, Symbol::Alias) {
            todo!()
        }

        let (type_id, pos) = consume_type_id(parser, pos)?;
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
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            Declaration::TypeDecl(type_id, type_args, algebraic_elements),
            pos,
        )))
    }

    fn parse_algebraic_type(parser: &Parser, pos: usize) -> Result<(AlgebraicType, usize)> {
        let (type_id, pos) = consume_type_id(parser, pos)?;
        if parser.peek(pos, Symbol::LeftCurly) {
            Declaration::parse_record_type(parser, pos, type_id)
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

    fn parse_record_type(
        parser: &Parser,
        pos: usize,
        type_id: String,
    ) -> Result<(AlgebraicType, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::LeftCurly)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (member, mut pos) = Declaration::parse_record_member(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut members = vec![member];
        while parser.peek(pos, Symbol::Comma) {
            pos = consume_symbol(parser, pos, Symbol::Comma)?;
            let (member, new_pos) = Declaration::parse_record_member(parser, pos)?;
            members.push(member);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Symbol::RightCurly)?;
        Ok((AlgebraicType::Record(type_id, members), pos))
    }

    fn parse_record_member(
        parser: &Parser,
        pos: usize,
    ) -> Result<((String, AlgebraicElement), usize)> {
        let (id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::Colon)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        Ok(((id, AlgebraicElement::Type(tid)), pos))
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

impl Declaration {
    fn parse_trait_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Trait)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Symbol::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut trait_decls = vec![];
        while !parser.peek(pos, Symbol::Dedent) {
            let (func_proto, new_pos) = Declaration::parse_func_prototype(parser, pos)?;
            trait_decls.push(func_proto);
            pos = parser.skip_nl(new_pos);
        }
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;

        Ok(Some((Declaration::TraitDecl(tid, trait_decls), pos)))
    }

    fn parse_func_prototype(parser: &Parser, pos: usize) -> Result<(FuncPrototype, usize)> {
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::Colon)?;
        let (types, pos) = Declaration::parse_func_types(parser, pos)?;
        Ok((FuncPrototype(func_id, types), pos))
    }

    fn parse_func_types(parser: &Parser, pos: usize) -> Result<(FuncType, usize)> {
        let (t, pos) = Declaration::parse_func_type(parser, pos)?;
        if parser.peek(pos, Symbol::Arrow) {
            let pos = consume_symbol(parser, pos, Symbol::Arrow)?;
            let (t2, pos) = Declaration::parse_func_types(parser, pos)?;
            Ok((FuncType::Chain(Box::new(t), Box::new(t2)), pos))
        } else {
            Ok((t, pos))
        }
    }

    fn parse_func_type(parser: &Parser, pos: usize) -> Result<(FuncType, usize)> {
        match parser.get_symbol(pos) {
            Some(Symbol::TypeId(tid)) => Ok((FuncType::Type(tid.to_string()), pos + 1)),
            Some(Symbol::Id(id)) => Ok((FuncType::Param(id.to_string()), pos + 1)),
            Some(Symbol::LeftParen) if parser.peek(pos + 1, Symbol::RightParen) => {
                Ok((FuncType::Void, pos + 2))
            }
            Some(Symbol::LeftParen) => {
                let pos = consume_symbol(parser, pos, Symbol::LeftParen)?;
                let (t, pos) = Declaration::parse_func_types(parser, pos)?;
                let pos = consume_symbol(parser, pos, Symbol::RightParen)?;
                Ok((t, pos))
            }
            sym => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingTypeIdentifier,
            )))
            .context(format!("Expecting a type or a param, found: {:?}", sym)),
        }
    }
}

impl Declaration {
    fn parse_extends(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Extends)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::With)?;
        let (trait_id, pos) = consume_type_id(parser, pos)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Symbol::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut decls = vec![];
        while let Some((decl, new_pos)) = Declaration::parse_func_or_val(parser, pos)? {
            decls.push(decl);
            pos = new_pos;
            if parser.peek(pos, Symbol::Dedent) {
                break;
            }
        }
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            Declaration::ExtensionDecl(type_id, trait_id, decls),
            pos,
        )))
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
