use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::Expression;
use crate::parser::ast::module::body::Arg::TupleArg;
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Arg {
    Void,
    SimpleArg(String),
    TupleArg(Vec<Arg>),
}

#[derive(Debug)]
pub enum Equation {
    Value {
        name: String,
        expr: Expression,
        wheres: Option<Where>,
    },
    Function {
        name: String,
        args: Vec<Arg>,
        expr: Expression,
        wheres: Option<Where>,
    },
}

pub type Where = Vec<Equation>;

#[derive(Debug)]
pub enum Declaration {
    FuncOrVal(Equation),
    // TypeDecl, // TODO
}

#[derive(Debug)]
pub struct Body {
    declarations: Vec<Declaration>,
}

impl Body {
    pub(crate) fn parse(parser: &Parser, pos: &usize) -> Result<Self> {
        let declarations = Body::parse_decls(parser, pos)?;
        Ok(Body { declarations })
    }

    fn parse_decls(parser: &Parser, pos: &usize) -> Result<Vec<Declaration>> {
        let mut result = vec![];
        let mut pos = *pos;
        while let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
            println!("found decl: {:?}", decl);
            result.push(decl);
            pos = new_pos;
        }
        Ok(result)
    }

    fn parse_decl(parser: &Parser, pos: usize) -> Result<Option<(Declaration, usize)>> {
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            _ => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingDeclaration,
            ))),
        }
    }
}

impl Declaration {
    fn parse_func_or_val(parser: &Parser, pos: usize) -> Result<Option<(Declaration, usize)>> {
        if let (Some(Symbol::Id(id)), pos) = (parser.get_symbol(pos), pos + 1) {
            let name = id.to_string();
            let wheres = None;
            if parser.peek(pos, Symbol::Eq) {
                let (expr, pos) = Expression::parse(parser, pos + 1)?;
                Ok(Some((
                    Declaration::FuncOrVal(Equation::Value { name, expr, wheres }),
                    pos,
                )))
            } else {
                let (args, pos) = Arg::parse(parser, pos)?;
                if parser.peek(pos, Symbol::Assign) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    Ok(Some((
                        Declaration::FuncOrVal(Equation::Function {
                            name,
                            args,
                            expr,
                            wheres,
                        }),
                        pos,
                    )))
                } else {
                    Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingAssignation,
                    )))
                    .context(format!("expecting = at pos {}", pos))
                }
            }
        } else {
            Ok(None)
        }
    }
}

impl Arg {
    fn parse(parser: &Parser, pos: usize) -> Result<(Vec<Arg>, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((arg, new_pos)) = Arg::parse_arg(parser, pos)? {
            println!("found arg: {:?} @{}", arg, new_pos);
            result.push(arg);
            pos = new_pos;
        }
        pos = parser.skip_nl(pos);
        Ok((result, pos))
    }

    fn parse_arg(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        println!("parse arg {} {:?}", pos, parser.get_symbol(pos));
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::LeftParen) => Arg::parse_tuple(parser, pos),
            Some(Symbol::Id(id)) => Ok(Some((Arg::SimpleArg(id.to_string()), pos + 1))),
            Some(Symbol::Assign) => Ok(None),
            s => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingValidArg,
            )))
            .context(format!("{:?} not valid", s))
            .context("argument invalid"),
        }
    }

    fn parse_tuple(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        if parser.peek(pos + 1, Symbol::RightParen) {
            Ok(Some((Arg::Void, pos + 2)))
        } else {
            let mut args = vec![];
            let mut pos = pos + 1;
            match Arg::parse_arg(parser, pos + 1)? {
                Some((arg, new_pos)) => {
                    args.push(arg);
                    pos = new_pos;
                }
                None => {
                    return Err(Error::new(OguError::ParserError(ParseError::InvalidArg)))
                        .context("unexpected token");
                }
            }
            while !parser.peek(pos, Symbol::RightParen) {
                if !parser.peek(pos, Symbol::Comma) {
                    return Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingComma,
                    )))
                    .context("expecting comma");
                }
                match Arg::parse_arg(parser, pos + 1)? {
                    Some((new_arg, new_pos)) => {
                        pos = new_pos;
                        args.push(new_arg.clone())
                    }
                    None => {
                        return Err(Error::new(OguError::ParserError(ParseError::InvalidArg)))
                            .context("unexpected token");
                    }
                }
            }
            Ok(Some((TupleArg(args), pos + 1)))
        }
    }
}
