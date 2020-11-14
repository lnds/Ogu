use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::lexer::tokens::Symbol::Dedent;
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
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<Self> {
        let declarations = Body::parse_decls(parser, pos)?;
        Ok(Body { declarations })
    }

    fn parse_decls(parser: &Parser, pos: usize) -> Result<Vec<Declaration>> {
        println!("parse decls @ {}", pos);
        let mut result = vec![];
        let mut pos = pos;
        while let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
            result.push(decl);
            pos = new_pos;
        }
        Ok(result)
    }

    fn parse_decl(parser: &Parser, pos: usize) -> Result<Option<(Declaration, usize)>> {
        println!("parse decl @ {}", pos);
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
    fn parse_func_or_val(
        id: &str,
        parser: &Parser,
        pos: usize,
    ) -> Result<Option<(Declaration, usize)>> {
        println!("parse_func_or_val @ {}", pos);
        // here
        let name = id.to_string();
        if parser.peek(pos, Symbol::Assign) {
            Declaration::parse_val(name, parser, pos + 1)
        } else {
            Declaration::parse_func(name, parser, pos)
        }
    }

    fn parse_val(
        name: String,
        parser: &Parser,
        pos: usize,
    ) -> Result<Option<(Declaration, usize)>> {
        // we already parsed a =
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok(Some((
            Declaration::FuncOrVal(Equation::Value {
                name,
                expr,
                wheres: None,
            }),
            pos,
        )))
    }

    fn parse_func(
        name: String,
        parser: &Parser,
        pos: usize,
    ) -> Result<Option<(Declaration, usize)>> {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Symbol::Assign) {
            Declaration::parse_func_without_guards(name, args, parser, pos + 1)
        } else {
            todo!()
        }
    }

    fn parse_func_without_guards(
        name: String,
        args: Vec<Arg>,
        parser: &Parser,
        pos: usize,
    ) -> Result<Option<(Declaration, usize)>> {
        let mut pos = parser.skip_nl(pos);
        let mut in_indent = false;
        if parser.peek(pos, Symbol::Indent) {
            in_indent = true;
            pos = pos + 1;
        }
        let (expr, pos) = Expression::parse(parser, pos)?;
        let mut pos = parser.skip_nl(pos);
        if in_indent {
            if !parser.peek(pos, Symbol::Dedent) {
                return Err(Error::new(OguError::ParserError(
                    ParseError::ExpectingIndentationEnd,
                )))
                .context("esperando fin de indentación");
            }
            pos = pos + 1;
        }
        Ok(Some((
            Declaration::FuncOrVal(Equation::Function {
                name,
                args,
                expr,
                wheres: None,
            }),
            pos,
        )))
    }
}

impl Arg {
    fn parse(parser: &Parser, pos: usize) -> Result<(Vec<Arg>, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((arg, new_pos)) = Arg::parse_arg(parser, pos)? {
            result.push(arg);
            pos = new_pos;
        }
        pos = parser.skip_nl(pos);
        Ok((result, pos))
    }

    fn parse_arg(parser: &Parser, pos: usize) -> Result<Option<(Arg, usize)>> {
        println!("parse arg @ {}", pos);
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
            return Ok(Some((Arg::Void, pos + 2)));
        }
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
