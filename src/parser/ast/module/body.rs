use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::Expression;
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

#[derive(Debug, Clone)]
pub enum Arg {
    Void,
    SimpleArg(String),
    TupleArg(Vec<Arg>),
}

type VecArg = Vec<Arg>;

#[derive(Debug, Clone)]
pub struct Guard {
    pub guard: Option<Box<Expression>>, // otherwise
    pub value: Box<Expression>,
}

pub type Where = Vec<Equation>;

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
    FunctionWithGuards {
        name: String,
        args: Vec<Arg>,
        guards: Vec<Guard>,
        wheres: Option<Where>,
    },
}

#[derive(Debug)]
pub enum Declaration {
    FuncOrVal(Equation),
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
        Ok(Some((
            Declaration::FuncOrVal(Equation::Value {
                name,
                expr,
                wheres: None,
            }),
            pos,
        )))
    }

    fn parse_func(name: String, parser: &Parser, pos: usize) -> DeclParseResult {
        let (args, pos) = Arg::parse(parser, pos)?;
        if parser.peek(pos, Symbol::Assign) {
            Declaration::parse_no_guards(name, args, parser, pos + 1)
        } else {
            Declaration::parse_guards(name, args, parser, pos)
        }
    }

    fn parse_no_guards(name: String, args: VecArg, parser: &Parser, pos: usize) -> DeclParseResult {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let eq = Equation::Function {
            name,
            args,
            expr,
            wheres: None,
        };
        Ok(Some((Declaration::FuncOrVal(eq), pos)))
    }

    /*
       fn args
          | cond =  expr
          | cond = expr
          | otherwise = expr

       fn args
       | cond =  expr
       | cond = expr
       | otherwise = expr
    */
    fn parse_guards(name: String, args: VecArg, parser: &Parser, pos: usize) -> DeclParseResult {
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        let mut guards = vec![];
        while parser.peek(pos, Symbol::Guard) {
            let (guard, new_pos) = if parser.peek(pos + 1, Symbol::Otherwise) {
                (None, pos + 2)
            } else {
                let (expr, new_pos) = Expression::parse(parser, pos + 1)?;
                (Some(Box::new(expr)), new_pos)
            };
            if !parser.peek(new_pos, Symbol::Assign) {
                return Err(Error::new(OguError::ParserError(
                    ParseError::ExpectingAssignation,
                )))
                .context("expecting guard assignation");
            }
            let new_pos = parser.skip_nl(new_pos + 1);
            let (guard_value, new_pos) = Expression::parse(parser, new_pos)?;
            guards.push(Guard {
                guard,
                value: Box::new(guard_value),
            });
            pos = parser.skip_nl(new_pos);
        }
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let eq = Equation::FunctionWithGuards {
            name,
            args,
            guards,
            wheres: None,
        };
        Ok(Some((Declaration::FuncOrVal(eq), pos)))
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
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::LeftParen) => Arg::parse_tuple(parser, pos),
            Some(Symbol::Id(id)) => Ok(Some((Arg::SimpleArg(id.to_string()), pos + 1))),
            Some(Symbol::Assign) => Ok(None),
            Some(Symbol::NewLine) => Ok(None),
            s => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingValidArg,
            )))
            .context(format!("{:?} not valid", s)),
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
        Ok(Some((Arg::TupleArg(args), pos + 1)))
    }
}

pub fn parse_opt_indent(parser: &Parser, pos: usize) -> (bool, usize) {
    let pos = parser.skip_nl(pos);
    if parser.peek(pos, Symbol::Indent) {
        (true, pos + 1)
    } else {
        (false, pos)
    }
}

pub fn parse_opt_dedent(parser: &Parser, pos: usize, in_indent: bool) -> Result<usize> {
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
    Ok(pos)
}
