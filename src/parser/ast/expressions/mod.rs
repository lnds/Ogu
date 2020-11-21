#[macro_use]
pub mod expression;
pub mod args;
pub mod equations;
pub mod guards;

use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::expression::{Expression, ParseResult};
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

pub struct LeftAssocExpr<'a>(Symbol<'a>, Box<Expression>, Box<Expression>);

pub struct RightAssocExpr<'a>(Symbol<'a>, Box<Expression>, Box<Expression>);

// 1 + 2 + 3 => (1 + 2) + 3
pub fn parse_left_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    build: fn(Expression, Expression) -> Expression,
) -> Result<(Expression, usize)> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        consume_left_args(parser, pos, op, next_level, expr, build)
    }
}

pub fn consume_left_args(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    left_expr: Expression,
    build: fn(Expression, Expression) -> Expression,
) -> ParseResult {
    if !parser.peek(pos, op) {
        Ok((left_expr, pos))
    } else {
        let (expr, pos) = next_level(parser, parser.skip_nl(pos + 1))?;
        consume_left_args(parser, pos, op, next_level, build(left_expr, expr), build)
    }
}

// 1 ^ 2 ^ 3 => 1 ^ (2 ^ 3)
pub fn parse_right_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    build: fn(Expression, Expression) -> Expression,
) -> ParseResult {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        consume_right_args(parser, pos, op, next_level, expr, build)
    }
}

pub fn consume_right_args(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    base_expr: Expression,
    build: fn(Expression, Expression) -> Expression,
) -> ParseResult {
    if !parser.peek(pos, op) {
        Ok((base_expr, pos))
    } else {
        let (expr, pos) = parse_right_assoc_expr(parser, pos + 1, op, next_level, build)?;
        consume_right_args(parser, pos, op, next_level, build(base_expr, expr), build)
    }
}

pub fn consume_exprs_sep_by(
    parser: &Parser,
    pos: usize,
    symbol: Symbol,
) -> Result<(Vec<Expression>, usize)> {
    let mut result = vec![];
    let (expr, mut pos) = Expression::parse(parser, pos)?;
    result.push(expr);
    while parser.peek(pos, symbol) {
        let (expr, new_pos) = Expression::parse(parser, pos + 1)?;
        result.push(expr);
        pos = new_pos;
    }
    Ok((result, pos))
}

pub fn consume_args(parser: &Parser, pos: usize) -> Result<(Vec<Expression>, usize)> {
    let mut args = vec![];
    let mut pos = pos;
    while !is_func_call_end_symbol(parser.get_symbol(pos)) {
        let (expr, new_pos) = Expression::parse_expr(parser, pos)?;
        pos = new_pos;
        args.push(expr);
    }
    Ok((args, pos))
}

pub fn consume_ids_sep_by(
    parser: &Parser,
    pos: usize,
    symbol: Symbol,
) -> Result<(Vec<String>, usize)> {
    let mut result = vec![];
    let (id, mut pos) = consume_id(parser, pos)?;
    result.push(id);
    while parser.peek(pos, symbol) {
        let (id, new_pos) = consume_id(parser, pos + 1)?;
        result.push(id);
        pos = new_pos;
    }
    Ok((result, pos))
}

pub fn consume_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    if let Some(Symbol::Id(id)) = parser.get_symbol(pos) {
        Ok((id.to_string(), pos + 1))
    } else {
        Err(Error::new(OguError::ParserError(
            ParseError::ExpectingIdentifier,
        )))
        .context(format!(
            "expecting id, but found: {:?} at {} ({})",
            parser.get_symbol(pos),
            parser.pos_to_line(pos).unwrap_or(0),
            pos
        ))
    }
}

pub fn is_literal(symbol: Symbol) -> bool {
    matches!(symbol, Symbol::Integer(_)
        |Symbol::Float(_)
        |Symbol::String(_)
        |Symbol::LargeString(_)
        |Symbol::FormatString(_) 
        |Symbol::RegExp(_)
        |Symbol::Char(_)
        |Symbol::IsoDate(_))
}

pub fn is_basic_op(symbol: Symbol) -> bool {
    matches!(
        symbol,
        Symbol::Cons
            | Symbol::Plus
            | Symbol::PlusPlus
            | Symbol::Mult
            | Symbol::Div
            | Symbol::DivDiv
            | Symbol::Minus
            | Symbol::Mod
            | Symbol::Pow
            | Symbol::And
            | Symbol::Or
            | Symbol::Equal
            | Symbol::NotEqual
            | Symbol::Not
            | Symbol::LessThan
            | Symbol::LessThanOrEqual
            | Symbol::Greater
            | Symbol::GreaterOrEqual
    )
}

pub fn is_func_call_end_symbol(symbol: Option<Symbol>) -> bool {
    match symbol {
        None => true,
        Some(Symbol::Error) => true,
        Some(sym) => matches!(
            sym,
            Symbol::NewLine
                | Symbol::Arroba
                | Symbol::Indent
                | Symbol::Dedent
                | Symbol::Assign
                | Symbol::Dollar
                | Symbol::Comma
                | Symbol::Dot
                | Symbol::DotDot
                | Symbol::DotDotDot
                | Symbol::Doto
                | Symbol::DotoBack
                | Symbol::Cons
                | Symbol::FatArrow
                | Symbol::Let
                | Symbol::Loop
                | Symbol::Do
                | Symbol::Then
                | Symbol::Else
                | Symbol::Elif
                | Symbol::Return
                | Symbol::Recur
                | Symbol::Yield
                | Symbol::RightParen
                | Symbol::In
                | Symbol::RightBracket
                | Symbol::RightCurly
                | Symbol::Where
                | Symbol::While
                | Symbol::Until
                | Symbol::PipeLeft
                | Symbol::PipeLeftFirstArg
                | Symbol::PipeRight
                | Symbol::PipeRightFirstArg
                | Symbol::ComposeForward
                | Symbol::ComposeBackward
                | Symbol::Pow
                | Symbol::Plus
                | Symbol::PlusPlus
                | Symbol::Mult
                | Symbol::Minus
                | Symbol::Div
                | Symbol::DivDiv
                | Symbol::Mod
                | Symbol::And
                | Symbol::Or
                | Symbol::Not
                | Symbol::Equal
                | Symbol::NotEqual
                | Symbol::Guard
                | Symbol::GreaterOrEqual
                | Symbol::Greater
                | Symbol::LessThan
                | Symbol::LessThanOrEqual
                | Symbol::Type
                | Symbol::Trait
                | Symbol::Alias
                | Symbol::SemiColon
                | Symbol::Colon
                | Symbol::Derive
                | Symbol::Extends
                | Symbol::Reify
        ),
    }
}

pub fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
    let LeftAssocExpr(sym, left, right) = la_expr;
    match sym {
        Symbol::PipeRight => Expression::PipeFuncCall(left, right),
        Symbol::PipeRightFirstArg => Expression::PipeFirstArgFuncCall(left, right),
        Symbol::PipeLeft => Expression::PipeBackFuncCall(left, right),
        Symbol::PipeLeftFirstArg => Expression::PipeBackFirstArgFuncCall(left, right),
        Symbol::Doto => Expression::DotoCall(left, right),
        Symbol::DotoBack => Expression::DotoBackCall(left, right),
        Symbol::Or => Expression::OrExpr(left, right),
        Symbol::And => Expression::AndExpr(left, right),
        Symbol::LessThan => Expression::LtExpr(left, right),
        Symbol::LessThanOrEqual => Expression::LeExpr(left, right),
        Symbol::Greater => Expression::GtExpr(left, right),
        Symbol::GreaterOrEqual => Expression::GeExpr(left, right),
        Symbol::Equal => Expression::EqExpr(left, right),
        Symbol::NotEqual => Expression::NeExpr(left, right),
        Symbol::Plus => Expression::AddExpr(left, right),
        Symbol::PlusPlus => Expression::ConcatExpr(left, right),
        Symbol::Minus => Expression::SubExpr(left, right),
        Symbol::Mult => Expression::MulExpr(left, right),
        Symbol::Div => Expression::DivExpr(left, right),
        Symbol::DivDiv => Expression::IntDivExpr(left, right),
        Symbol::Mod => Expression::ModExpr(left, right),
        Symbol::ComposeForward => Expression::ComposeFwdExpr(left, right),
        Symbol::ComposeBackward => Expression::ComposeBckExpr(left, right),
        sym => {
            println!("TODO {:?}", sym);
            todo!()
        }
    }
}

pub fn right_assoc_expr_to_expr(ra_expr: RightAssocExpr) -> Expression {
    let RightAssocExpr(sym, left, right) = ra_expr;
    match sym {
        Symbol::Cons => Expression::ConsExpr(left, right),
        Symbol::Pow => Expression::PowExpr(left, right),
        _ => Expression::Error,
    }
}
