#[macro_use]
pub mod expression;
pub mod args;
pub mod equations;
pub mod guards;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::expression::{Expression, ParseResult};
use crate::parser::{raise_parser_error, Parser};
use anyhow::Result;

pub struct LeftAssocExpr<'a>(Lexeme<'a>, Box<Expression<'a>>, Box<Expression<'a>>);

pub struct RightAssocExpr<'a>(Lexeme<'a>, Box<Expression<'a>>, Box<Expression<'a>>);

// 1 + 2 + 3 => (1 + 2) + 3
pub(crate) fn parse_left_assoc_expr<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    op: Lexeme,
    next_level: fn(&'a Parser<'a>, usize) -> ParseResult<'a>,
    build: fn(Expression<'a>, Expression<'a>) -> Expression<'a>,
) -> Result<(Expression<'a>, usize)> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        consume_left_args(parser, pos, op, next_level, expr, build)
    }
}

pub(crate) fn consume_left_args<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    op: Lexeme,
    next_level: fn(&'a Parser<'a>, usize) -> ParseResult<'a>,
    left_expr: Expression<'a>,
    build: fn(Expression<'a>, Expression<'a>) -> Expression<'a>,
) -> ParseResult<'a> {
    if !parser.peek(pos, op) {
        Ok((left_expr, pos))
    } else {
        let (expr, pos) = next_level(parser, parser.skip_nl(pos + 1))?;
        consume_left_args(parser, pos, op, next_level, build(left_expr, expr), build)
    }
}

// 1 ^ 2 ^ 3 => 1 ^ (2 ^ 3)
pub(crate) fn parse_right_assoc_expr<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    op: Lexeme,
    next_level: fn(&'a Parser<'a>, usize) -> ParseResult<'a>,
    build: fn(Expression<'a>, Expression<'a>) -> Result<Expression<'a>>,
) -> ParseResult<'a> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        consume_right_args(parser, pos, op, next_level, expr, build)
    }
}

pub(crate) fn consume_right_args<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    op: Lexeme,
    next_level: fn(&'a Parser<'a>, usize) -> ParseResult<'a>,
    base_expr: Expression<'a>,
    build: fn(Expression<'a>, Expression<'a>) -> Result<Expression<'a>>,
) -> ParseResult<'a> {
    if !parser.peek(pos, op) {
        Ok((base_expr, pos))
    } else {
        let (expr, pos) = parse_right_assoc_expr(parser, pos + 1, op, next_level, build)?;
        consume_right_args(parser, pos, op, next_level, build(base_expr, expr)?, build)
    }
}

pub(crate) fn consume_exprs_sep_by<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    symbol: Lexeme,
) -> Result<(Vec<Expression<'a>>, usize)> {
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

pub(crate) fn consume_args<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
) -> Result<(Vec<Expression<'a>>, usize)> {
    let mut args = vec![];
    let mut pos = pos;
    while !is_func_call_end_symbol(parser.get_token(pos)) {
        let (expr, new_pos) = Expression::parse_control_expr(parser, pos)?;
        pos = new_pos;
        args.push(expr);
    }
    Ok((args, pos))
}

pub(crate) fn consume_ids_sep_by<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    symbol: Lexeme,
) -> Result<(Vec<&'a str>, usize)> {
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

pub(crate) fn consume_id<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(&'a str, usize)> {
    if let Some(Lexeme::Id(id)) = parser.get_token(pos) {
        Ok((id, pos + 1))
    } else {
        raise_parser_error("Expecting identifier", parser, pos, true)
    }
}

pub(crate) fn is_literal(symbol: Lexeme) -> bool {
    matches!(symbol, Lexeme::Integer(_)
        |Lexeme::Float(_)
        |Lexeme::String(_)
        |Lexeme::LargeString(_)
        |Lexeme::FormatString(_)
        |Lexeme::RegExp(_)
        |Lexeme::Char(_)
        |Lexeme::IsoDate(_))
}

pub(crate) fn is_basic_op(symbol: Lexeme) -> bool {
    matches!(
        symbol,
        Lexeme::Cons
            | Lexeme::Plus
            | Lexeme::PlusPlus
            | Lexeme::Mult
            | Lexeme::Div
            | Lexeme::DivDiv
            | Lexeme::Minus
            | Lexeme::Mod
            | Lexeme::Pow
            | Lexeme::And
            | Lexeme::Or
            | Lexeme::Equal
            | Lexeme::NotEqual
            | Lexeme::Not
            | Lexeme::LessThan
            | Lexeme::LessThanOrEqual
            | Lexeme::Greater
            | Lexeme::GreaterOrEqual
    )
}

pub(crate) fn is_func_call_end_symbol(symbol: Option<Lexeme>) -> bool {
    match symbol {
        None => true,
        Some(Lexeme::Error) => true,
        Some(sym) => matches!(
            sym,
            Lexeme::NewLine
                | Lexeme::Arroba
                | Lexeme::Arrow
                | Lexeme::BackArrow
                | Lexeme::Indent
                | Lexeme::Dedent
                | Lexeme::Assign
                | Lexeme::Dollar
                | Lexeme::DollarCurly
                | Lexeme::Comma
                | Lexeme::Cond
                | Lexeme::Case
                | Lexeme::Dot
                | Lexeme::DotDot
                | Lexeme::DotDotDot
                | Lexeme::Cons
                | Lexeme::FatArrow
                | Lexeme::Let
                | Lexeme::Loop
                | Lexeme::Do
                | Lexeme::Then
                | Lexeme::Else
                | Lexeme::Elif
                | Lexeme::Of
                | Lexeme::Return
                | Lexeme::Recur
                | Lexeme::Yield
                | Lexeme::RightParen
                | Lexeme::In
                | Lexeme::RightBracket
                | Lexeme::RightCurly
                | Lexeme::RightCurlyCurly
                | Lexeme::Where
                | Lexeme::With
                | Lexeme::While
                | Lexeme::Until
                | Lexeme::PipeLeft
                | Lexeme::Perform
                | Lexeme::PipeRight
                | Lexeme::ComposeForward
                | Lexeme::ComposeBackward
                | Lexeme::Pow
                | Lexeme::Plus
                | Lexeme::PlusPlus
                | Lexeme::Mult
                | Lexeme::Minus
                | Lexeme::Div
                | Lexeme::DivDiv
                | Lexeme::Mod
                | Lexeme::And
                | Lexeme::Or
                | Lexeme::Not
                | Lexeme::Equal
                | Lexeme::NotEqual
                | Lexeme::Guard
                | Lexeme::GreaterOrEqual
                | Lexeme::Greater
                | Lexeme::LessThan
                | Lexeme::LessThanOrEqual
                | Lexeme::Type
                | Lexeme::Trait
                | Lexeme::Try
                | Lexeme::Alias
                | Lexeme::SemiColon
                | Lexeme::Colon
                | Lexeme::Derive
                | Lexeme::Extends
                | Lexeme::Reify
                | Lexeme::Otherwise
                | Lexeme::Error
        ),
    }
}

pub(crate) fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
    let LeftAssocExpr(sym, left, right) = la_expr;
    match sym {
        Lexeme::PipeRight => Expression::FuncCallExpr(right, left),
        Lexeme::Or => Expression::OrExpr(left, right),
        Lexeme::And => Expression::AndExpr(left, right),
        Lexeme::LessThan => Expression::LtExpr(left, right),
        Lexeme::LessThanOrEqual => Expression::LeExpr(left, right),
        Lexeme::Greater => Expression::GtExpr(left, right),
        Lexeme::GreaterOrEqual => Expression::GeExpr(left, right),
        Lexeme::Equal => Expression::EqExpr(left, right),
        Lexeme::NotEqual => Expression::NeExpr(left, right),
        Lexeme::Plus => Expression::AddExpr(left, right),
        Lexeme::PlusPlus => Expression::ConcatExpr(left, right),
        Lexeme::Minus => Expression::SubExpr(left, right),
        Lexeme::Mult => Expression::MulExpr(left, right),
        Lexeme::Div => Expression::DivExpr(left, right),
        Lexeme::DivDiv => Expression::IntDivExpr(left, right),
        Lexeme::Mod => Expression::ModExpr(left, right),
        Lexeme::ComposeBackward => Expression::ComposeBckExpr(left, right),
        Lexeme::Matches => Expression::MatchesExpr(left, right),
        Lexeme::NotMatches => Expression::NoMatchesExpr(left, right),
        Lexeme::Match => Expression::ReMatchExpr(left, right),
        Lexeme::Arroba => Expression::IndexExpr(left, right),
        sym => {
            println!("TODO {:?}", sym);
            todo!()
        }
    }
}

pub(crate) fn right_assoc_expr_to_expr(ra_expr: RightAssocExpr) -> Result<Expression> {
    let RightAssocExpr(sym, left, right) = ra_expr;
    match sym {
        Lexeme::Cons => Ok(Expression::ConsExpr(left, right)),
        Lexeme::Pow => Ok(Expression::PowExpr(left, right)),
        Lexeme::Dollar => Ok(Expression::FuncCallExpr(left, right)),
        Lexeme::ComposeForward => Ok(Expression::ComposeFwdExpr(left, right)),
        Lexeme::PipeLeft => Ok(Expression::FuncCallExpr(left, right)),
        _ => todo!(),
    }
}
