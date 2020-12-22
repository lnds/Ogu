#[macro_use]
pub mod expression;
pub mod args;
pub mod equations;
pub mod guards;

use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::expression::{Expression, ParseResult};
use crate::parser::{raise_parser_error, Parser};
use anyhow::Result;

pub struct LeftAssocExpr<'a>(Token<'a>, Box<Expression<'a>>, Box<Expression<'a>>);

pub struct RightAssocExpr<'a>(Token<'a>, Box<Expression<'a>>, Box<Expression<'a>>);

// 1 + 2 + 3 => (1 + 2) + 3
pub(crate) fn parse_left_assoc_expr<'a>(
    parser: &'a Parser<'a>,
    pos: usize,
    op: Token,
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
    op: Token,
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
    op: Token,
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
    op: Token,
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
    symbol: Token,
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

pub(crate) fn consume_args<'a>(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<Expression<'a>>, usize)> {
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
    parser: &'a  Parser<'a>,
    pos: usize,
    symbol: Token,
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
    if let Some(Token::Id(id)) = parser.get_token(pos) {
        Ok((id, pos + 1))
    } else {
        raise_parser_error("Expecting identifier", parser, pos, true)
    }
}

pub(crate) fn is_literal(symbol: Token) -> bool {
    matches!(symbol, Token::Integer(_)
        |Token::Float(_)
        |Token::String(_)
        |Token::LargeString(_)
        |Token::FormatString(_) 
        |Token::RegExp(_)
        |Token::Char(_)
        |Token::IsoDate(_))
}

pub(crate) fn is_basic_op(symbol: Token) -> bool {
    matches!(
        symbol,
        Token::Cons
            | Token::Plus
            | Token::PlusPlus
            | Token::Mult
            | Token::Div
            | Token::DivDiv
            | Token::Minus
            | Token::Mod
            | Token::Pow
            | Token::And
            | Token::Or
            | Token::Equal
            | Token::NotEqual
            | Token::Not
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::Greater
            | Token::GreaterOrEqual
    )
}

pub(crate) fn is_func_call_end_symbol(symbol: Option<Token>) -> bool {
    match symbol {
        None => true,
        Some(Token::Error) => true,
        Some(sym) => matches!(
            sym,
            Token::NewLine
                | Token::Arroba
                | Token::Arrow
                | Token::BackArrow
                | Token::Indent
                | Token::Dedent
                | Token::Assign
                | Token::Dollar
                | Token::DollarCurly
                | Token::Comma
                | Token::Cond
                | Token::Case
                | Token::Dot
                | Token::DotDot
                | Token::DotDotDot
                | Token::Cons
                | Token::FatArrow
                | Token::Let
                | Token::Loop
                | Token::Do
                | Token::Then
                | Token::Else
                | Token::Elif
                | Token::Of
                | Token::Return
                | Token::Recur
                | Token::Yield
                | Token::RightParen
                | Token::In
                | Token::RightBracket
                | Token::RightCurly
                | Token::RightCurlyCurly
                | Token::Where
                | Token::With
                | Token::While
                | Token::Until
                | Token::PipeLeft
                | Token::Perform
                | Token::PipeRight
                | Token::ComposeForward
                | Token::ComposeBackward
                | Token::Pow
                | Token::Plus
                | Token::PlusPlus
                | Token::Mult
                | Token::Minus
                | Token::Div
                | Token::DivDiv
                | Token::Mod
                | Token::And
                | Token::Or
                | Token::Not
                | Token::Equal
                | Token::NotEqual
                | Token::Guard
                | Token::GreaterOrEqual
                | Token::Greater
                | Token::LessThan
                | Token::LessThanOrEqual
                | Token::Type
                | Token::Trait
                | Token::Try
                | Token::Alias
                | Token::SemiColon
                | Token::Colon
                | Token::Derive
                | Token::Extends
                | Token::Reify
                | Token::Otherwise
                | Token::Error
        ),
    }
}

pub(crate) fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
    let LeftAssocExpr(sym, left, right) = la_expr;
    match sym {
        Token::PipeRight => Expression::FuncCallExpr(right, left),
        Token::Or => Expression::OrExpr(left, right),
        Token::And => Expression::AndExpr(left, right),
        Token::LessThan => Expression::LtExpr(left, right),
        Token::LessThanOrEqual => Expression::LeExpr(left, right),
        Token::Greater => Expression::GtExpr(left, right),
        Token::GreaterOrEqual => Expression::GeExpr(left, right),
        Token::Equal => Expression::EqExpr(left, right),
        Token::NotEqual => Expression::NeExpr(left, right),
        Token::Plus => Expression::AddExpr(left, right),
        Token::PlusPlus => Expression::ConcatExpr(left, right),
        Token::Minus => Expression::SubExpr(left, right),
        Token::Mult => Expression::MulExpr(left, right),
        Token::Div => Expression::DivExpr(left, right),
        Token::DivDiv => Expression::IntDivExpr(left, right),
        Token::Mod => Expression::ModExpr(left, right),
        Token::ComposeBackward => Expression::ComposeBckExpr(left, right),
        Token::Matches => Expression::MatchesExpr(left, right),
        Token::NotMatches => Expression::NoMatchesExpr(left, right),
        Token::Match => Expression::ReMatchExpr(left, right),
        Token::Arroba => Expression::IndexExpr(left, right),
        sym => {
            println!("TODO {:?}", sym);
            todo!()
        }
    }
}

pub(crate) fn right_assoc_expr_to_expr(ra_expr: RightAssocExpr) -> Result<Expression> {
    let RightAssocExpr(sym, left, right) = ra_expr;
    match sym {
        Token::Cons => Ok(Expression::ConsExpr(left, right)),
        Token::Pow => Ok(Expression::PowExpr(left, right)),
        Token::Dollar => Ok(Expression::FuncCallExpr(left, right)),
        Token::ComposeForward => Ok(Expression::ComposeFwdExpr(left, right)),
        Token::PipeLeft => Ok(Expression::FuncCallExpr(left, right)),
        _ => todo!(),
    }
}
