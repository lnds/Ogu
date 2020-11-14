use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

struct LeftAssocExpr<'a>(Symbol<'a>, Box<Expression>, Vec<Expression>);
struct RightAssocExpr<'a>(Symbol<'a>, Box<Expression>, Vec<Expression>);

#[derive(Debug, Clone)]
pub enum Expression {
    Error,
    Identifier(String),
    Atom(String),
    StringLiteral(String),
    IntegerLiteral(String),
    Unit,
    PipeFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeFirstArgFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeBackFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    PipeBackFirstArgFuncCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    FuncCallWithDollar {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    FuncCallExpr {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    LambdaExpr {
        args: Vec<LambdaArg>,
        expr: Box<Expression>,
    },
    MatchesExpr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    NoMatchesExpr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    ReMatchExpr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    ConsExpr {
        head: Box<Expression>,
        tail: Vec<Expression>,
    },
    PowExpr {
        base: Box<Expression>,
        exponents: Vec<Expression>,
    },
    DotoCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    DotoBackCall {
        name: Box<Expression>,
        args: Vec<Expression>,
    },
    OrExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    AndExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    LeExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    LtExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    GeExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    GtExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    EqExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    NeExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    AddExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    SubExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    MulExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    DivExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    IntDivExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    ModExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    ComposeFwdExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
    ComposeBckExpr {
        first: Box<Expression>,
        rest: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum LambdaArg {
    Simple(String),
    Tupled(Vec<String>),
}

fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
    let LeftAssocExpr(sym, name, args) = la_expr;
    match sym {
        Symbol::PipeRight => Expression::PipeFuncCall { name, args },
        Symbol::PipeRightFirstArg => Expression::PipeFirstArgFuncCall { name, args },
        Symbol::PipeLeft => Expression::PipeBackFuncCall { name, args },
        Symbol::PipeLeftFirstArg => Expression::PipeBackFirstArgFuncCall { name, args },
        Symbol::Doto => Expression::DotoCall { name, args },
        Symbol::DotoBack => Expression::DotoBackCall { name, args },
        Symbol::Or => Expression::OrExpr {
            first: name,
            rest: args,
        },
        Symbol::And => Expression::AndExpr {
            first: name,
            rest: args,
        },
        Symbol::LessThan => Expression::LtExpr {
            first: name,
            rest: args,
        },
        Symbol::LessThanOrEqual => Expression::LeExpr {
            first: name,
            rest: args,
        },
        Symbol::Greater => Expression::GtExpr {
            first: name,
            rest: args,
        },
        Symbol::GreaterOrEqual => Expression::GeExpr {
            first: name,
            rest: args,
        },
        Symbol::Equal => Expression::EqExpr {
            first: name,
            rest: args,
        },
        Symbol::NotEqual => Expression::NeExpr {
            first: name,
            rest: args,
        },
        Symbol::Plus => Expression::AddExpr {
            first: name,
            rest: args,
        },
        Symbol::Minus => Expression::SubExpr {
            first: name,
            rest: args,
        },
        Symbol::Mult => Expression::MulExpr {
            first: name,
            rest: args,
        },
        Symbol::Div => Expression::DivExpr {
            first: name,
            rest: args,
        },
        Symbol::DivDiv => Expression::IntDivExpr {
            first: name,
            rest: args,
        },
        Symbol::Mod => Expression::ModExpr {
            first: name,
            rest: args,
        },
        Symbol::ComposeForward => Expression::ComposeFwdExpr {
            first: name,
            rest: args,
        },
        Symbol::ComposeBackward => Expression::ComposeBckExpr {
            first: name,
            rest: args,
        },
        _ => Expression::Error,
    }
}

fn right_assoc_expr_to_expr(ra_expr: RightAssocExpr) -> Expression {
    let RightAssocExpr(sym, name, args) = ra_expr;
    match sym {
        Symbol::Cons => Expression::ConsExpr {
            head: name,
            tail: args,
        },
        Symbol::Pow => Expression::PowExpr {
            base: name,
            exponents: args,
        },
        _ => Expression::Error,
    }
}

type ParseResult = Result<(Expression, usize)>;

macro_rules! parse_left_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &Parser, pos: usize) -> ParseResult {
            parse_left_assoc_expr(parser, pos, $op, $next_level, |name, (args, pos)| {
                let la_expr = LeftAssocExpr($op, Box::new(name), args);
                Ok((left_assoc_expr_to_expr(la_expr), pos))
            })
        }
    };
}

macro_rules! parse_right_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &Parser, pos: usize) -> ParseResult {
            parse_right_assoc_expr(parser, pos, $op, $next_level, |name, (args, pos)| {
                let ra_expr = RightAssocExpr($op, Box::new(name), args);
                Ok((right_assoc_expr_to_expr(ra_expr), pos))
            })
        }
    };
}

impl Expression {
    pub fn parse(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_pipe_func_call_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_pipe_func_call_expr,
        Symbol::PipeRight,
        Expression::parse_pipe_first_arg_func_call_expr
    );

    parse_left_assoc!(
        parse_pipe_first_arg_func_call_expr,
        Symbol::PipeRightFirstArg,
        Expression::parse_backpipe_func_call_expr
    );

    parse_left_assoc!(
        parse_backpipe_func_call_expr,
        Symbol::PipeLeft,
        Expression::parse_backpipe_first_arg_func_call_expr
    );

    parse_left_assoc!(
        parse_backpipe_first_arg_func_call_expr,
        Symbol::PipeLeftFirstArg,
        Expression::parse_doto_func_call_expr
    );

    parse_left_assoc!(
        parse_doto_func_call_expr,
        Symbol::Doto,
        Expression::parse_backdoto_func_call_expr
    );

    parse_left_assoc!(
        parse_backdoto_func_call_expr,
        Symbol::DotoBack,
        Expression::parse_dollar_func_call_expr
    );

    fn parse_dollar_func_call_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (expr, pos) = Expression::parse_expr(parser, pos)?;
        if parser.peek(pos, Symbol::Dollar) {
            let (args, pos) = consume_args(parser, pos)?;
            Ok((
                Expression::FuncCallWithDollar {
                    name: Box::new(expr),
                    args: args,
                },
                pos,
            ))
        } else {
            Ok((expr, pos))
        }
    }

    fn parse_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("Expecting an expression but found eof"),
            Some(Symbol::Let) => Expression::parse_let(parser, pos),
            Some(Symbol::For) => Expression::parse_loop(parser, pos),
            Some(Symbol::Loop) => Expression::parse_loop(parser, pos),
            Some(Symbol::Until) => Expression::parse_loop(parser, pos),
            Some(Symbol::While) => Expression::parse_loop(parser, pos),
            Some(Symbol::If) => Expression::parse_if(parser, pos),
            _ => Expression::parse_lambda_expr(parser, pos),
        }
    }

    fn parse_lambda_expr(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::Lambda) {
            Expression::parse_logical_expr(parser, pos)
        } else {
            let (args, pos) = Expression::parse_lambda_args(parser, pos + 1)?;
            if !parser.peek(pos, Symbol::Arrow) {
                Err(Error::new(OguError::ParserError(
                    ParseError::ExpectingArrow,
                )))
            } else {
                let pos = parser.skip_nl(pos + 1); // skip arrow
                let (expr, pos) = Expression::parse_expr(parser, pos)?;
                Ok((
                    Expression::LambdaExpr {
                        args,
                        expr: Box::new(expr),
                    },
                    pos,
                ))
            }
        }
    }

    fn parse_lambda_args(parser: &Parser, pos: usize) -> Result<(Vec<LambdaArg>, usize)> {
        let mut args = vec![];
        let (arg, mut pos) = Expression::parse_lambda_arg(parser, pos)?;
        args.push(arg);
        while !parser.peek(pos, Symbol::Arrow) {
            let (arg, new_pos) = Expression::parse_lambda_arg(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_lambda_arg(parser: &Parser, pos: usize) -> Result<(LambdaArg, usize)> {
        match parser.get_symbol(pos) {
            Some(Symbol::LeftParen) => {
                let (ids, pos) = consume_ids_sep_by(parser, pos, Symbol::Comma)?;
                if !parser.peek(pos, Symbol::RightParen) {
                    Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingIdentifier,
                    )))
                } else {
                    Ok((LambdaArg::Tupled(ids), pos))
                }
            }
            Some(Symbol::Id(id)) => Ok((LambdaArg::Simple(id.to_string()), pos + 1)),
            _ => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingLambdaArg,
            ))),
        }
    }

    fn parse_logical_expr(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_logical_or_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_logical_or_expr,
        Symbol::Or,
        Expression::parse_logical_and_expr
    );

    parse_left_assoc!(
        parse_logical_and_expr,
        Symbol::And,
        Expression::parse_comparative_expr
    );

    fn parse_comparative_expr(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_lt_expr(parser, pos)
    }

    parse_left_assoc!(parse_lt_expr, Symbol::LessThan, Expression::parse_le_expr);

    parse_left_assoc!(
        parse_le_expr,
        Symbol::LessThanOrEqual,
        Expression::parse_gt_expr
    );

    parse_left_assoc!(parse_gt_expr, Symbol::Greater, Expression::parse_ge_expr);

    parse_left_assoc!(
        parse_ge_expr,
        Symbol::GreaterOrEqual,
        Expression::parse_eq_expr
    );

    parse_left_assoc!(parse_eq_expr, Symbol::Equal, Expression::parse_ne_expr);

    parse_left_assoc!(
        parse_ne_expr,
        Symbol::NotEqual,
        Expression::parse_regex_expr
    );

    fn parse_regex_expr(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_matches_expr(parser, pos)
    }

    fn parse_matches_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (left, pos) = Expression::parse_nomatch_expr(parser, pos)?;
        if !parser.peek(pos, Symbol::Matches) {
            Ok((left, pos))
        } else {
            let pos = parser.skip_nl(pos + 1);
            let (right, pos) = Expression::parse_nomatch_expr(parser, pos)?;
            Ok((
                Expression::MatchesExpr {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                pos,
            ))
        }
    }

    fn parse_nomatch_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (left, pos) = Expression::parse_rematch_expr(parser, pos)?;
        if !parser.peek(pos, Symbol::NotMatches) {
            Ok((left, pos))
        } else {
            let pos = parser.skip_nl(pos + 1);
            let (right, pos) = Expression::parse_rematch_expr(parser, pos)?;
            Ok((
                Expression::NoMatchesExpr {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                pos,
            ))
        }
    }

    fn parse_rematch_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (left, pos) = Expression::parse_cons_expr(parser, pos)?;
        if !parser.peek(pos, Symbol::Match) {
            Ok((left, pos))
        } else {
            let pos = parser.skip_nl(pos + 1);
            let (right, pos) = Expression::parse_cons_expr(parser, pos)?;
            Ok((
                Expression::ReMatchExpr {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                pos,
            ))
        }
    }

    parse_right_assoc!(parse_cons_expr, Symbol::Cons, Expression::parse_add_expr);

    parse_left_assoc!(parse_add_expr, Symbol::Plus, Expression::parse_sub_expr);

    parse_left_assoc!(parse_sub_expr, Symbol::Minus, Expression::parse_concat_expr);

    parse_left_assoc!(
        parse_concat_expr,
        Symbol::PlusPlus,
        Expression::parse_mult_expr
    );

    parse_left_assoc!(parse_mult_expr, Symbol::Mult, Expression::parse_div_expr);

    parse_left_assoc!(parse_div_expr, Symbol::Div, Expression::parse_int_div_expr);

    parse_left_assoc!(
        parse_int_div_expr,
        Symbol::DivDiv,
        Expression::parse_mod_expr
    );

    parse_left_assoc!(parse_mod_expr, Symbol::Mod, Expression::parse_pow_expr);

    parse_right_assoc!(
        parse_pow_expr,
        Symbol::Pow,
        Expression::parse_compose_fwd_expr
    );

    parse_left_assoc!(
        parse_compose_fwd_expr,
        Symbol::ComposeForward,
        Expression::parse_compose_bck_expr
    );

    parse_left_assoc!(
        parse_compose_bck_expr,
        Symbol::ComposeBackward,
        Expression::parse_primary_expr
    );

    fn parse_primary_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::LeftParen) => Expression::parse_paren_expr(parser, pos),
            Some(Symbol::LeftBracket) => Expression::parse_list_expr(parser, pos),
            Some(Symbol::LeftCurly) => Expression::parse_record_expr(parser, pos),
            Some(Symbol::HashCurly) => Expression::parse_set_expr(parser, pos),
            Some(Symbol::Lazy) => Expression::parse_lazy_expr(parser, pos),
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            Some(Symbol::TypeId(_)) => Expression::parse_ctor_expr(parser, pos),
            _ => Expression::parse_func_call_expr(parser, pos),
        }
    }

    fn parse_paren_expr(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::LeftParen) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingLeftParenthesis,
            )));
        }
        match parser.get_symbol(pos + 1) {
            Some(Symbol::RightParen) => Ok((Expression::Unit, pos + 2)),
            Some(sym) => {
                println!("parse_paren_expr sym = {:?}", sym);
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos + 1)?;
                println!("parse_paren_expr expr = {:?}, pos = {}", expr, pos);
                if parser.peek(pos, Symbol::RightParen) {
                    Ok((expr, pos + 1))
                } else {
                    todo!()
                }
            }
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("unexpected eof"),
        }
    }

    fn parse_list_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_record_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_set_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_lazy_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_literal_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::String(str)) => Ok((Expression::StringLiteral(str.to_string()), pos + 1)),
            Some(Symbol::Integer(int)) => {
                Ok((Expression::IntegerLiteral(int.to_string()), pos + 1))
            }
            _ => todo!(),
        }
    }

    fn parse_ctor_expr(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_func_call_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if is_func_call_end_symbol(parser.get_symbol(pos)) {
            Ok((expr, pos))
        } else {
            let (args, pos) = Expression::parse_func_call_args(parser, pos)?;
            Ok((
                Expression::FuncCallExpr {
                    name: Box::new(expr),
                    args,
                },
                pos,
            ))
        }
    }

    fn parse_func_call_args(parser: &Parser, pos: usize) -> Result<(Vec<Expression>, usize)> {
        let mut args = vec![];
        let (expr, mut pos) = Expression::parse_prim_expr(parser, pos)?;
        args.push(expr);
        while !is_func_call_end_symbol(parser.get_symbol(pos)) {
            let (expr, new_pos) = Expression::parse_prim_expr(parser, pos)?;
            args.push(expr);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_prim_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::Id(id)) => Ok((Expression::Identifier(id.to_string()), pos + 1)),
            Some(Symbol::Key(atom)) => Ok((Expression::Atom(atom.to_string()), pos + 1)),
            sym if is_func_call_end_symbol(sym) => {
                Err(Error::new(OguError::ParserError(ParseError::InvalidArg)))
                    .context(format!("invalid symbol: {:?}", sym))
            }
            _ => Expression::parse_dollar_func_call_expr(parser, pos),
        }
    }

    fn parse_let(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_loop(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_if(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }
}

fn parse_left_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    build: fn(Expression, (Vec<Expression>, usize)) -> ParseResult,
) -> Result<(Expression, usize)> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        build(expr, consume_op_args(parser, pos, op, next_level)?)
    }
}

fn consume_op_args(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
) -> Result<(Vec<Expression>, usize)> {
    let mut args = vec![];
    if !parser.peek(pos, op) {
        Ok((args, pos))
    } else {
        let pos = parser.skip_nl(pos + 1);
        let (expr, mut pos) = next_level(parser, pos)?;
        args.push(expr);
        while parser.peek(pos, op) {
            pos = parser.skip_nl(pos + 1);
            let (expr, new_pos) = next_level(parser, pos)?;
            args.push(expr);
            pos = new_pos;
        }
        Ok((args, pos))
    }
}

fn parse_right_assoc_expr(
    parser: &Parser,
    pos: usize,
    op: Symbol,
    next_level: fn(&Parser, usize) -> ParseResult,
    build: fn(Expression, (Vec<Expression>, usize)) -> ParseResult,
) -> Result<(Expression, usize)> {
    let (expr, pos) = next_level(parser, pos)?;
    if !parser.peek(pos, op) {
        Ok((expr, pos))
    } else {
        let mut pos = pos;
        let mut args = vec![];
        while parser.peek(pos, op) {
            let (expr, new_pos) = next_level(parser, pos + 1)?;
            pos = new_pos;
            args.push(expr);
        }
        build(expr, (args, pos))
    }
}

fn consume_args(parser: &Parser, pos: usize) -> Result<(Vec<Expression>, usize)> {
    let mut args = vec![];
    let mut pos = pos;
    while !is_func_call_end_symbol(parser.get_symbol(pos)) {
        let (expr, new_pos) = Expression::parse_expr(parser, pos)?;
        pos = new_pos;
        args.push(expr);
    }
    Ok((args, pos))
}

fn consume_ids_sep_by(parser: &Parser, pos: usize, symbol: Symbol) -> Result<(Vec<String>, usize)> {
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

fn consume_id(parser: &Parser, pos: usize) -> Result<(String, usize)> {
    if let Some(Symbol::Id(id)) = parser.get_symbol(pos) {
        Ok((id.to_string(), pos + 1))
    } else {
        Err(Error::new(OguError::ParserError(
            ParseError::ExpectingIdentifier,
        )))
    }
}

fn is_literal(symbol: Symbol) -> bool {
    match symbol {
        Symbol::Integer(_) => true,
        Symbol::Float(_) => true,
        Symbol::String(_) => true,
        Symbol::RegExp(_) => true,
        Symbol::Char(_) => true,
        Symbol::True => true,
        Symbol::False => true,
        Symbol::IsoDate(_) => true,
        _ => false,
    }
}

fn is_func_call_end_symbol(symbol: Option<Symbol>) -> bool {
    match symbol {
        None => true,
        Some(sym) => match sym {
            Symbol::NewLine
            | Symbol::Indent
            | Symbol::Dedent
            | Symbol::Assign
            | Symbol::Dollar
            | Symbol::Comma
            | Symbol::Let
            | Symbol::Do
            | Symbol::Then
            | Symbol::Else
            | Symbol::Elif
            | Symbol::RightParen
            | Symbol::In
            | Symbol::RightBracket
            | Symbol::RightCurly
            | Symbol::Where
            | Symbol::PipeLeft
            | Symbol::PipeLeftFirstArg
            | Symbol::PipeRight
            | Symbol::PipeRightFirstArg
            | Symbol::ComposeForward
            | Symbol::ComposeBackward
            | Symbol::Plus
            | Symbol::PlusPlus
            | Symbol::Mult
            | Symbol::Minus
            | Symbol::Div
            | Symbol::DivDiv
            | Symbol::Mod
            | Symbol::And
            | Symbol::BitAnd
            | Symbol::Or
            | Symbol::Not
            | Symbol::Equal
            | Symbol::NotEqual
            | Symbol::GreaterOrEqual
            | Symbol::Greater
            | Symbol::LessThan
            | Symbol::LessThanOrEqual
            | Symbol::Type
            | Symbol::Trait
            | Symbol::Alias => true,
            _ => false,
        },
    }
}
