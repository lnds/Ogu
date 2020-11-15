use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::module::body::{parse_opt_dedent, parse_opt_indent, Guard};
use crate::parser::{ParseError, Parser};
use anyhow::{Context, Error, Result};

struct LeftAssocExpr<'a>(Symbol<'a>, Box<Expression>, Box<Expression>);

struct RightAssocExpr<'a>(Symbol<'a>, Box<Expression>, Box<Expression>);

#[derive(Debug, Clone)]
pub enum Expression {
    Error,
    Identifier(String),
    Atom(String),
    StringLiteral(String),
    IntegerLiteral(String),
    DateLiteral(String),
    Unit,
    EmptyList,
    ListExpr(Vec<Expression>),
    RangeExpr(Vec<Expression>, Box<Expression>),
    PipeFuncCall(Box<Expression>, Box<Expression>),
    PipeFirstArgFuncCall(Box<Expression>, Box<Expression>),
    PipeBackFuncCall(Box<Expression>, Box<Expression>),
    PipeBackFirstArgFuncCall(Box<Expression>, Box<Expression>),
    FuncCallWithDollar(Box<Expression>, Vec<Expression>),
    FuncCallExpr(Box<Expression>, Vec<Expression>),
    LambdaExpr(Vec<LambdaArg>, Box<Expression>),
    MatchesExpr(Box<Expression>, Box<Expression>),
    NoMatchesExpr(Box<Expression>, Box<Expression>),
    ReMatchExpr(Box<Expression>, Box<Expression>),
    ConsExpr(Box<Expression>, Box<Expression>),
    PowExpr(Box<Expression>, Box<Expression>),
    DotoCall(Box<Expression>, Box<Expression>),
    DotoBackCall(Box<Expression>, Box<Expression>),
    OrExpr(Box<Expression>, Box<Expression>),
    AndExpr(Box<Expression>, Box<Expression>),
    LeExpr(Box<Expression>, Box<Expression>),
    LtExpr(Box<Expression>, Box<Expression>),
    GeExpr(Box<Expression>, Box<Expression>),
    GtExpr(Box<Expression>, Box<Expression>),
    EqExpr(Box<Expression>, Box<Expression>),
    NeExpr(Box<Expression>, Box<Expression>),
    AddExpr(Box<Expression>, Box<Expression>),
    SubExpr(Box<Expression>, Box<Expression>),
    MulExpr(Box<Expression>, Box<Expression>),
    DivExpr(Box<Expression>, Box<Expression>),
    IntDivExpr(Box<Expression>, Box<Expression>),
    ModExpr(Box<Expression>, Box<Expression>),
    ComposeFwdExpr(Box<Expression>, Box<Expression>),
    ComposeBckExpr(Box<Expression>, Box<Expression>),
    DoExpr(Vec<Expression>),
    LetExpr(Vec<LetEquation>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum LambdaArg {
    Simple(String),
    Tupled(Vec<String>),
}

#[derive(Debug, Clone)]
pub enum LetArg {
    Void,
    SimpleArg(String),
    TupleArg(Vec<LetArg>),
}

#[derive(Debug, Clone)]
pub enum LetEquation {
    Value {
        name: String,
        expr: Expression,
    },
    Function {
        name: String,
        args: Vec<LetArg>,
        expr: Expression,
    },
    FunctionWithGuards {
        name: String,
        args: Vec<LetArg>,
        guards: Vec<Guard>,
    },
}

fn left_assoc_expr_to_expr(la_expr: LeftAssocExpr) -> Expression {
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
        Symbol::Minus => Expression::SubExpr(left, right),
        Symbol::Mult => Expression::MulExpr(left, right),
        Symbol::Div => Expression::DivExpr(left, right),
        Symbol::DivDiv => Expression::IntDivExpr(left, right),
        Symbol::Mod => Expression::ModExpr(left, right),
        Symbol::ComposeForward => Expression::ComposeFwdExpr(left, right),
        Symbol::ComposeBackward => Expression::ComposeBckExpr(left, right),
        _ => todo!(),
    }
}

fn right_assoc_expr_to_expr(ra_expr: RightAssocExpr) -> Expression {
    let RightAssocExpr(sym, left, right) = ra_expr;
    match sym {
        Symbol::Cons => Expression::ConsExpr(left, right),
        Symbol::Pow => Expression::PowExpr(left, right),
        _ => Expression::Error,
    }
}

type ParseResult = Result<(Expression, usize)>;

macro_rules! parse_left_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &Parser, pos: usize) -> ParseResult {
            parse_left_assoc_expr(parser, pos, $op, $next_level, |left, right| {
                let la_expr = LeftAssocExpr($op, Box::new(left), Box::new(right));
                left_assoc_expr_to_expr(la_expr)
            })
        }
    };
}

macro_rules! parse_right_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &Parser, pos: usize) -> ParseResult {
            parse_right_assoc_expr(parser, pos, $op, $next_level, |base_expr, expr| {
                let ra_expr = RightAssocExpr($op, Box::new(base_expr), Box::new(expr));
                right_assoc_expr_to_expr(ra_expr)
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
            Ok((Expression::FuncCallWithDollar(Box::new(expr), args), pos))
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
            Some(Symbol::Do) => Expression::parse_do(parser, pos),
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
                Ok((Expression::LambdaExpr(args, Box::new(expr)), pos))
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
                Expression::MatchesExpr(Box::new(left), Box::new(right)),
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
                Expression::NoMatchesExpr(Box::new(left), Box::new(right)),
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
                Expression::ReMatchExpr(Box::new(left), Box::new(right)),
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
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos + 1)?;
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

    fn parse_list_expr(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::LeftBracket) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingLeftParenthesis,
            )));
        }
        match parser.get_symbol(pos + 1) {
            Some(Symbol::RightBracket) => Ok((Expression::EmptyList, pos + 2)),
            Some(sym) => {
                let (exprs, pos) = consume_exprs_sep_by(parser, pos + 1, Symbol::Comma)?;
                if parser.peek(pos, Symbol::RightBracket) {
                    Ok((Expression::ListExpr(exprs), pos + 1))
                } else if parser.peek(pos, Symbol::DotDot) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Symbol::RightBracket) {
                        println!("RANGE -> {:?} .. {:?}", exprs, expr);
                        return Ok((Expression::RangeExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
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
            Some(Symbol::IsoDate(date)) => Ok((Expression::DateLiteral(date.to_string()), pos + 1)),
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
            Ok((Expression::FuncCallExpr(Box::new(expr), args), pos))
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

    fn parse_let(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::Let) {
            return Err(Error::new(OguError::ParserError(ParseError::ExpectingLet)))
                .context("expecting let");
        }
        let (indent, pos) = parse_opt_indent(parser, pos + 1);
        let mut equations = vec![];
        let (equation, pos) = Expression::parse_let_equation(parser, pos)?;
        equations.push(equation);
        let mut pos = parser.skip_nl(pos);
        while !parser.peek(pos, Symbol::Dedent) && !parser.peek(pos, Symbol::In) {
            let (equation, new_pos) = Expression::parse_let_equation(parser, pos)?;
            equations.push(equation);
            pos = parser.skip_nl(new_pos);
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        if !parser.peek(pos, Symbol::In) {
            return Err(Error::new(OguError::ParserError(ParseError::ExpectingIn)))
                .context("expecting in");
        }
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos + 1);
        let (expr, pos) = Expression::parse(parser, pos + 1)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::LetExpr(equations, Box::new(expr)), pos))
    }

    fn parse_let_equation(parser: &Parser, pos: usize) -> Result<(LetEquation, usize)> {
        if let Some(Symbol::Id(id)) = parser.get_symbol(pos) {
            Expression::parse_let_func_or_val(id, parser, pos)
        } else {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIdentifier,
            )));
        }
    }

    fn parse_let_func_or_val(
        id: &str,
        parser: &Parser,
        pos: usize,
    ) -> Result<(LetEquation, usize)> {
        let name = id.to_string();
        if parser.peek(pos, Symbol::Assign) {
            Expression::parse_let_val(name, parser, pos + 1)
        } else {
            Expression::parse_let_func(name, parser, pos)
        }
    }

    fn parse_let_val(name: String, parser: &Parser, pos: usize) -> Result<(LetEquation, usize)> {
        // we already parsed a =
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((LetEquation::Value { name, expr }, pos))
    }

    fn parse_let_func(name: String, parser: &Parser, pos: usize) -> Result<(LetEquation, usize)> {
        let (args, pos) = LetArg::parse(parser, pos)?;
        if parser.peek(pos, Symbol::Assign) {
            Expression::parse_let_func_no_guards(name, args, parser, pos + 1)
        } else {
            Expression::parse_let_func_guards(name, args, parser, pos)
        }
    }

    fn parse_let_func_no_guards(
        name: String,
        args: Vec<LetArg>,
        parser: &Parser,
        pos: usize,
    ) -> Result<(LetEquation, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let eq = LetEquation::Function { name, args, expr };
        Ok((eq, pos))
    }

    fn parse_let_func_guards(
        name: String,
        args: Vec<LetArg>,
        parser: &Parser,
        pos: usize,
    ) -> Result<(LetEquation, usize)> {
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
        let eq = LetEquation::FunctionWithGuards { name, args, guards };
        Ok((eq, pos))
    }

    fn parse_loop(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_if(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_do(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::Do) {
            return Err(Error::new(OguError::ParserError(ParseError::ExpectingDo)));
        }
        let pos = parser.skip_nl(pos + 1);
        if !parser.peek(pos, Symbol::Indent) {
            return Err(Error::new(OguError::ParserError(
                ParseError::ExpectingIndentation,
            )));
        }
        let mut exprs = vec![];
        let (expr, mut pos) = Expression::parse(parser, pos + 1)?;
        exprs.push(expr);
        while !parser.peek(pos, Symbol::Dedent) {
            pos = parser.skip_nl(pos);
            let (expr, new_pos) = Expression::parse(parser, pos)?;
            pos = new_pos;
            exprs.push(expr);
        }
        Ok((Expression::DoExpr(exprs), pos))
    }
}

// 1 + 2 + 3 => (1 + 2) + 3
fn parse_left_assoc_expr(
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

fn consume_left_args(
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

// 1 ^ 2 ^ 3 => 1 ^ (2 ^ 3)
fn parse_right_assoc_expr(
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

fn consume_right_args(
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

fn consume_exprs_sep_by(
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
            | Symbol::Cons
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

impl LetArg {
    fn parse(parser: &Parser, pos: usize) -> Result<(Vec<LetArg>, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((arg, new_pos)) = LetArg::parse_arg(parser, pos)? {
            result.push(arg);
            pos = new_pos;
        }
        pos = parser.skip_nl(pos);
        Ok((result, pos))
    }

    fn parse_arg(parser: &Parser, pos: usize) -> Result<Option<(LetArg, usize)>> {
        match parser.get_symbol(pos) {
            None => Ok(None),
            Some(Symbol::LeftParen) => LetArg::parse_tuple(parser, pos),
            Some(Symbol::Id(id)) => Ok(Some((LetArg::SimpleArg(id.to_string()), pos + 1))),
            Some(Symbol::Assign) => Ok(None),
            Some(Symbol::NewLine) => Ok(None),
            s => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingValidArg,
            )))
            .context(format!("{:?} not valid", s)),
        }
    }

    fn parse_tuple(parser: &Parser, pos: usize) -> Result<Option<(LetArg, usize)>> {
        if parser.peek(pos + 1, Symbol::RightParen) {
            return Ok(Some((LetArg::Void, pos + 2)));
        }
        let mut args = vec![];
        let mut pos = pos + 1;
        match LetArg::parse_arg(parser, pos + 1)? {
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
            match LetArg::parse_arg(parser, pos + 1)? {
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
        Ok(Some((LetArg::TupleArg(args), pos + 1)))
    }
}
