use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::{
    consume_args, consume_exprs_sep_by, consume_ids_sep_by, is_func_call_end_symbol, is_literal,
    left_assoc_expr_to_expr, parse_left_assoc_expr, parse_right_assoc_expr,
    right_assoc_expr_to_expr, LeftAssocExpr, RightAssocExpr,
};
use crate::parser::{consume_symbol, parse_opt_dedent, parse_opt_indent, ParseError, Parser};
use anyhow::{Context, Error, Result};

#[macro_export]
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

#[macro_export]
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
    LazyExpr(Box<Expression>),
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
    LetExpr(Vec<Equation>, Box<Expression>),
}

pub type ParseResult = Result<(Expression, usize)>;

#[derive(Debug, Clone)]
pub enum LambdaArg {
    Simple(String),
    Tuple(Vec<String>),
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
            let (args, pos) = consume_args(parser, pos + 1)?;
            Ok((Expression::FuncCallWithDollar(Box::new(expr), args), pos))
        } else {
            Ok((expr, pos))
        }
    }

    pub fn parse_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_lambda_expr(parser: &Parser, pos: usize) -> ParseResult {
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
                    Ok((LambdaArg::Tuple(ids), pos))
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
            Some(_) => {
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos + 1)?;
                if parser.peek(pos, Symbol::RightParen) {
                    Ok((expr, pos + 1))
                } else {
                    println!(
                        "expr = {:?} @={}, next={:?}",
                        expr,
                        pos,
                        parser.get_symbol(pos)
                    );
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
            Some(_) => {
                let (exprs, pos) = consume_exprs_sep_by(parser, pos + 1, Symbol::Comma)?;
                if parser.peek(pos, Symbol::RightBracket) {
                    Ok((Expression::ListExpr(exprs), pos + 1))
                } else if parser.peek(pos, Symbol::DotDot) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Symbol::RightBracket) {
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

    fn parse_lazy_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Lazy)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
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
                Err(Error::new(OguError::ParserError(ParseError::InvalidArg))).context(format!(
                    "invalid symbol: {:?} @{:?}:{}",
                    sym,
                    parser.pos_to_line(pos),
                    pos
                ))
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

    fn parse_let_equation(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        Equation::parse(parser, pos)
    }

    fn parse_loop(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_if(_parser: &Parser, _pos: usize) -> ParseResult {
        todo!()
    }

    fn parse_do(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Do)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut exprs = vec![];
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let mut pos = parser.skip_nl(pos);
        exprs.push(expr);
        while !parser.peek(pos, Symbol::Dedent) {
            let (expr, new_pos) = Expression::parse(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            exprs.push(expr);
        }
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        Ok((Expression::DoExpr(exprs), pos))
    }
}
