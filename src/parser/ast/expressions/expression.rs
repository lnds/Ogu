use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression::TryHandleExpr;
use crate::parser::ast::expressions::{
    consume_args, consume_exprs_sep_by, consume_id, consume_ids_sep_by, is_basic_op,
    is_func_call_end_symbol, is_literal, left_assoc_expr_to_expr, parse_left_assoc_expr,
    parse_right_assoc_expr, right_assoc_expr_to_expr, LeftAssocExpr, RightAssocExpr,
};
use crate::parser::{
    consume_opt_symbol, consume_symbol, consume_type_id, parse_opt_dedent, parse_opt_indent,
    raise_parser_error, Parser,
};
use anyhow::Result;

#[macro_export]
macro_rules! parse_left_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name (parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
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
        fn $func_name (parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
            parse_right_assoc_expr(parser, pos, $op, $next_level, |base_expr, expr| {
                let ra_expr = RightAssocExpr($op, Box::new(base_expr), Box::new(expr));
                right_assoc_expr_to_expr(ra_expr)
            })
        }
    };
}

#[derive(Debug, Clone)]
pub(crate) enum RecurValue<'a> {
    Value(Expression<'a>),
    Var(&'a str, Expression<'a>),
}

#[derive(Debug, Clone)]
pub(crate) enum LoopCond<'a> {
    WhileExpr(Box<Expression<'a>>),
    UntilExpr(Box<Expression<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) struct HandleGuard<'a>(
    Expression<'a>,
    Option<Expression<'a>>,
    Option<Vec<&'a str>>,
    Option<Expression<'a>>,
);

#[derive(Debug, Clone)]
pub(crate) enum Expression<'a> {
    Identifier(&'a str),
    QualifedIdentifier(&'a str, Vec<&'a str>),
    TypeIdentifier(&'a str),
    StringLiteral(&'a str),
    LargeStringLiteral(Option<String>),
    RegexpLiteral(&'a str),
    CharLiteral(&'a str),
    IntegerLiteral(&'a str),
    FloatLiteral(&'a str),
    DateLiteral(&'a str),
    FormatString(&'a str),
    Unit,
    EmptyList,
    NotExpr(Box<Expression<'a>>),
    LazyExpr(Box<Expression<'a>>),
    YieldExpr(Box<Expression<'a>>),
    ReifyExpr(&'a str, Vec<Equation<'a>>),
    ListExpr(Vec<Expression<'a>>),
    ListByComprehension(
        Vec<Expression<'a>>,
        Vec<Equation<'a>>,
        Vec<Expression<'a>>,
        Vec<Equation<'a>>,
    ),
    RangeExpr(Vec<Expression<'a>>, Box<Expression<'a>>),
    RangeOpenExpr(Vec<Expression<'a>>, Box<Expression<'a>>),
    RangeInfExpr(Vec<Expression<'a>>),
    // [exprs...]
    DictExpr(Vec<(Expression<'a>, Expression<'a>)>),
    SetExpr(Vec<Expression<'a>>),
    RecordExpr(Vec<(&'a str, Expression<'a>)>),
    TypedFuncCall(String, Vec<Expression<'a>>, Vec<Expression<'a>>),
    FuncCallExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    LambdaExpr(Vec<LambdaArg<'a>>, Box<Expression<'a>>),
    MatchesExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    NoMatchesExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ReMatchExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ConsExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    PowExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    IndexExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    UnaryCons(Option<Box<Expression<'a>>>),
    UnaryAdd(Option<Box<Expression<'a>>>),
    UnaryConcat(Option<Box<Expression<'a>>>),
    UnarySub(Option<Box<Expression<'a>>>),
    UnaryMul(Option<Box<Expression<'a>>>),
    UnaryPow(Option<Box<Expression<'a>>>),
    UnaryMod(Option<Box<Expression<'a>>>),
    UnaryDiv(Option<Box<Expression<'a>>>),
    UnaryAnd(Option<Box<Expression<'a>>>),
    UnaryOr(Option<Box<Expression<'a>>>),
    UnaryNot(Option<Box<Expression<'a>>>),
    UnaryEq(Option<Box<Expression<'a>>>),
    UnaryNotEq(Option<Box<Expression<'a>>>),
    UnaryGt(Option<Box<Expression<'a>>>),
    UnaryGe(Option<Box<Expression<'a>>>),
    UnaryLt(Option<Box<Expression<'a>>>),
    UnaryLe(Option<Box<Expression<'a>>>),
    OrExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    AndExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    LeExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    LtExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    GeExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    GtExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    EqExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    NeExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    AddExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ConcatExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    SubExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    MulExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    DivExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    IntDivExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ModExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ComposeFwdExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ComposeBckExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    TupleExpr(Vec<Expression<'a>>),
    DoExpr(Vec<Expression<'a>>),
    TryHandleExpr(Box<Expression<'a>>, Vec<HandleGuard<'a>>),
    RepeatExpr(Vec<RecurValue<'a>>),
    RecurExpr(Vec<Expression<'a>>),
    PerformExpr(
        Box<Expression<'a>>,
        Option<Vec<Expression<'a>>>,
        Option<Box<Expression<'a>>>,
    ),
    LetExpr(Vec<Equation<'a>>, Box<Expression<'a>>),
    CondExpr(Vec<(Option<Expression<'a>>, Expression<'a>)>),
    CaseExpr(Box<Expression<'a>>, Vec<(Option<Expression<'a>>, Expression<'a>)>),
    IfExpr(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    MacroExpandExpr(Box<Expression<'a>>),
    ResumeExpr(Option<Box<Expression<'a>>>, Option<Vec<Expression<'a>>>),
    LoopExpr(
        Option<Vec<Equation<'a>>>,
        Option<LoopCond<'a>>,
        Box<Expression<'a>>,
        Option<Box<Expression<'a>>>,
    ),
}

pub(crate) type ParseResult<'a> = Result<(Expression<'a>, usize)>;

#[derive(Debug, Clone)]
pub(crate) enum LambdaArg<'a> {
    Simple(String),
    Tuple(Vec<&'a str>),
}

impl<'a> Expression<'a> {
    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (expr, mut pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
        if parser.peek(pos, Token::SemiColon) {
            let mut exprs = vec![expr];
            while parser.peek(pos, Token::SemiColon) {
                pos = consume_symbol(parser, pos, Token::SemiColon)?;
                pos = parser.skip_nl(pos);
                let (expr, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                pos = parser.skip_nl(new_pos);
                exprs.push(expr);
            }
            Ok((Expression::DoExpr(exprs), pos))
        } else {
            Ok((expr, pos))
        }
    }

    parse_left_assoc!(
        parse_pipe_func_call_expr,
        Token::PipeRight,
        Expression::parse_backpipe_func_call_expr
    );

    parse_right_assoc!(
        parse_backpipe_func_call_expr,
        Token::PipeLeft,
        Expression::parse_control_expr
    );

    parse_right_assoc!(
        parse_dollar_func_call_expr,
        Token::Dollar,
        Expression::parse_pipe_func_call_expr
    );

    pub(crate) fn parse_control_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            None => raise_parser_error("Expecting an expression but found EOF", parser, pos, false),
            Some(Token::Cond) => Expression::parse_cond(parser, pos),
            Some(Token::Case) => Expression::parse_case(parser, pos),
            Some(Token::Do) => Expression::parse_do(parser, pos),
            Some(Token::Try) => Expression::parse_try_expr(parser, pos),
            Some(Token::Let) => Expression::parse_let(parser, pos),
            Some(Token::For) => Expression::parse_loop(parser, pos),
            Some(Token::Loop) => Expression::parse_loop(parser, pos),
            Some(Token::If) => Expression::parse_if(parser, pos),
            Some(Token::Repeat) => Expression::parse_repeat(parser, pos),
            Some(Token::Recur) => Expression::parse_recur(parser, pos),
            Some(Token::Resume) => Expression::parse_resume_expr(parser, pos),
            Some(Token::Reify) => Expression::parse_reify(parser, pos),
            _ => Expression::parse_lambda_expr(parser, pos),
        }
    }

    pub(crate) fn parse_cond(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Cond)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut conds = vec![];
        while !parser.peek(pos, Token::Dedent) {
            let (cond, new_pos) = Expression::parse_opt_otherwise(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Token::Arrow)?;
            let (in_indent, new_pos) = parse_opt_indent(parser, pos);
            let (value, new_pos) = Expression::parse(parser, new_pos)?;
            pos = parse_opt_dedent(parser, new_pos, in_indent)?;
            pos = parser.skip_nl(pos);
            if cond.is_none() {
                conds.push((None, value));
                break;
            } else {
                conds.push((cond, value));
            }
        }
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        Ok((Expression::CondExpr(conds), pos))
    }

    fn parse_opt_otherwise(parser: &'a Parser<'a>, pos: usize) -> Result<(Option<Expression<'a>>, usize)> {
        if parser.peek(pos, Token::Otherwise) {
            Ok((None, consume_symbol(parser, pos, Token::Otherwise)?))
        } else {
            let (c, p) = Expression::parse_logical_expr(parser, pos)?;
            Ok((Some(c), p))
        }
    }

    pub(crate) fn parse_case(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Case)?;
        let (match_expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Of)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut matches = vec![];
        while !parser.peek(pos, Token::Dedent) {
            let (cond, new_pos) = Expression::parse_opt_otherwise(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Token::Arrow)?;
            let (in_indent, new_pos) = parse_opt_indent(parser, pos);
            let (value, new_pos) = Expression::parse(parser, new_pos)?;
            pos = parse_opt_dedent(parser, new_pos, in_indent)?;
            pos = parser.skip_nl(pos);
            if cond.is_none() {
                matches.push((None, value));
                break;
            } else {
                matches.push((cond, value));
            }
        }
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        Ok((Expression::CaseExpr(Box::new(match_expr), matches), pos))
    }

    pub(crate) fn parse_reify(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Reify)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Token::Where)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::Indent)?;
        let (eq, mut pos) = Equation::parse(parser, pos, true)?;
        let mut eqs = vec![eq];
        pos = parser.skip_nl(pos);
        while !parser.peek(pos, Token::Dedent) {
            let (eq, new_pos) = Equation::parse(parser, pos, true)?;
            eqs.push(eq);
            pos = parser.skip_nl(new_pos);
        }
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::ReifyExpr(type_id, eqs), pos))
    }

    pub(crate) fn parse_lambda_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        if !parser.peek(pos, Token::Lambda) {
            Expression::parse_logical_expr(parser, pos)
        } else {
            let pos = consume_symbol(parser, pos, Token::Lambda)?;
            let (args, pos) = Expression::parse_lambda_args(parser, pos)?;
            let pos = consume_symbol(parser, pos, Token::Arrow)?;
            let pos = parser.skip_nl(pos); // skip arrow
            let (expr, pos) = Expression::parse_control_expr(parser, pos)?;
            Ok((Expression::LambdaExpr(args, Box::new(expr)), pos))
        }
    }

    fn parse_lambda_args(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<LambdaArg<'a>>, usize)> {
        let mut args = vec![];
        let (arg, mut pos) = Expression::parse_lambda_arg(parser, pos)?;
        args.push(arg);
        while !parser.peek(pos, Token::Arrow) {
            let (arg, new_pos) = Expression::parse_lambda_arg(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_lambda_arg(parser: &'a Parser<'a>, pos: usize) -> Result<(LambdaArg<'a>, usize)> {
        match parser.get_token(pos) {
            Some(Token::LeftParen) => {
                let (ids, pos) = consume_ids_sep_by(parser, pos + 1, Token::Comma)?;
                let pos = consume_symbol(parser, pos, Token::RightParen)?;
                Ok((LambdaArg::Tuple(ids), pos))
            }
            Some(Token::Id(id)) => Ok((LambdaArg::Simple(id.to_string()), pos + 1)),
            _ => raise_parser_error("Expecting lambda arg", parser, pos, true),
        }
    }

    fn parse_logical_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_logical_or_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_logical_or_expr,
        Token::Or,
        Expression::parse_logical_and_expr
    );

    parse_left_assoc!(
        parse_logical_and_expr,
        Token::And,
        Expression::parse_comparative_expr
    );

    fn parse_comparative_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_lt_expr(parser, pos)
    }

    parse_left_assoc!(parse_lt_expr, Token::LessThan, Expression::parse_le_expr);

    parse_left_assoc!(
        parse_le_expr,
        Token::LessThanOrEqual,
        Expression::parse_gt_expr
    );

    parse_left_assoc!(parse_gt_expr, Token::Greater, Expression::parse_ge_expr);

    parse_left_assoc!(
        parse_ge_expr,
        Token::GreaterOrEqual,
        Expression::parse_eq_expr
    );

    parse_left_assoc!(parse_eq_expr, Token::Equal, Expression::parse_ne_expr);

    parse_left_assoc!(parse_ne_expr, Token::NotEqual, Expression::parse_regex_expr);

    fn parse_regex_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_matches_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_matches_expr,
        Token::Matches,
        Expression::parse_nomatch_expr
    );

    parse_left_assoc!(
        parse_nomatch_expr,
        Token::NotMatches,
        Expression::parse_rematch_expr
    );

    parse_left_assoc!(
        parse_rematch_expr,
        Token::Match,
        Expression::parse_cons_expr
    );

    parse_right_assoc!(parse_cons_expr, Token::Cons, Expression::parse_add_expr);

    parse_left_assoc!(parse_add_expr, Token::Plus, Expression::parse_sub_expr);

    parse_left_assoc!(parse_sub_expr, Token::Minus, Expression::parse_concat_expr);

    parse_left_assoc!(
        parse_concat_expr,
        Token::PlusPlus,
        Expression::parse_mult_expr
    );

    parse_left_assoc!(parse_mult_expr, Token::Mult, Expression::parse_div_expr);

    parse_left_assoc!(parse_div_expr, Token::Div, Expression::parse_int_div_expr);

    parse_left_assoc!(
        parse_int_div_expr,
        Token::DivDiv,
        Expression::parse_mod_expr
    );

    parse_left_assoc!(parse_mod_expr, Token::Mod, Expression::parse_pow_expr);

    parse_right_assoc!(
        parse_pow_expr,
        Token::Pow,
        Expression::parse_compose_fwd_expr
    );

    parse_right_assoc!(
        parse_compose_fwd_expr,
        Token::ComposeForward,
        Expression::parse_compose_bck_expr
    );

    parse_left_assoc!(
        parse_compose_bck_expr,
        Token::ComposeBackward,
        Expression::parse_postfix_expr
    );

    parse_left_assoc!(
        parse_postfix_expr,
        Token::Arroba,
        Expression::parse_primary_expr
    );

    pub(crate) fn parse_primary_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Token::LeftBracket) => Expression::parse_list_expr(parser, pos),
            Some(Token::LeftCurly) => Expression::parse_record_expr(parser, pos),
            Some(Token::LeftCurlyCurly) => Expression::parse_macro_expand_expr(parser, pos),
            Some(Token::HashCurly) => Expression::parse_dict_expr(parser, pos),
            Some(Token::DollarCurly) => Expression::parse_set_expr(parser, pos),
            Some(Token::Lazy) => Expression::parse_lazy_expr(parser, pos),
            Some(Token::Yield) => Expression::parse_yield_expr(parser, pos),
            Some(Token::Perform) => Expression::parse_perform(parser, pos),
            Some(Token::Not) => Expression::parse_not_expr(parser, pos),
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            Some(Token::TypeId(_)) => Expression::parse_ctor_expr(parser, pos),
            _ => Expression::parse_func_call_expr(parser, pos),
        }
    }

    fn parse_macro_expand_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::LeftCurlyCurly)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::RightCurlyCurly)?;
        Ok((Expression::MacroExpandExpr(Box::new(expr)), pos))
    }

    fn parse_paren_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::LeftParen)?;
        match parser.get_token(pos) {
            Some(Token::RightParen) => Ok((Expression::Unit, pos + 1)),
            Some(op) if is_basic_op(op) => {
                let (opt_expr, pos) = if parser.peek(pos + 1, Token::RightParen) {
                    (None, pos + 1)
                } else {
                    let (expr, pos) = Expression::parse_lambda_expr(parser, pos + 1)?;
                    (Some(Box::new(expr)), pos)
                };
                let pos = consume_symbol(parser, pos, Token::RightParen)?;
                match op {
                    Token::Cons => Ok((Expression::UnaryCons(opt_expr), pos)),
                    Token::Plus => Ok((Expression::UnaryAdd(opt_expr), pos)),
                    Token::PlusPlus => Ok((Expression::UnaryConcat(opt_expr), pos)),
                    Token::Minus => Ok((Expression::UnarySub(opt_expr), pos)),
                    Token::Mult => Ok((Expression::UnaryMul(opt_expr), pos)),
                    Token::Pow => Ok((Expression::UnaryPow(opt_expr), pos)),
                    Token::Div => Ok((Expression::UnaryDiv(opt_expr), pos)),
                    Token::Mod => Ok((Expression::UnaryMod(opt_expr), pos)),
                    Token::And => Ok((Expression::UnaryAnd(opt_expr), pos)),
                    Token::Or => Ok((Expression::UnaryOr(opt_expr), pos)),
                    Token::Not => Ok((Expression::UnaryNot(opt_expr), pos)),
                    Token::Equal => Ok((Expression::UnaryEq(opt_expr), pos)),
                    Token::NotEqual => Ok((Expression::UnaryNotEq(opt_expr), pos)),
                    Token::Greater => Ok((Expression::UnaryGt(opt_expr), pos)),
                    Token::GreaterOrEqual => Ok((Expression::UnaryGe(opt_expr), pos)),
                    Token::LessThan => Ok((Expression::UnaryLt(opt_expr), pos)),
                    Token::LessThanOrEqual => Ok((Expression::UnaryLe(opt_expr), pos)),
                    _ => raise_parser_error("Expecting an operator", parser, pos, true),
                }
            }
            Some(_) => {
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                if parser.peek(pos, Token::RightParen) {
                    Ok((expr, pos + 1))
                } else if parser.peek(pos, Token::Comma) {
                    let mut exprs = vec![expr];
                    let mut pos = pos;
                    while parser.peek(pos, Token::Comma) {
                        pos = consume_symbol(parser, pos, Token::Comma)?;
                        pos = parser.skip_nl(pos);
                        let (expr, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                        pos = new_pos;
                        exprs.push(expr);
                    }
                    pos = consume_symbol(parser, pos, Token::RightParen)?;
                    Ok((Expression::TupleExpr(exprs), pos))
                } else {
                    let (arg, pos) = Expression::parse_prim_expr(parser, pos)?;
                    let pos = consume_symbol(parser, pos, Token::RightParen)?;
                    Ok((Expression::FuncCallExpr(Box::new(expr), Box::new(arg)), pos))
                }
            }
            None => raise_parser_error("unexpected EOF", parser, pos, false),
        }
    }

    fn parse_list_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::LeftBracket)?;
        match parser.get_token(pos) {
            Some(Token::RightBracket) => Ok((Expression::EmptyList, pos + 1)),
            Some(_) => {
                let (exprs, pos) = consume_exprs_sep_by(parser, pos, Token::Comma)?;
                if parser.peek(pos, Token::RightBracket) {
                    Ok((Expression::ListExpr(exprs), pos + 1))
                } else if parser.peek(pos, Token::DotDotDot) {
                    let pos = consume_symbol(parser, pos + 1, Token::RightBracket)?;
                    Ok((Expression::RangeInfExpr(exprs), pos))
                } else if parser.peek(pos, Token::DotDot) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Token::RightBracket) {
                        return Ok((Expression::RangeExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Token::DotDotLess) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Token::RightBracket) {
                        return Ok((Expression::RangeOpenExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Token::Guard) {
                    let pos = consume_symbol(parser, pos, Token::Guard)?;
                    let (eq, mut pos) = Equation::parse_back_arrow_eq(parser, pos)?;
                    let mut eqs = vec![];
                    let mut guards = vec![];
                    let mut lets = vec![];
                    eqs.push(eq);
                    while parser.peek(pos, Token::Comma) {
                        pos = consume_symbol(parser, pos, Token::Comma)?;
                        match Equation::parse_back_arrow_eq(parser, pos) {
                            Ok((eq, new_pos)) => {
                                eqs.push(eq);
                                pos = new_pos;
                            }
                            Err(_) => {
                                if parser.peek(pos, Token::Let) {
                                    pos = consume_symbol(parser, pos, Token::Let)?;
                                    let (eq, new_pos) = Equation::parse_value(parser, pos)?;
                                    lets.push(eq);
                                    pos = new_pos;
                                } else {
                                    let (expr, new_pos) =
                                        Expression::parse_logical_expr(parser, pos)?;
                                    guards.push(expr);
                                    pos = new_pos;
                                }
                            }
                        }
                    }
                    pos = consume_symbol(parser, pos, Token::RightBracket)?;
                    Ok((
                        Expression::ListByComprehension(exprs, eqs, guards, lets),
                        pos,
                    ))
                } else {
                    todo!()
                }
            }
            None => raise_parser_error("unexpected eof", parser, pos, true),
        }
    }

    fn parse_record_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let mut pos = consume_symbol(parser, pos, Token::LeftCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Token::RightCurly) {
            let (field, new_pos) = consume_id(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Token::Assign)?;
            let (val, new_pos) = Expression::parse(parser, pos)?;
            if parser.peek(new_pos, Token::Comma) {
                pos = consume_symbol(parser, new_pos, Token::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((field, val));
        }
        pos = consume_symbol(parser, pos, Token::RightCurly)?;
        Ok((Expression::RecordExpr(pairs), pos))
    }

    fn parse_dict_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let mut pos = consume_symbol(parser, pos, Token::HashCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Token::RightCurly) {
            let (key, new_pos) = Expression::parse_primary_expr(parser, pos)?;

            pos = consume_symbol(parser, new_pos, Token::FatArrow)?;
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Token::Comma) {
                pos = consume_symbol(parser, new_pos, Token::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((key, val));
        }
        pos = consume_symbol(parser, pos, Token::RightCurly)?;
        Ok((Expression::DictExpr(pairs), pos))
    }

    fn parse_set_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let mut pos = consume_symbol(parser, pos, Token::DollarCurly)?;
        let mut elems = vec![];
        while !parser.peek(pos, Token::RightCurly) {
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Token::Comma) {
                pos = consume_symbol(parser, new_pos, Token::Comma)?;
            } else {
                pos = new_pos;
            }
            elems.push(val);
        }
        pos = consume_symbol(parser, pos, Token::RightCurly)?;
        Ok((Expression::SetExpr(elems), pos))
    }

    fn parse_lazy_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Lazy)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_yield_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Yield)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::YieldExpr(Box::new(expr)), pos))
    }

    fn parse_not_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Not)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::NotExpr(Box::new(expr)), pos))
    }

    fn parse_literal_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Token::LargeString(index)) => Ok((
                Expression::LargeStringLiteral(parser.get_large_string(index)),
                pos + 1,
            )),
            Some(Token::String(str)) => Ok((Expression::StringLiteral(str), pos + 1)),
            Some(Token::Integer(int)) => Ok((Expression::IntegerLiteral(int), pos + 1)),
            Some(Token::Float(float)) => Ok((Expression::FloatLiteral(float), pos + 1)),
            Some(Token::IsoDate(date)) => Ok((Expression::DateLiteral(date), pos + 1)),
            Some(Token::FormatString(f_str)) => {
                Ok((Expression::FormatString(f_str), pos + 1))
            }
            Some(Token::Char(chr)) => Ok((Expression::CharLiteral(chr), pos + 1)),
            Some(Token::RegExp(expr)) => Ok((Expression::RegexpLiteral(expr), pos + 1)),
            sym => {
                println!("parse literal sym = {:?}", sym);
                todo!()
            }
        }
    }

    fn parse_ctor_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Token::TypeId(tid)) => {
                let type_id = tid.to_string();
                let mut pos = consume_symbol(parser, pos, Token::TypeId(tid))?;
                let mut q_ids = vec![];
                if parser.peek(pos, Token::Dot) {
                    while parser.peek(pos, Token::Dot) {
                        pos = consume_symbol(parser, pos, Token::Dot)?;
                        match parser.get_token(pos) {
                            Some(Token::Id(id)) => {
                                q_ids.push(Expression::Identifier(id))
                            }
                            Some(Token::TypeId(tid)) => {
                                q_ids.push(Expression::TypeIdentifier(tid))
                            }
                            _ => break,
                        }
                        pos += 1;
                    }
                }
                let mut args = vec![];
                while !is_func_call_end_symbol(parser.get_token(pos)) {
                    let (expr, new_pos) = Expression::parse(parser, pos)?;
                    args.push(expr);
                    pos = new_pos;
                }
                Ok((Expression::TypedFuncCall(type_id, q_ids, args), pos))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn parse_recur(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Recur)?;
        let (args, pos) = consume_args(parser, pos)?;
        Ok((Expression::RecurExpr(args), pos))
    }

    pub(crate) fn parse_perform(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Perform)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if parser.peek(pos, Token::With) {
            let pos = consume_symbol(parser, pos, Token::With)?;
            let (context, pos) = Expression::parse_prim_expr(parser, pos)?;
            Ok((
                Expression::PerformExpr(Box::new(expr), None, Some(Box::new(context))),
                pos,
            ))
        } else if !is_func_call_end_symbol(parser.get_token(pos)) {
            let (args, pos) = consume_args(parser, pos)?;
            if parser.peek(pos, Token::With) {
                let pos = consume_symbol(parser, pos, Token::With)?;
                let (context, pos) = Expression::parse_prim_expr(parser, pos)?;
                Ok((
                    Expression::PerformExpr(Box::new(expr), Some(args), Some(Box::new(context))),
                    pos,
                ))
            } else {
                Ok((
                    Expression::PerformExpr(Box::new(expr), Some(args), None),
                    pos,
                ))
            }
        } else {
            Ok((Expression::PerformExpr(Box::new(expr), None, None), pos))
        }
    }

    fn parse_func_call_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if is_func_call_end_symbol(parser.get_token(pos)) {
            Ok((expr, pos))
        } else {
            let (arg, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if is_func_call_end_symbol(parser.get_token(new_pos)) {
                Ok((
                    Expression::FuncCallExpr(Box::new(expr), Box::new(arg)),
                    new_pos,
                ))
            } else {
                let (arg, pos) = Expression::parse_func_call_expr(parser, pos)?;
                Ok((Expression::FuncCallExpr(Box::new(expr), Box::new(arg)), pos))
            }
        }
    }

    fn parse_prim_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Token::LeftParen) => Expression::parse_paren_expr(parser, pos),

            Some(Token::Id(_)) => {
                let (id, mut pos) = consume_id(parser, pos)?;
                let mut fields = vec![];
                while parser.peek(pos, Token::Dot) {
                    pos = consume_symbol(parser, pos, Token::Dot)?;
                    let (id, new_pos) = consume_id(parser, pos)?;
                    fields.push(id);
                    pos = new_pos;
                }
                let (expr, pos) = if fields.is_empty() {
                    (Expression::Identifier(id), pos)
                } else {
                    (Expression::QualifedIdentifier(id, fields), pos)
                };
                if parser.peek(pos, Token::Dollar) {
                    let pos = consume_symbol(parser, pos, Token::Dollar)?;
                    let (arg, pos) = Expression::parse(parser, pos)?;
                    Ok((Expression::FuncCallExpr(Box::new(expr), Box::new(arg)), pos))
                } else {
                    Ok((expr, pos))
                }
            }
            sym if is_func_call_end_symbol(sym) => {
                raise_parser_error("invalid token", parser, pos, true)
            }
            _ => Expression::parse_dollar_func_call_expr(parser, pos),
        }
    }

    fn parse_let(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Let)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut equations = vec![];
        let (equation, pos) = Expression::parse_let_equation(parser, pos)?;
        equations.push(equation);
        let pos = consume_opt_symbol(parser, pos, Token::Comma)?;
        let mut pos = parser.skip_nl(pos);
        while !parser.peek(pos, Token::Dedent) && !parser.peek(pos, Token::In) {
            let (equation, new_pos) = Expression::parse_let_equation(parser, pos)?;
            equations.push(equation);
            pos = consume_opt_symbol(parser, new_pos, Token::Comma)?;
            pos = parser.skip_nl(pos);
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::In)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::LetExpr(equations, Box::new(expr)), pos))
    }

    fn parse_let_equation(parser: &'a Parser<'a>, pos: usize) -> Result<(Equation<'a>, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eq, pos))
    }

    fn parse_loop(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (for_part, pos) = if parser.peek(pos, Token::For) {
            let (eqs, pos) = Expression::parse_for(parser, pos)?;
            (Some(eqs), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let (opt_while_or_until_part, pos) = if parser.peek(pos, Token::While) {
            let (while_part, pos) = Expression::parse_while(parser, pos)?;
            (Some(while_part), pos)
        } else if parser.peek(pos, Token::Until) {
            let (until_part, pos) = Expression::parse_until(parser, pos)?;
            (Some(until_part), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::Loop)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (loop_body, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        if parser.peek(pos, Token::Return) {
            let pos = consume_symbol(parser, pos, Token::Return)?;
            let pos = parser.skip_nl(pos);
            let (indent, pos) = parse_opt_indent(parser, pos);
            let (expr, pos) = Expression::parse(parser, pos)?;
            let pos = parse_opt_dedent(parser, pos, indent)?;
            Ok((
                Expression::LoopExpr(
                    for_part,
                    opt_while_or_until_part,
                    Box::new(loop_body),
                    Some(Box::new(expr)),
                ),
                pos,
            ))
        } else {
            Ok((
                Expression::LoopExpr(for_part, opt_while_or_until_part, Box::new(loop_body), None),
                pos,
            ))
        }
    }

    fn parse_for(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<Equation<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Token::For)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        let (eq, mut pos) = Equation::parse_back_arrow_or_assign_eq(parser, pos)?;
        eqs.push(eq);
        while parser.peek(pos, Token::NewLine) || parser.peek(pos, Token::Comma) {
            pos = parser.skip_nl(pos + 1);
            if matches!(
                parser.get_token(pos),
                Some(Token::Loop) | Some(Token::While) | Some(Token::Until) | Some(Token::Dedent)
            ) {
                break;
            }
            let (eq, new_pos) = Equation::parse_back_arrow_or_assign_eq(parser, pos)?;
            eqs.push(eq);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eqs, pos))
    }

    fn parse_while(parser: &'a Parser<'a>, pos: usize) -> Result<(LoopCond<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Token::While)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::WhileExpr(Box::new(expr)), pos))
    }

    fn parse_until(parser: &'a Parser<'a>, pos: usize) -> Result<(LoopCond<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Token::Until)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::UntilExpr(Box::new(expr)), pos))
    }

    fn parse_repeat(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Repeat)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, mut pos) = Expression::parse_recur_expr(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut exprs = vec![];
        exprs.push(expr);
        while parser.peek(pos, Token::Comma) {
            pos = consume_symbol(parser, pos, Token::Comma)?;
            pos = parser.skip_nl(pos);
            let (in_indent, new_pos) = parse_opt_indent(parser, pos);
            let (expr, new_pos) = Expression::parse_recur_expr(parser, new_pos)?;
            pos = parser.skip_nl(new_pos);
            pos = parse_opt_dedent(parser, pos, in_indent)?;
            pos = parser.skip_nl(pos);
            exprs.push(expr);
        }
        pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::RepeatExpr(exprs), pos))
    }

    fn parse_recur_expr(parser: &'a Parser<'a>, pos: usize) -> Result<(RecurValue<'a>, usize)> {
        if parser.peek(pos, Token::Let) {
            let pos = consume_symbol(parser, pos, Token::Let)?;
            let (id, pos) = consume_id(parser, pos)?;
            let pos = consume_symbol(parser, pos, Token::Assign)?;
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Var(id, expr), pos))
        } else {
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Value(expr), pos))
        }
    }

    fn parse_if(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_inner_if(parser, pos, Token::If)
    }

    fn parse_inner_if(parser: &'a Parser<'a>, pos: usize, if_symbol: Token) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, if_symbol)?;
        let (cond, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Then)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (then_expr, pos) = Expression::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        if parser.peek(pos, Token::Else) {
            let pos = consume_symbol(parser, pos, Token::Else)?;
            let pos = parser.skip_nl(pos);
            let (indent, pos) = parse_opt_indent(parser, pos);
            let (else_expr, pos) = Expression::parse(parser, pos)?;
            let pos = parse_opt_dedent(parser, pos, indent)?;
            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(else_expr)),
                pos,
            ))
        } else {
            let (elif_expr, pos) = Expression::parse_inner_if(parser, pos, Token::Elif)?;
            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(elif_expr)),
                pos,
            ))
        }
    }

    fn parse_do(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Do)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut exprs = vec![];
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let mut pos = parser.skip_nl(pos);
        exprs.push(expr);
        while !parser.peek(pos, Token::Dedent) {
            let (expr, new_pos) = Expression::parse(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            exprs.push(expr);
        }
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        Ok((Expression::DoExpr(exprs), pos))
    }

    fn parse_try_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Try)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Token::Handle)?;
        let pos = parser.skip_nl(pos);
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (handles, pos) = Expression::parse_handle_guards(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok((TryHandleExpr(Box::new(expr), handles), pos))
    }

    pub(crate) fn parse_handle_guards(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Vec<HandleGuard<'a>>, usize)> {
        let mut handles = vec![];
        let mut pos = pos;
        while parser.peek(pos, Token::Guard) {
            let (handle, new_pos) = Expression::parse_handle_guard(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            handles.push(handle);
        }
        pos = parser.skip_nl(pos);
        Ok((handles, pos))
    }

    fn parse_handle_guard(parser: &'a Parser<'a>, pos: usize) -> Result<(HandleGuard<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Token::Guard)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        let (ctx, args, pos) = if parser.peek(pos, Token::With) {
            let pos = consume_symbol(parser, pos, Token::With)?;
            let (args, pos) = Expression::parse_args_before_arrow(parser, pos)?;
            (None, if args.is_empty() { None } else { Some(args) }, pos)
        } else if !parser.peek(pos, Token::Arrow) && !parser.peek(pos, Token::NewLine) {
            let (ctx, pos) = Expression::parse(parser, pos)?;
            if parser.peek(pos, Token::With) {
                let pos = consume_symbol(parser, pos, Token::With)?;
                let (args, pos) = Expression::parse_args_before_arrow(parser, pos)?;
                (
                    Some(ctx),
                    if args.is_empty() { None } else { Some(args) },
                    pos,
                )
            } else {
                (Some(ctx), None, pos)
            }
        } else {
            (None, None, pos)
        };
        if parser.peek(pos, Token::Arrow) {
            let pos = consume_symbol(parser, pos, Token::Arrow)?;
            let (resume_expr, pos) = Expression::parse(parser, pos)?;
            let pos = parser.skip_nl(pos);
            Ok((HandleGuard(expr, ctx, args, Some(resume_expr)), pos))
        } else {
            let pos = parser.skip_nl(pos);
            Ok((HandleGuard(expr, ctx, args, None), pos))
        }
    }

    fn parse_args_before_arrow(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<&'a str>, usize)> {
        let mut args = vec![];
        let mut pos = pos;
        while !parser.peek(pos, Token::Arrow) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_resume_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Token::Resume)?;
        if is_func_call_end_symbol(parser.get_token(pos)) {
            Ok((Expression::ResumeExpr(None, None), pos))
        } else if parser.peek(pos, Token::With) {
            let pos = consume_symbol(parser, pos, Token::With)?;
            let (args, pos) = consume_args(parser, pos)?;
            Ok((Expression::ResumeExpr(None, Some(args)), pos))
        } else {
            let (new_ctx, pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(pos, Token::With) {
                let pos = consume_symbol(parser, pos, Token::With)?;
                let (args, pos) = consume_args(parser, pos)?;
                Ok((
                    Expression::ResumeExpr(Some(Box::new(new_ctx)), Some(args)),
                    pos,
                ))
            } else {
                Ok((Expression::ResumeExpr(Some(Box::new(new_ctx)), None), pos))
            }
        }
    }
}
