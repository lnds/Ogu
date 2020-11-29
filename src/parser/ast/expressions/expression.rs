use crate::backend::OguError;
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
    ParseError, Parser,
};
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
pub enum RecurValue {
    Value(Expression),
    Var(String, Expression),
}

#[derive(Debug, Clone)]
pub enum LoopCond {
    WhileExpr(Box<Expression>),
    UntilExpr(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct HandleGuard(
    Expression,
    Option<Expression>,
    Option<Vec<String>>,
    Option<Expression>,
);

#[derive(Debug, Clone)]
pub enum Expression {
    Error,
    Identifier(String),
    QualifedIdentifier(String, Vec<String>),
    TypeIdentifier(String),
    StringLiteral(String),
    LargeStringLiteral(Option<String>),
    RegexpLiteral(String),
    CharLiteral(String),
    IntegerLiteral(String),
    FloatLiteral(String),
    DateLiteral(String),
    FormatString(String),
    Unit,
    EmptyList,
    NotExpr(Box<Expression>),
    LazyExpr(Box<Expression>),
    YieldExpr(Box<Expression>),
    ReifyExpr(String, Vec<Equation>),
    ListExpr(Vec<Expression>),
    ListByComprehension(
        Vec<Expression>,
        Vec<Equation>,
        Vec<Expression>,
        Vec<Equation>,
    ),
    RangeExpr(Vec<Expression>, Box<Expression>),
    RangeOpenExpr(Vec<Expression>, Box<Expression>),
    RangeInfExpr(Vec<Expression>),
    // [exprs...]
    DictExpr(Vec<(Expression, Expression)>),
    SetExpr(Vec<Expression>),
    RecordExpr(Vec<(String, Expression)>),
    TypedFuncCall(String, Vec<Expression>, Vec<Expression>),
    FuncCallExpr(Box<Expression>, Box<Expression>),
    LambdaExpr(Vec<LambdaArg>, Box<Expression>),
    MatchesExpr(Box<Expression>, Box<Expression>),
    NoMatchesExpr(Box<Expression>, Box<Expression>),
    ReMatchExpr(Box<Expression>, Box<Expression>),
    ConsExpr(Box<Expression>, Box<Expression>),
    PowExpr(Box<Expression>, Box<Expression>),
    IndexExpr(Box<Expression>, Box<Expression>),
    UnaryCons(Option<Box<Expression>>),
    UnaryAdd(Option<Box<Expression>>),
    UnaryConcat(Option<Box<Expression>>),
    UnarySub(Option<Box<Expression>>),
    UnaryMul(Option<Box<Expression>>),
    UnaryPow(Option<Box<Expression>>),
    UnaryMod(Option<Box<Expression>>),
    UnaryDiv(Option<Box<Expression>>),
    UnaryAnd(Option<Box<Expression>>),
    UnaryOr(Option<Box<Expression>>),
    UnaryNot(Option<Box<Expression>>),
    UnaryEq(Option<Box<Expression>>),
    UnaryNotEq(Option<Box<Expression>>),
    UnaryGt(Option<Box<Expression>>),
    UnaryGe(Option<Box<Expression>>),
    UnaryLt(Option<Box<Expression>>),
    UnaryLe(Option<Box<Expression>>),
    OrExpr(Box<Expression>, Box<Expression>),
    AndExpr(Box<Expression>, Box<Expression>),
    LeExpr(Box<Expression>, Box<Expression>),
    LtExpr(Box<Expression>, Box<Expression>),
    GeExpr(Box<Expression>, Box<Expression>),
    GtExpr(Box<Expression>, Box<Expression>),
    EqExpr(Box<Expression>, Box<Expression>),
    NeExpr(Box<Expression>, Box<Expression>),
    AddExpr(Box<Expression>, Box<Expression>),
    ConcatExpr(Box<Expression>, Box<Expression>),
    SubExpr(Box<Expression>, Box<Expression>),
    MulExpr(Box<Expression>, Box<Expression>),
    DivExpr(Box<Expression>, Box<Expression>),
    IntDivExpr(Box<Expression>, Box<Expression>),
    ModExpr(Box<Expression>, Box<Expression>),
    ComposeFwdExpr(Box<Expression>, Box<Expression>),
    ComposeBckExpr(Box<Expression>, Box<Expression>),
    TupleExpr(Vec<Expression>),
    OpFunc(Box<Token<'static>>),
    DoExpr(Vec<Expression>),
    TryHandleExpr(Box<Expression>, Vec<HandleGuard>),
    RepeatExpr(Vec<RecurValue>),
    RecurExpr(Vec<Expression>),
    PerformExpr(
        Box<Expression>,
        Option<Vec<Expression>>,
        Option<Box<Expression>>,
    ),
    LetExpr(Vec<Equation>, Box<Expression>),
    CondExpr(Vec<(Option<Expression>, Expression)>),
    CaseExpr(Box<Expression>, Vec<(Option<Expression>, Expression)>),
    IfExpr(Box<Expression>, Box<Expression>, Box<Expression>),
    MacroExpandExpr(Box<Expression>),
    ResumeExpr(Option<Box<Expression>>, Option<Vec<Expression>>),
    LoopExpr(
        Option<Vec<Equation>>,
        Option<LoopCond>,
        Box<Expression>,
        Option<Box<Expression>>,
    ),
}

pub type ParseResult = Result<(Expression, usize)>;

#[derive(Debug, Clone)]
pub enum LambdaArg {
    Simple(String),
    Tuple(Vec<String>),
}

impl Expression {
    pub fn parse(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_control_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("Expecting an expression but found eof"),
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

    pub fn parse_cond(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_opt_otherwise(parser: &Parser, pos: usize) -> Result<(Option<Expression>, usize)> {
        if parser.peek(pos, Token::Otherwise) {
            Ok((None, consume_symbol(parser, pos, Token::Otherwise)?))
        } else {
            let (c, p) = Expression::parse_logical_expr(parser, pos)?;
            Ok((Some(c), p))
        }
    }

    pub fn parse_case(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_reify(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_lambda_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_lambda_args(parser: &Parser, pos: usize) -> Result<(Vec<LambdaArg>, usize)> {
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

    fn parse_lambda_arg(parser: &Parser, pos: usize) -> Result<(LambdaArg, usize)> {
        match parser.get_symbol(pos) {
            Some(Token::LeftParen) => {
                let (ids, pos) = consume_ids_sep_by(parser, pos + 1, Token::Comma)?;
                let pos = consume_symbol(parser, pos, Token::RightParen)?;
                Ok((LambdaArg::Tuple(ids), pos))
            }
            Some(Token::Id(id)) => Ok((LambdaArg::Simple(id.to_string()), pos + 1)),
            sym => Err(Error::new(OguError::ParserError(
                ParseError::ExpectingLambdaArg,
            )))
            .context(format!(
                "expecting arg, found: {:?} @{}",
                sym,
                parser.pos_to_line(pos).unwrap_or(0)
            )),
        }
    }

    fn parse_logical_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_comparative_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_regex_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_primary_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
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

    fn parse_macro_expand_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::LeftCurlyCurly)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::RightCurlyCurly)?;
        Ok((Expression::MacroExpandExpr(Box::new(expr)), pos))
    }

    fn parse_paren_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::LeftParen)?;
        match parser.get_symbol(pos) {
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
                    sym => Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingOperator,
                    )))
                    .context(format!("expecting an operator, found {:?}", sym)),
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
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("unexpected eof"),
        }
    }

    fn parse_list_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::LeftBracket)?;
        match parser.get_symbol(pos) {
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
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("unexpected eof"),
        }
    }

    fn parse_record_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_dict_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_set_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_lazy_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Lazy)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_yield_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Yield)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_not_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Not)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::NotExpr(Box::new(expr)), pos))
    }

    fn parse_literal_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Token::LargeString(index)) => Ok((
                Expression::LargeStringLiteral(parser.get_large_string(index)),
                pos + 1,
            )),
            Some(Token::String(str)) => Ok((Expression::StringLiteral(str.to_string()), pos + 1)),
            Some(Token::Integer(int)) => Ok((Expression::IntegerLiteral(int.to_string()), pos + 1)),
            Some(Token::Float(float)) => Ok((Expression::FloatLiteral(float.to_string()), pos + 1)),
            Some(Token::IsoDate(date)) => Ok((Expression::DateLiteral(date.to_string()), pos + 1)),
            Some(Token::FormatString(f_str)) => {
                Ok((Expression::FormatString(f_str.to_string()), pos + 1))
            }
            Some(Token::Char(chr)) => Ok((Expression::CharLiteral(chr.to_string()), pos + 1)),
            Some(Token::RegExp(expr)) => Ok((Expression::RegexpLiteral(expr.to_string()), pos + 1)),
            sym => {
                println!("parse literal sym = {:?}", sym);
                todo!()
            }
        }
    }

    fn parse_ctor_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Token::TypeId(tid)) => {
                let type_id = tid.to_string();
                let mut pos = consume_symbol(parser, pos, Token::TypeId(tid))?;
                let mut q_ids = vec![];
                if parser.peek(pos, Token::Dot) {
                    while parser.peek(pos, Token::Dot) {
                        pos = consume_symbol(parser, pos, Token::Dot)?;
                        match parser.get_symbol(pos) {
                            Some(Token::Id(id)) => {
                                q_ids.push(Expression::Identifier(id.to_string()))
                            }
                            Some(Token::TypeId(tid)) => {
                                q_ids.push(Expression::TypeIdentifier(tid.to_string()))
                            }
                            _ => break,
                        }
                        pos += 1;
                    }
                }
                let mut args = vec![];
                while !is_func_call_end_symbol(parser.get_symbol(pos)) {
                    let (expr, new_pos) = Expression::parse(parser, pos)?;
                    args.push(expr);
                    pos = new_pos;
                }
                Ok((Expression::TypedFuncCall(type_id, q_ids, args), pos))
            }
            _ => todo!(),
        }
    }

    pub fn parse_recur(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Recur)?;
        let (args, pos) = consume_args(parser, pos)?;
        Ok((Expression::RecurExpr(args), pos))
    }

    pub fn parse_perform(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Perform)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if parser.peek(pos, Token::With) {
            let pos = consume_symbol(parser, pos, Token::With)?;
            let (context, pos) = Expression::parse_prim_expr(parser, pos)?;
            Ok((
                Expression::PerformExpr(Box::new(expr), None, Some(Box::new(context))),
                pos,
            ))
        } else if !is_func_call_end_symbol(parser.get_symbol(pos)) {
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

    fn parse_func_call_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if is_func_call_end_symbol(parser.get_symbol(pos)) {
            Ok((expr, pos))
        } else {
            let (arg, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if is_func_call_end_symbol(parser.get_symbol(new_pos)) {
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

    fn parse_prim_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
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

    fn parse_let_equation(parser: &Parser, pos: usize) -> Result<(Equation, usize)> {
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eq, pos))
    }

    fn parse_loop(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_for(parser: &Parser, pos: usize) -> Result<(Vec<Equation>, usize)> {
        let pos = consume_symbol(parser, pos, Token::For)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        let (eq, mut pos) = Equation::parse_back_arrow_or_assign_eq(parser, pos)?;
        eqs.push(eq);
        while parser.peek(pos, Token::NewLine) || parser.peek(pos, Token::Comma) {
            pos = parser.skip_nl(pos + 1);
            if matches!(
                parser.get_symbol(pos),
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

    fn parse_while(parser: &Parser, pos: usize) -> Result<(LoopCond, usize)> {
        let pos = consume_symbol(parser, pos, Token::While)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::WhileExpr(Box::new(expr)), pos))
    }

    fn parse_until(parser: &Parser, pos: usize) -> Result<(LoopCond, usize)> {
        let pos = consume_symbol(parser, pos, Token::Until)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::UntilExpr(Box::new(expr)), pos))
    }

    fn parse_repeat(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_recur_expr(parser: &Parser, pos: usize) -> Result<(RecurValue, usize)> {
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

    fn parse_if(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_inner_if(parser, pos, Token::If)
    }

    fn parse_inner_if(parser: &Parser, pos: usize, if_symbol: Token) -> ParseResult {
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

    fn parse_do(parser: &Parser, pos: usize) -> ParseResult {
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

    fn parse_try_expr(parser: &Parser, pos: usize) -> ParseResult {
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

    pub fn parse_handle_guards(parser: &Parser, pos: usize) -> Result<(Vec<HandleGuard>, usize)> {
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

    fn parse_handle_guard(parser: &Parser, pos: usize) -> Result<(HandleGuard, usize)> {
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

    fn parse_args_before_arrow(parser: &Parser, pos: usize) -> Result<(Vec<String>, usize)> {
        let mut args = vec![];
        let mut pos = pos;
        while !parser.peek(pos, Token::Arrow) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_resume_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Token::Resume)?;
        if is_func_call_end_symbol(parser.get_symbol(pos)) {
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
