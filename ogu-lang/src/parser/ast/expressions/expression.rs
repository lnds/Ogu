use crate::backend::errors::OguError;
use crate::lexer::tokens::Lexeme;
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
use anyhow::{Error, Result};

#[macro_export]
macro_rules! parse_left_assoc {
    ($func_name:ident, $op:expr, $next_level: expr) => {
        fn $func_name(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
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
        fn $func_name(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
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
pub(crate) enum Identifier<'a> {
    Id(&'a str),
    TypeId(&'a str),
}

#[derive(Debug, Clone)]
pub(crate) enum Expression<'a> {
    InvalidExpr,
    Name(&'a str),
    NameStr(String),
    QualifiedIdentifier(&'a str, Vec<&'a str>),
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
    ParenExpr(Box<Expression<'a>>),
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
    TypedFuncCall(String, Vec<Identifier<'a>>, Vec<Expression<'a>>),
    FuncCallExpr(Box<Expression<'a>>, Vec<Expression<'a>>),
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
    UnaryNot,
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
    CaseExpr(
        Box<Expression<'a>>,
        Vec<(Option<Expression<'a>>, Expression<'a>)>,
    ),
    IfExpr(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    MacroExpandExpr(Box<Expression<'a>>),
    ResumeExpr(Option<Box<Expression<'a>>>, Option<Vec<Expression<'a>>>),
    LoopExpr(
        Option<Vec<Equation<'a>>>,
        Option<LoopCond<'a>>,
        Box<Expression<'a>>,
        Option<Box<Expression<'a>>>,
    ),
    // special
    GuardedExpr(Box<Expression<'a>>, Box<Expression<'a>>), // pattern if expr ->
}

pub(crate) type ParseResult<'a> = Result<(Expression<'a>, usize)>;
type ParseOptExprResult<'a> = Result<(Option<Expression<'a>>, usize)>;
pub(crate) type OptExprTuple<'a> = (Option<Expression<'a>>, Expression<'a>);

#[derive(Debug, Clone)]
pub(crate) enum LambdaArg<'a> {
    Simple(String),
    Tuple(Vec<&'a str>),
}

impl<'a> Expression<'a> {
    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (expr, mut pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
        if parser.peek(pos, Lexeme::SemiColon) {
            let mut exprs = vec![expr];
            while parser.peek(pos, Lexeme::SemiColon) {
                pos = consume_symbol(parser, pos, Lexeme::SemiColon)?;
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
        Lexeme::PipeRight,
        Expression::parse_backpipe_func_call_expr
    );

    parse_right_assoc!(
        parse_backpipe_func_call_expr,
        Lexeme::PipeLeft,
        Expression::parse_control_expr
    );

    pub(crate) fn parse_control_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            None => raise_parser_error("Expecting an expression but found EOF", parser, pos, false),
            Some(Lexeme::Cond) => Expression::parse_cond(parser, pos),
            Some(Lexeme::Case) => Expression::parse_case(parser, pos),
            Some(Lexeme::Do) => Expression::parse_do(parser, pos),
            Some(Lexeme::Try) => Expression::parse_try_expr(parser, pos),
            Some(Lexeme::Let) => Expression::parse_let(parser, pos),
            Some(Lexeme::For) => Expression::parse_loop(parser, pos),
            Some(Lexeme::Loop) => Expression::parse_loop(parser, pos),
            Some(Lexeme::If) => Expression::parse_if(parser, pos),
            Some(Lexeme::Repeat) => Expression::parse_repeat(parser, pos),
            Some(Lexeme::Recur) => Expression::parse_recur(parser, pos),
            Some(Lexeme::Resume) => Expression::parse_resume_expr(parser, pos),
            Some(Lexeme::Reify) => Expression::parse_reify(parser, pos),
            _ => Expression::parse_lambda_expr(parser, pos),
        }
    }

    pub(crate) fn parse_cond(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Cond)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let (conds, pos) = Expression::parse_indented_vec_of_pairs_cond_expr(
            parser,
            pos,
            Expression::parse_opt_otherwise,
        )?;
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok((Expression::if_from(&conds)?, pos))
    }

    fn parse_opt_otherwise(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Option<Expression<'a>>, usize)> {
        if parser.peek(pos, Lexeme::Otherwise) {
            Ok((None, consume_symbol(parser, pos, Lexeme::Otherwise)?))
        } else {
            let (c, p) = Expression::parse_logical_expr(parser, pos)?;
            Ok((Some(c), p))
        }
    }

    fn parse_opt_pattern(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Option<Expression<'a>>, usize)> {
        if parser.peek(pos, Lexeme::Id("_")) {
            Ok((None, pos + 1))
        } else {
            let (c, pos) = Expression::parse_primary_expr(parser, pos)?;
            if matches!(c, Expression::StringLiteral(_) | Expression::RegexpLiteral(_)
            | Expression::CharLiteral(_) | Expression::IntegerLiteral(_)|Expression::FloatLiteral(_)
            | Expression::DateLiteral(_) | Expression::Unit | Expression::Name(_)
            | Expression::EmptyList | Expression::ListExpr(_)
            | Expression::ConsExpr(_,_) | Expression::TupleExpr(_)| Expression::TypedFuncCall(_,_,_)
            | Expression::RangeExpr(_,_) | Expression::RangeOpenExpr(_,_)
            | Expression::RangeInfExpr(_) | Expression::DictExpr(_) | Expression::SetExpr(_)
            ) {
                if parser.peek(pos, Lexeme::Guard) {
                    let pos = consume_symbol(parser, pos, Lexeme::Guard)?;
                    let (if_expr, pos) = Expression::parse_logical_expr(parser, pos)?;
                    Ok((
                        Some(Expression::GuardedExpr(Box::new(c), Box::new(if_expr))),
                        pos,
                    ))
                } else {
                    Ok((Some(c), pos))
                }
            } else {
                Err(Error::new(OguError::SemanticError)
                    .context(format!("expression: {:?} must be a pattern in a case", c)))
            }
        }
    }

    pub(crate) fn parse_case(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Case)?;
        let (match_expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Of)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let (matches, pos) = Expression::parse_indented_vec_of_pairs_cond_expr(
            parser,
            pos,
            Expression::parse_opt_pattern,
        )?;
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok((Expression::CaseExpr(Box::new(match_expr), matches), pos))
    }

    fn parse_indented_vec_of_pairs_cond_expr(
        parser: &'a Parser<'a>,
        pos: usize,
        cond_parser: fn(&'a Parser<'a>, usize) -> ParseOptExprResult,
    ) -> Result<(Vec<OptExprTuple<'a>>, usize)> {
        let mut result = vec![];
        let mut pos = pos;
        while !parser.peek(pos, Lexeme::Dedent) {
            let (cond, new_pos) = cond_parser(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Lexeme::Arrow)?;
            let (in_indent, new_pos) = parse_opt_indent(parser, pos);
            let (value, new_pos) = Expression::parse(parser, new_pos)?;
            pos = parse_opt_dedent(parser, new_pos, in_indent)?;
            pos = parser.skip_nl(pos);
            if cond.is_none() {
                result.push((None, value));
                break;
            } else {
                result.push((cond, value));
            }
        }
        Ok((result, pos))
    }

    pub(crate) fn parse_reify(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Reify)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let (eq, mut pos) = Equation::parse(parser, pos, true)?;
        let mut eqs = vec![eq];
        pos = parser.skip_nl(pos);
        while !parser.peek(pos, Lexeme::Dedent) {
            let (eq, new_pos) = Equation::parse(parser, pos, true)?;
            eqs.push(eq);
            pos = parser.skip_nl(new_pos);
        }
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::ReifyExpr(type_id, eqs), pos))
    }

    pub(crate) fn parse_lambda_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        if !parser.peek(pos, Lexeme::Lambda) {
            Expression::parse_logical_expr(parser, pos)
        } else {
            let pos = consume_symbol(parser, pos, Lexeme::Lambda)?;
            let (args, pos) = Expression::parse_lambda_args(parser, pos)?;
            let pos = consume_symbol(parser, pos, Lexeme::Arrow)?;
            let pos = parser.skip_nl(pos); // skip arrow
            let (expr, pos) = Expression::parse_control_expr(parser, pos)?;
            Ok((Expression::LambdaExpr(args, Box::new(expr)), pos))
        }
    }

    fn parse_lambda_args(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Vec<LambdaArg<'a>>, usize)> {
        let mut args = vec![];
        let (arg, mut pos) = Expression::parse_lambda_arg(parser, pos)?;
        args.push(arg);
        while !parser.peek(pos, Lexeme::Arrow) {
            let (arg, new_pos) = Expression::parse_lambda_arg(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_lambda_arg(parser: &'a Parser<'a>, pos: usize) -> Result<(LambdaArg<'a>, usize)> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftParen) => {
                let (ids, pos) = consume_ids_sep_by(parser, pos + 1, Lexeme::Comma)?;
                let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                Ok((LambdaArg::Tuple(ids), pos))
            }
            Some(Lexeme::Id(id)) => Ok((LambdaArg::Simple(id.to_string()), pos + 1)),
            _ => raise_parser_error("Expecting lambda arg", parser, pos, true),
        }
    }

    pub(crate) fn parse_logical_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_logical_or_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_logical_or_expr,
        Lexeme::Or,
        Expression::parse_logical_and_expr
    );

    parse_left_assoc!(
        parse_logical_and_expr,
        Lexeme::And,
        Expression::parse_comparative_expr
    );

    fn parse_comparative_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_lt_expr(parser, pos)
    }

    parse_left_assoc!(parse_lt_expr, Lexeme::LessThan, Expression::parse_le_expr);

    parse_left_assoc!(
        parse_le_expr,
        Lexeme::LessThanOrEqual,
        Expression::parse_gt_expr
    );

    parse_left_assoc!(parse_gt_expr, Lexeme::Greater, Expression::parse_ge_expr);

    parse_left_assoc!(
        parse_ge_expr,
        Lexeme::GreaterOrEqual,
        Expression::parse_eq_expr
    );

    parse_left_assoc!(parse_eq_expr, Lexeme::Equal, Expression::parse_ne_expr);

    parse_left_assoc!(
        parse_ne_expr,
        Lexeme::NotEqual,
        Expression::parse_regex_expr
    );

    fn parse_regex_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_matches_expr(parser, pos)
    }

    parse_left_assoc!(
        parse_matches_expr,
        Lexeme::Matches,
        Expression::parse_nomatch_expr
    );

    parse_left_assoc!(
        parse_nomatch_expr,
        Lexeme::NotMatches,
        Expression::parse_rematch_expr
    );

    parse_left_assoc!(
        parse_rematch_expr,
        Lexeme::Match,
        Expression::parse_cons_expr
    );

    parse_right_assoc!(parse_cons_expr, Lexeme::Cons, Expression::parse_add_expr);

    parse_left_assoc!(parse_add_expr, Lexeme::Plus, Expression::parse_sub_expr);

    parse_left_assoc!(parse_sub_expr, Lexeme::Minus, Expression::parse_concat_expr);

    parse_left_assoc!(
        parse_concat_expr,
        Lexeme::PlusPlus,
        Expression::parse_mult_expr
    );

    parse_left_assoc!(parse_mult_expr, Lexeme::Mult, Expression::parse_div_expr);

    parse_left_assoc!(parse_div_expr, Lexeme::Div, Expression::parse_int_div_expr);

    parse_left_assoc!(
        parse_int_div_expr,
        Lexeme::DivDiv,
        Expression::parse_mod_expr
    );

    parse_left_assoc!(parse_mod_expr, Lexeme::Mod, Expression::parse_pow_expr);

    parse_right_assoc!(
        parse_pow_expr,
        Lexeme::Pow,
        Expression::parse_compose_fwd_expr
    );

    parse_right_assoc!(
        parse_compose_fwd_expr,
        Lexeme::ComposeForward,
        Expression::parse_compose_bck_expr
    );

    parse_left_assoc!(
        parse_compose_bck_expr,
        Lexeme::ComposeBackward,
        Expression::parse_postfix_expr
    );

    parse_left_assoc!(
        parse_postfix_expr,
        Lexeme::Arroba,
        Expression::parse_primary_expr
    );

    pub(crate) fn parse_primary_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftBracket) => Expression::parse_list_expr(parser, pos),
            Some(Lexeme::LeftCurly) => Expression::parse_record_expr(parser, pos),
            Some(Lexeme::LeftCurlyCurly) => Expression::parse_macro_expand_expr(parser, pos),
            Some(Lexeme::HashCurly) => Expression::parse_dict_expr(parser, pos),
            Some(Lexeme::DollarCurly) => Expression::parse_set_expr(parser, pos),
            Some(Lexeme::Lazy) => Expression::parse_lazy_expr(parser, pos),
            Some(Lexeme::Yield) => Expression::parse_yield_expr(parser, pos),
            Some(Lexeme::Perform) => Expression::parse_perform(parser, pos),
            Some(Lexeme::Not) => Expression::parse_not_expr(parser, pos),
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            Some(Lexeme::TypeId(_)) => Expression::parse_ctor_expr(parser, pos),
            _ => Expression::parse_func_call_expr(parser, pos),
        }
    }

    fn parse_macro_expand_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::LeftCurlyCurly)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::RightCurlyCurly)?;
        Ok((Expression::MacroExpandExpr(Box::new(expr)), pos))
    }

    fn parse_paren_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
        match parser.get_token(pos) {
            Some(Lexeme::RightParen) => Ok((Expression::Unit, pos + 1)),
            Some(Lexeme::Not) if parser.peek(pos + 1, Lexeme::RightParen) => {
                Ok((Expression::UnaryNot, pos + 2))
            }
            Some(op) if is_basic_op(op) => {
                let (opt_expr, pos) = if parser.peek(pos + 1, Lexeme::RightParen) {
                    (None, pos + 1)
                } else {
                    let (expr, pos) = Expression::parse_lambda_expr(parser, pos + 1)?;
                    (Some(Box::new(expr)), pos)
                };
                let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                match op {
                    Lexeme::Cons => Ok((Expression::UnaryCons(opt_expr), pos)),
                    Lexeme::Plus => Ok((Expression::UnaryAdd(opt_expr), pos)),
                    Lexeme::PlusPlus => Ok((Expression::UnaryConcat(opt_expr), pos)),
                    Lexeme::Minus => Ok((Expression::UnarySub(opt_expr), pos)),
                    Lexeme::Mult => Ok((Expression::UnaryMul(opt_expr), pos)),
                    Lexeme::Pow => Ok((Expression::UnaryPow(opt_expr), pos)),
                    Lexeme::Div => Ok((Expression::UnaryDiv(opt_expr), pos)),
                    Lexeme::Mod => Ok((Expression::UnaryMod(opt_expr), pos)),
                    Lexeme::And => Ok((Expression::UnaryAnd(opt_expr), pos)),
                    Lexeme::Or => Ok((Expression::UnaryOr(opt_expr), pos)),
                    Lexeme::Equal => Ok((Expression::UnaryEq(opt_expr), pos)),
                    Lexeme::NotEqual => Ok((Expression::UnaryNotEq(opt_expr), pos)),
                    Lexeme::Greater => Ok((Expression::UnaryGt(opt_expr), pos)),
                    Lexeme::GreaterOrEqual => Ok((Expression::UnaryGe(opt_expr), pos)),
                    Lexeme::LessThan => Ok((Expression::UnaryLt(opt_expr), pos)),
                    Lexeme::LessThanOrEqual => Ok((Expression::UnaryLe(opt_expr), pos)),
                    _ => raise_parser_error("Expecting an operator", parser, pos, true),
                }
            }
            Some(_) => {
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                if parser.peek(pos, Lexeme::RightParen) {
                    match expr {
                        Expression::Name(_)
                        | Expression::FuncCallExpr(_, _)
                        | Expression::RecurExpr(_)
                        | Expression::LambdaExpr(_, _)
                        | Expression::QualifiedIdentifier(_, _) => {
                            Ok((Expression::ParenExpr(Box::new(expr)), pos + 1))
                        } //
                        _ => Ok((expr, pos + 1)),
                    }
                } else if parser.peek(pos, Lexeme::Comma) {
                    let mut exprs = vec![expr];
                    let mut pos = pos;
                    while parser.peek(pos, Lexeme::Comma) {
                        pos = consume_symbol(parser, pos, Lexeme::Comma)?;
                        pos = parser.skip_nl(pos);
                        let (expr, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                        pos = new_pos;
                        exprs.push(expr);
                    }
                    pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    Ok((Expression::TupleExpr(exprs), pos))
                } else {
                    let (arg, pos) = Expression::parse_prim_expr(parser, pos)?;
                    let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    let f_call = Expression::FuncCallExpr(Box::new(expr), vec![arg]);
                    Ok((Expression::ParenExpr(Box::new(f_call)), pos))
                }
            }
            None => raise_parser_error("unexpected EOF", parser, pos, false),
        }
    }

    fn parse_list_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::LeftBracket)?;
        match parser.get_token(pos) {
            Some(Lexeme::RightBracket) => Ok((Expression::EmptyList, pos + 1)),
            Some(_) => {
                let (exprs, pos) = consume_exprs_sep_by(parser, pos, Lexeme::Comma)?;
                if parser.peek(pos, Lexeme::RightBracket) {
                    Ok((Expression::ListExpr(exprs), pos + 1))
                } else if parser.peek(pos, Lexeme::DotDotDot) {
                    let pos = consume_symbol(parser, pos + 1, Lexeme::RightBracket)?;
                    Ok((Expression::RangeInfExpr(exprs), pos))
                } else if parser.peek(pos, Lexeme::DotDot) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Lexeme::RightBracket) {
                        return Ok((Expression::RangeExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Lexeme::DotDotLess) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Lexeme::RightBracket) {
                        return Ok((Expression::RangeOpenExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Lexeme::Guard) {
                    let pos = consume_symbol(parser, pos, Lexeme::Guard)?;
                    let (eq, mut pos) = Equation::parse_back_arrow_eq(parser, pos)?;
                    let mut eqs = vec![];
                    let mut guards = vec![];
                    let mut lets = vec![];
                    eqs.push(eq);
                    while parser.peek(pos, Lexeme::Comma) {
                        pos = consume_symbol(parser, pos, Lexeme::Comma)?;
                        match Equation::parse_back_arrow_eq(parser, pos) {
                            Ok((eq, new_pos)) => {
                                eqs.push(eq);
                                pos = new_pos;
                            }
                            Err(_) => {
                                if parser.peek(pos, Lexeme::Let) {
                                    pos = consume_symbol(parser, pos, Lexeme::Let)?;
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
                    pos = consume_symbol(parser, pos, Lexeme::RightBracket)?;
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
        let mut pos = consume_symbol(parser, pos, Lexeme::LeftCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Lexeme::RightCurly) {
            let (field, new_pos) = consume_id(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Lexeme::Assign)?;
            let (val, new_pos) = Expression::parse(parser, pos)?;
            if parser.peek(new_pos, Lexeme::Comma) {
                pos = consume_symbol(parser, new_pos, Lexeme::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((field, val));
        }
        pos = consume_symbol(parser, pos, Lexeme::RightCurly)?;
        Ok((Expression::RecordExpr(pairs), pos))
    }

    fn parse_dict_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let mut pos = consume_symbol(parser, pos, Lexeme::HashCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Lexeme::RightCurly) {
            let (key, new_pos) = Expression::parse_primary_expr(parser, pos)?;

            pos = consume_symbol(parser, new_pos, Lexeme::FatArrow)?;
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Lexeme::Comma) {
                pos = consume_symbol(parser, new_pos, Lexeme::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((key, val));
        }
        pos = consume_symbol(parser, pos, Lexeme::RightCurly)?;
        Ok((Expression::DictExpr(pairs), pos))
    }

    fn parse_set_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let mut pos = consume_symbol(parser, pos, Lexeme::DollarCurly)?;
        let mut elems = vec![];
        while !parser.peek(pos, Lexeme::RightCurly) {
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Lexeme::Comma) {
                pos = consume_symbol(parser, new_pos, Lexeme::Comma)?;
            } else {
                pos = new_pos;
            }
            elems.push(val);
        }
        pos = consume_symbol(parser, pos, Lexeme::RightCurly)?;
        Ok((Expression::SetExpr(elems), pos))
    }

    fn parse_lazy_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Lazy)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_yield_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Yield)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::YieldExpr(Box::new(expr)), pos))
    }

    fn parse_not_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Not)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::NotExpr(Box::new(expr)), pos))
    }

    fn parse_literal_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::LargeString(index)) => Ok((
                Expression::LargeStringLiteral(parser.get_large_string(index)),
                pos + 1,
            )),
            Some(Lexeme::String(str)) => Ok((Expression::StringLiteral(str), pos + 1)),
            Some(Lexeme::Integer(int)) => Ok((Expression::IntegerLiteral(int), pos + 1)),
            Some(Lexeme::Float(float)) => Ok((Expression::FloatLiteral(float), pos + 1)),
            Some(Lexeme::IsoDate(date)) => Ok((Expression::DateLiteral(date), pos + 1)),
            Some(Lexeme::FormatString(f_str)) => Ok((Expression::FormatString(f_str), pos + 1)),
            Some(Lexeme::Char(chr)) => Ok((Expression::CharLiteral(chr), pos + 1)),
            Some(Lexeme::RegExp(expr)) => Ok((Expression::RegexpLiteral(expr), pos + 1)),
            sym => {
                println!("parse literal sym = {:?}", sym);
                todo!()
            }
        }
    }

    fn parse_ctor_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::TypeId(tid)) => {
                let type_id = tid.to_string();
                let mut pos = consume_symbol(parser, pos, Lexeme::TypeId(tid))?;
                let mut q_ids = vec![];
                if parser.peek(pos, Lexeme::Dot) {
                    while parser.peek(pos, Lexeme::Dot) {
                        pos = consume_symbol(parser, pos, Lexeme::Dot)?;
                        match parser.get_token(pos) {
                            Some(Lexeme::Id(id)) => q_ids.push(Identifier::Id(id)),
                            Some(Lexeme::TypeId(tid)) => q_ids.push(Identifier::TypeId(tid)),
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
        let pos = consume_symbol(parser, pos, Lexeme::Recur)?;
        let (args, pos) = consume_args(parser, pos)?;
        Ok((Expression::RecurExpr(args), pos))
    }

    pub(crate) fn parse_perform(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Perform)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if parser.peek(pos, Lexeme::With) {
            let pos = consume_symbol(parser, pos, Lexeme::With)?;
            let (context, pos) = Expression::parse_prim_expr(parser, pos)?;
            Ok((
                Expression::PerformExpr(Box::new(expr), None, Some(Box::new(context))),
                pos,
            ))
        } else if !is_func_call_end_symbol(parser.get_token(pos)) {
            let (args, pos) = consume_args(parser, pos)?;
            if parser.peek(pos, Lexeme::With) {
                let pos = consume_symbol(parser, pos, Lexeme::With)?;
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
            let mut args = vec![];
            let mut pos = pos;
            while !is_func_call_end_symbol(parser.get_token(pos)) {
                let (arg, new_pos) = Expression::parse_prim_expr(parser, pos)?;
                args.push(arg);
                pos = new_pos;
            }
            Ok((Expression::FuncCallExpr(Box::new(expr), args), pos))
        }
    }

    fn parse_dollar_func_call_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
        if !parser.peek(pos, Lexeme::Dollar) {
            Ok((expr, pos))
        } else {
            let mut pos = consume_symbol(parser, pos, Lexeme::Dollar)?;
            let mut args = vec![];
            while !is_func_call_end_symbol(parser.get_token(pos)) {
                let (arg, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                args.push(arg);
                pos = new_pos;
            }
            Ok((Expression::FuncCallExpr(Box::new(expr), args), pos))
        }
    }

    fn parse_prim_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftParen) => Expression::parse_paren_expr(parser, pos),

            Some(Lexeme::Id(_)) => {
                let (id, mut pos) = consume_id(parser, pos)?;
                let mut fields = vec![];
                while parser.peek(pos, Lexeme::Dot) {
                    pos = consume_symbol(parser, pos, Lexeme::Dot)?;
                    let (id, new_pos) = consume_id(parser, pos)?;
                    fields.push(id);
                    pos = new_pos;
                }
                let (expr, pos) = if fields.is_empty() {
                    (Expression::Name(id), pos)
                } else {
                    (Expression::QualifiedIdentifier(id, fields), pos)
                };
                if parser.peek(pos, Lexeme::Dollar) {
                    let mut pos = consume_symbol(parser, pos, Lexeme::Dollar)?;
                    let mut args = vec![];
                    while !is_func_call_end_symbol(parser.get_token(pos)) {
                        let (arg, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                        args.push(arg);
                        pos = new_pos;
                    }
                    Ok((Expression::FuncCallExpr(Box::new(expr), args), pos))
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
        let pos = consume_symbol(parser, pos, Lexeme::Let)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut equations = vec![];
        let (equation, pos) = Expression::parse_let_equation(parser, pos)?;
        equations.push(equation);
        let pos = consume_opt_symbol(parser, pos, Lexeme::Comma)?;
        let mut pos = parser.skip_nl(pos);
        while !parser.peek(pos, Lexeme::Dedent) && !parser.peek(pos, Lexeme::In) {
            let (equation, new_pos) = Expression::parse_let_equation(parser, pos)?;
            equations.push(equation);
            pos = consume_opt_symbol(parser, new_pos, Lexeme::Comma)?;
            pos = parser.skip_nl(pos);
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::In)?;
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
        let (for_part, pos) = if parser.peek(pos, Lexeme::For) {
            let (eqs, pos) = Expression::parse_for(parser, pos)?;
            (Some(eqs), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let (opt_while_or_until_part, pos) = if parser.peek(pos, Lexeme::While) {
            let (while_part, pos) = Expression::parse_while(parser, pos)?;
            (Some(while_part), pos)
        } else if parser.peek(pos, Lexeme::Until) {
            let (until_part, pos) = Expression::parse_until(parser, pos)?;
            (Some(until_part), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Loop)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (loop_body, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        if parser.peek(pos, Lexeme::Return) {
            let pos = consume_symbol(parser, pos, Lexeme::Return)?;
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
        let pos = consume_symbol(parser, pos, Lexeme::For)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        let (eq, mut pos) = Equation::parse_back_arrow_or_assign_eq(parser, pos)?;
        eqs.push(eq);
        while parser.peek(pos, Lexeme::NewLine) || parser.peek(pos, Lexeme::Comma) {
            pos = parser.skip_nl(pos + 1);
            if matches!(
                parser.get_token(pos),
                Some(Lexeme::Loop)
                    | Some(Lexeme::While)
                    | Some(Lexeme::Until)
                    | Some(Lexeme::Dedent)
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
        let pos = consume_symbol(parser, pos, Lexeme::While)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::WhileExpr(Box::new(expr)), pos))
    }

    fn parse_until(parser: &'a Parser<'a>, pos: usize) -> Result<(LoopCond<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Until)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::UntilExpr(Box::new(expr)), pos))
    }

    fn parse_repeat(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Repeat)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, mut pos) = Expression::parse_recur_expr(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut exprs = vec![];
        exprs.push(expr);
        while parser.peek(pos, Lexeme::Comma) {
            pos = consume_symbol(parser, pos, Lexeme::Comma)?;
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
        if parser.peek(pos, Lexeme::Let) {
            let pos = consume_symbol(parser, pos, Lexeme::Let)?;
            let (id, pos) = consume_id(parser, pos)?;
            let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Var(id, expr), pos))
        } else {
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Value(expr), pos))
        }
    }

    fn parse_if(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        Expression::parse_inner_if(parser, pos, Lexeme::If)
    }

    fn parse_inner_if(parser: &'a Parser<'a>, pos: usize, if_symbol: Lexeme) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, if_symbol)?;
        let (cond, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Then)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (then_expr, pos) = Expression::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        let (outer_indent, pos) = parse_opt_indent(parser, pos);
        if parser.peek(pos, Lexeme::Else) {
            let pos = consume_symbol(parser, pos, Lexeme::Else)?;
            let pos = parser.skip_nl(pos);
            let (indent, pos) = parse_opt_indent(parser, pos);
            let (else_expr, pos) = Expression::parse(parser, pos)?;
            let pos = parse_opt_dedent(parser, pos, indent)?;
            let pos = parse_opt_dedent(parser, pos, outer_indent)?;

            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(else_expr)),
                pos,
            ))
        } else {
            println!("NEXT TOKE = {:?}", parser.get_token(pos));
            let (indent, pos) = parse_opt_indent(parser, pos);
            let (elif_expr, pos) = Expression::parse_inner_if(parser, pos, Lexeme::Elif)?;
            let pos = parse_opt_dedent(parser, pos, indent)?;
            let pos = parse_opt_dedent(parser, pos, outer_indent)?;

            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(elif_expr)),
                pos,
            ))
        }
    }

    fn parse_do(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Do)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let mut exprs = vec![];
        let pos = parser.skip_nl(pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let mut pos = parser.skip_nl(pos);
        exprs.push(expr);
        while !parser.peek(pos, Lexeme::Dedent) {
            let (expr, new_pos) = Expression::parse(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            exprs.push(expr);
        }
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok((Expression::DoExpr(exprs), pos))
    }

    fn parse_try_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Try)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Lexeme::Handle)?;
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
        while parser.peek(pos, Lexeme::Guard) {
            let (handle, new_pos) = Expression::parse_handle_guard(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            handles.push(handle);
        }
        pos = parser.skip_nl(pos);
        Ok((handles, pos))
    }

    fn parse_handle_guard(parser: &'a Parser<'a>, pos: usize) -> Result<(HandleGuard<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Guard)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        let (ctx, args, pos) = if parser.peek(pos, Lexeme::With) {
            let pos = consume_symbol(parser, pos, Lexeme::With)?;
            let (args, pos) = Expression::parse_args_before_arrow(parser, pos)?;
            (None, if args.is_empty() { None } else { Some(args) }, pos)
        } else if !parser.peek(pos, Lexeme::Arrow) && !parser.peek(pos, Lexeme::NewLine) {
            let (ctx, pos) = Expression::parse(parser, pos)?;
            if parser.peek(pos, Lexeme::With) {
                let pos = consume_symbol(parser, pos, Lexeme::With)?;
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
        if parser.peek(pos, Lexeme::Arrow) {
            let pos = consume_symbol(parser, pos, Lexeme::Arrow)?;
            let (resume_expr, pos) = Expression::parse(parser, pos)?;
            let pos = parser.skip_nl(pos);
            Ok((HandleGuard(expr, ctx, args, Some(resume_expr)), pos))
        } else {
            let pos = parser.skip_nl(pos);
            Ok((HandleGuard(expr, ctx, args, None), pos))
        }
    }

    fn parse_args_before_arrow(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Vec<&'a str>, usize)> {
        let mut args = vec![];
        let mut pos = pos;
        while !parser.peek(pos, Lexeme::Arrow) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_resume_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Resume)?;
        if is_func_call_end_symbol(parser.get_token(pos)) {
            Ok((Expression::ResumeExpr(None, None), pos))
        } else if parser.peek(pos, Lexeme::With) {
            let pos = consume_symbol(parser, pos, Lexeme::With)?;
            let (args, pos) = consume_args(parser, pos)?;
            Ok((Expression::ResumeExpr(None, Some(args)), pos))
        } else {
            let (new_ctx, pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(pos, Lexeme::With) {
                let pos = consume_symbol(parser, pos, Lexeme::With)?;
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

impl<'a> Expression<'a> {
    pub(crate) fn if_from(
        pairs: &[(Option<Expression<'a>>, Expression<'a>)],
    ) -> Result<Expression<'a>> {
        if pairs.is_empty() {
            return Err(Error::new(OguError::ParserError).context("empty conditional"));
        }
        if pairs.len() == 1 {
            let (cond, expr) = &pairs[0];
            match cond {
                None => Ok(expr.clone()),
                Some(c) => Ok(Expression::IfExpr(
                    Box::new(c.clone()),
                    Box::new(expr.clone()),
                    Box::new(Expression::InvalidExpr),
                )),
            }
        } else {
            // len > 1
            let (cond, expr) = &pairs[0];
            let rest = &pairs[1..];
            match cond {
                None => Err(Error::new(OguError::ParserError)
                    .context("invalid otherwise before other conditions")),
                Some(c) => Ok(Expression::IfExpr(
                    Box::new(c.clone()),
                    Box::new(expr.clone()),
                    Box::new(Expression::if_from(rest)?),
                )),
            }
        }
    }
}
