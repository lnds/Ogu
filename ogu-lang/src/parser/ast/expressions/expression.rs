use anyhow::{bail, Result};

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression::{FuncCallExpr, TryHandleExpr};
use crate::parser::ast::expressions::{
    consume_args, consume_exprs_sep_by, consume_id, consume_ids_sep_by, is_basic_op,
    is_func_call_end_symbol, is_literal, left_assoc_expr_to_expr, parse_left_assoc_expr,
    parse_right_assoc_expr, right_assoc_expr_to_expr, LeftAssocExpr, RightAssocExpr,
};
use crate::parser::{
    consume_opt_symbol, consume_symbol, consume_type_id, parse_opt_dedent, parse_opt_indent,
    raise_parser_error, Parser,
};

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
    InvalidExpr,     // ok
    Name(&'a str),   // ok
    NameStr(String), // ok
    QualifiedIdentifier(&'a str, Vec<&'a str>),
    StringLiteral(&'a str),             // ok
    LargeStringLiteral(String), // ok
    RegexpLiteral(&'a str),             // ok
    CharLiteral(&'a str),               // ok
    IntegerLiteral(&'a str),            // ok
    FloatLiteral(&'a str),              // ok
    DateLiteral(&'a str),               // ok
    FormatString(&'a str),
    True,                           // ok
    False,                          //ok
    Unit,                           // ok
    EmptyList,                      // ok
    ParenExpr(Box<Expression<'a>>), // ok
    NotExpr(Box<Expression<'a>>),   // ok
    LazyExpr(Box<Expression<'a>>),  // ok
    YieldExpr(Box<Expression<'a>>),
    ReifyExpr(&'a str, Vec<Equation<'a>>),
    ListExpr(Vec<Expression<'a>>), // ok
    ListByComprehension(Box<Expression<'a>>, Vec<ListComprehensionGuard<'a>>), // ok
    RangeExpr(Box<Expression<'a>>, Box<Expression<'a>>), //ok
    RangeExprInfinite(Box<Expression<'a>>, Box<Expression<'a>>), //ok
    RangeExpr3(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ), //ok
    DictExpr(Vec<(Expression<'a>, Expression<'a>)>), // ok
    RecordExpr(Vec<(&'a str, Expression<'a>)>),
    TypedFuncCall(String, Vec<Identifier<'a>>, Vec<Expression<'a>>),
    FuncCallExpr(Box<Expression<'a>>, Vec<Expression<'a>>), // ok
    LambdaExpr(Vec<LambdaArg<'a>>, Box<Expression<'a>>),    // ok
    ConsExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    PowExpr(Box<Expression<'a>>, Box<Expression<'a>>),      // ok
    IndexExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    UnaryCons(Option<Box<Expression<'a>>>),
    UnaryAdd(Option<Box<Expression<'a>>>), // ok
    UnaryConcat(Option<Box<Expression<'a>>>),
    UnarySub(Option<Box<Expression<'a>>>),                // ok
    UnaryMul(Option<Box<Expression<'a>>>),                // ok
    UnaryPow(Option<Box<Expression<'a>>>),                // ok
    UnaryMod(Option<Box<Expression<'a>>>),                // ok
    UnaryDiv(Option<Box<Expression<'a>>>),                // ok
    UnaryDivDiv(Option<Box<Expression<'a>>>),             // ok
    UnaryAnd(Option<Box<Expression<'a>>>),                // ok
    UnaryOr(Option<Box<Expression<'a>>>),                 // ok
    UnaryNot,                                             //ok
    UnaryEq(Option<Box<Expression<'a>>>),                 // ok
    UnaryNotEq(Option<Box<Expression<'a>>>),              // ok
    UnaryGt(Option<Box<Expression<'a>>>),                 // ok
    UnaryGe(Option<Box<Expression<'a>>>),                 // ok
    UnaryLt(Option<Box<Expression<'a>>>),                 // ok
    UnaryLe(Option<Box<Expression<'a>>>),                 // ok
    OrExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    AndExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    LeExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    LtExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    GeExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    GtExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    EqExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    NeExpr(Box<Expression<'a>>, Box<Expression<'a>>),     // ok
    AddExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    ConcatExpr(Box<Expression<'a>>, Box<Expression<'a>>), // ok
    SubExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    MulExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    DivExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    IntDivExpr(Box<Expression<'a>>, Box<Expression<'a>>), // ok
    ModExpr(Box<Expression<'a>>, Box<Expression<'a>>),    // ok
    ComposeFwdExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    ComposeBckExpr(Box<Expression<'a>>, Box<Expression<'a>>),
    TupleExpr(Vec<Expression<'a>>), // ok
    DoExpr(Vec<Expression<'a>>),    // ok
    TryHandleExpr(Box<Expression<'a>>, Vec<HandleGuard<'a>>),
    RepeatExpr(Vec<RecurValue<'a>>), // ok
    RecurExpr(Vec<Expression<'a>>),  // ok
    PerformExpr(
        Box<Expression<'a>>,
        Option<Vec<Expression<'a>>>,
        Option<Box<Expression<'a>>>,
    ),
    LetExpr(Vec<Equation<'a>>, Box<Expression<'a>>), // ok
    CaseExpr(
        Box<Expression<'a>>,
        Vec<(Option<Expression<'a>>, Expression<'a>)>,
    ), // ok
    IfExpr(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ), // ok
    MacroExpandExpr(Box<Expression<'a>>),
    ResumeExpr(Option<Box<Expression<'a>>>, Option<Vec<Expression<'a>>>),
    LoopExpr(Vec<Equation<'a>>, Box<Expression<'a>>), // ok
    // special
    GuardedExpr(Box<Expression<'a>>, Box<Expression<'a>>), // pattern | expr -> // ok
}

#[derive(Debug, Clone)]
pub(crate) enum ListComprehensionGuard<'a> {
    Generator(&'a str, Expression<'a>),
    // id <- expr
    TupleGenerator(Vec<&'a str>, Expression<'a>),
    // (id, id..) <- expr
    Let(&'a str, Expression<'a>),
    // let id = expr
    LetTuple(Vec<&'a str>, Expression<'a>),
    // let id = expr
    Expr(Expression<'a>), // , expr
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
        let (expr, mut pos) = Expression::parse_dollar_func_call_expr(parser, pos)?;
        if parser.peek(pos, Lexeme::SemiColon) {
            let mut exprs = vec![expr];
            while parser.peek(pos, Lexeme::SemiColon) {
                pos = consume_symbol(parser, pos, Lexeme::SemiColon)?;
                pos = parser.skip_nl(pos);
                let (expr, new_pos) = Expression::parse_dollar_func_call_expr(parser, pos)?;
                pos = parser.skip_nl(new_pos);
                exprs.push(expr);
            }
            Ok((Expression::DoExpr(exprs), pos))
        } else {
            Ok((expr, pos))
        }
    }

    fn parse_dollar_func_call_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let (expr, pos) = Expression::parse_control_expr(parser, pos)?;
        if !parser.peek(pos, Lexeme::Dollar) {
            Ok((expr, pos))
        } else {
            let mut pos = consume_symbol(parser, pos, Lexeme::Dollar)?;
            let mut args = vec![];
            while !is_func_call_end_symbol(parser.get_token(pos)) {
                let (arg, new_pos) = Expression::parse_control_expr(parser, pos)?;
                args.push(arg);
                pos = new_pos;
            }
            Ok((Expression::FuncCallExpr(Box::new(expr), args), pos))
        }
    }
    //  Expression::parse_dollar_func_call_expr(parser, pos),

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
            | Expression::True | Expression::False
            | Expression::DateLiteral(_) | Expression::Unit | Expression::Name(_)
            | Expression::EmptyList | Expression::ListExpr(_)
            | Expression::ConsExpr(_,_) | Expression::TupleExpr(_)| Expression::TypedFuncCall(_,_,_)
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
                bail!("expression: {:?} must be a pattern in a case", c)
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

    parse_left_assoc!(parse_ne_expr, Lexeme::NotEqual, Expression::parse_cons_expr);

    parse_right_assoc!(parse_cons_expr, Lexeme::Cons, Expression::parse_add_expr);

    parse_left_assoc!(parse_add_expr, Lexeme::Plus, Expression::parse_sub_expr);

    parse_left_assoc!(parse_sub_expr, Lexeme::Minus, Expression::parse_concat_expr);

    parse_left_assoc!(parse_concat_expr, Lexeme::PlusPlus, Expression::parse_mult_expr);

    parse_left_assoc!(parse_mult_expr, Lexeme::Mult, Expression::parse_div_expr);

    parse_left_assoc!(parse_div_expr, Lexeme::Div, Expression::parse_int_div_expr);

    parse_left_assoc!(parse_int_div_expr, Lexeme::DivDiv, Expression::parse_mod_expr);

    parse_left_assoc!(parse_mod_expr, Lexeme::Mod, Expression::parse_pow_expr);

    parse_right_assoc!(parse_pow_expr, Lexeme::Pow, Expression::parse_postfix_expr);

    parse_left_assoc!(parse_postfix_expr, Lexeme::At, Expression::parse_compose_fwd_expr);

    pub(crate) fn parse_primary_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftBracket) => Expression::parse_list_expr(parser, pos),
            Some(Lexeme::LeftCurly) => Expression::parse_record_expr(parser, pos),
            Some(Lexeme::LeftCurlyCurly) => Expression::parse_macro_expand_expr(parser, pos),
            Some(Lexeme::HashCurly) => Expression::parse_dict_expr(parser, pos),
            Some(Lexeme::Lazy) => Expression::parse_lazy_expr(parser, pos),
            Some(Lexeme::Yield) => Expression::parse_yield_expr(parser, pos),
            Some(Lexeme::Perform) => Expression::parse_perform(parser, pos),
            Some(Lexeme::Not) => Expression::parse_not_expr(parser, pos),
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            Some(Lexeme::TypeId(_)) => Expression::parse_ctor_expr(parser, pos),
            Some(Lexeme::Recur) => Expression::parse_recur(parser, pos),
            _ => Expression::parse_prim_expr(parser, pos),
        }
    }

    parse_right_assoc!(
        parse_compose_fwd_expr,
        Lexeme::ComposeForward,
        Expression::parse_compose_bck_expr
    );

    parse_left_assoc!(
        parse_compose_bck_expr,
        Lexeme::ComposeBackward,
        Expression::parse_pipe_func_call_expr
    );

    parse_left_assoc!(
        parse_pipe_func_call_expr,
        Lexeme::PipeRight,
        Expression::parse_backpipe_func_call_expr
    );
    parse_left_assoc!(
        parse_backpipe_func_call_expr,
        Lexeme::PipeLeft,
        Expression::parse_func_call_expr
    );

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
                let pos = consume_symbol(parser, pos + 1, Lexeme::RightParen)?;
                if is_func_call_end_symbol(parser.get_token(pos)) {
                    Ok((Expression::UnaryNot, pos))
                } else {
                    let (expr, pos) = Expression::parse(parser, pos)?;
                    Ok((Expression::NotExpr(Box::new(expr)), pos))
                }
            }
            Some(Lexeme::Not) => {
                let (expr, pos) = Expression::parse(parser, pos + 1)?;
                let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;

                Ok((Expression::NotExpr(Box::new(expr)), pos))
            }
            Some(op) if is_basic_op(op) => {
                let (opt_expr, pos) = if parser.peek(pos + 1, Lexeme::RightParen) {
                    (None, pos + 1)
                } else {
                    let (expr, pos) = Expression::parse_primary_expr(parser, pos + 1)?;
                    (Some(Box::new(expr)), pos)
                };
                if parser.peek(pos, Lexeme::RightParen) {
                    let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    let expr = match op {
                        Lexeme::Cons => Expression::UnaryCons(opt_expr.clone()),
                        Lexeme::Plus => Expression::UnaryAdd(opt_expr.clone()),
                        Lexeme::PlusPlus => Expression::UnaryConcat(opt_expr.clone()),
                        Lexeme::Minus => Expression::UnarySub(opt_expr.clone()),
                        Lexeme::Mult => Expression::UnaryMul(opt_expr.clone()),
                        Lexeme::Pow => Expression::UnaryPow(opt_expr.clone()),
                        Lexeme::Div => Expression::UnaryDiv(opt_expr.clone()),
                        Lexeme::DivDiv => Expression::UnaryDivDiv(opt_expr.clone()),
                        Lexeme::Mod => Expression::UnaryMod(opt_expr.clone()),
                        Lexeme::And => Expression::UnaryAnd(opt_expr.clone()),
                        Lexeme::Or => Expression::UnaryOr(opt_expr.clone()),
                        Lexeme::Equal => Expression::UnaryEq(opt_expr.clone()),
                        Lexeme::NotEqual => Expression::UnaryNotEq(opt_expr.clone()),
                        Lexeme::Greater => Expression::UnaryGt(opt_expr.clone()),
                        Lexeme::GreaterOrEqual => Expression::UnaryGe(opt_expr.clone()),
                        Lexeme::LessThan => Expression::UnaryLt(opt_expr.clone()),
                        Lexeme::LessThanOrEqual => Expression::UnaryLe(opt_expr.clone()),
                        _ => raise_parser_error("Expecting an operator", parser, pos, true)?,
                    };
                    match opt_expr {
                        Some(_) => Ok((expr, pos)),
                        None => {
                            if is_func_call_end_symbol(parser.get_token(pos)) {
                                return Ok((expr, pos));
                            }
                            let back_pos = pos;
                            let (left_expr, pos) = Expression::parse(parser, pos)?;
                            if is_func_call_end_symbol(parser.get_token(pos)) {
                                // BACK TRACK
                                return match op {
                                    Lexeme::Cons => Ok((Expression::UnaryCons(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Plus => Ok((Expression::UnaryAdd(Some(Box::new(left_expr))), pos)),
                                    Lexeme::PlusPlus => Ok((Expression::UnaryConcat(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Minus => Ok((Expression::UnarySub(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Mult => Ok((Expression::UnaryMul(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Pow => Ok((Expression::UnaryPow(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Div => Ok((Expression::UnaryDiv(Some(Box::new(left_expr))), pos)),
                                    Lexeme::DivDiv => Ok((Expression::UnaryDivDiv(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Mod => Ok((Expression::UnaryMod(Some(Box::new(left_expr))), pos)),
                                Lexeme::And => Ok((Expression::UnaryAnd(Some(Box::new(left_expr))),pos)),
                                    Lexeme::Or => Ok((Expression::UnaryOr(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Equal => Ok((Expression::UnaryEq(Some(Box::new(left_expr))), pos)),
                                    Lexeme::NotEqual => Ok((Expression::UnaryNotEq(Some(Box::new(left_expr))), pos)),
                                    Lexeme::Greater => Ok((Expression::UnaryGt(Some(Box::new(left_expr))), pos)),
                                    Lexeme::GreaterOrEqual => Ok((Expression::UnaryGe(Some(Box::new(left_expr))), pos)),
                                    Lexeme::LessThan => Ok((Expression::UnaryLt(Some(Box::new(left_expr))), pos)),
                                    Lexeme::LessThanOrEqual => Ok((Expression::UnaryLe(Some(Box::new(left_expr))), pos)),
                                    _ => Ok((expr, back_pos))
                                };
                            }
                            let (right_expr, pos) = Expression::parse(parser, pos)?;
                            if !is_func_call_end_symbol(parser.get_token(pos)) {
                                return Ok((expr, back_pos));
                            }
                            let pos = parser.skip_nl(pos);
                            match op {
                                Lexeme::Cons => Ok((
                                    Expression::ConsExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Plus => Ok((
                                    Expression::AddExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::PlusPlus => Ok((
                                    Expression::ConcatExpr(
                                        Box::new(left_expr),
                                        Box::new(right_expr),
                                    ),
                                    pos,
                                )),
                                Lexeme::Minus => Ok((
                                    Expression::SubExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Mult => Ok((
                                    Expression::MulExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Pow => Ok((
                                    Expression::PowExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Div => Ok((
                                    Expression::DivExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::DivDiv => Ok((
                                    Expression::IntDivExpr(
                                        Box::new(left_expr),
                                        Box::new(right_expr),
                                    ),
                                    pos,
                                )),
                                Lexeme::Mod => Ok((
                                    Expression::ModExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::And => Ok((
                                    Expression::AndExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Or => Ok((
                                    Expression::OrExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Equal => Ok((
                                    Expression::EqExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::NotEqual => Ok((
                                    Expression::NeExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::Greater => Ok((
                                    Expression::GtExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::GreaterOrEqual => Ok((
                                    Expression::GeExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::LessThan => Ok((
                                    Expression::LtExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                Lexeme::LessThanOrEqual => Ok((
                                    Expression::LeExpr(Box::new(left_expr), Box::new(right_expr)),
                                    pos,
                                )),
                                _ => {
                                    raise_parser_error("Expecting an operator", parser, pos, true)?
                                }
                            }
                        }
                    }
                } else if let Some(left_expr) = opt_expr {
                    let (right_expr, pos) = Expression::parse_primary_expr(parser, pos)?;
                    let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    match op {
                        Lexeme::Cons => {
                            Ok((Expression::ConsExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Plus => {
                            Ok((Expression::AddExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::PlusPlus => {
                            Ok((Expression::ConcatExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Minus => {
                            Ok((Expression::SubExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Mult => {
                            Ok((Expression::MulExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Pow => {
                            Ok((Expression::PowExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Div => {
                            Ok((Expression::DivExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::DivDiv => {
                            Ok((Expression::IntDivExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Mod => {
                            Ok((Expression::ModExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::And => {
                            Ok((Expression::AndExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Or => {
                            Ok((Expression::OrExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Equal => {
                            Ok((Expression::EqExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::NotEqual => {
                            Ok((Expression::NeExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::Greater => {
                            Ok((Expression::GtExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::GreaterOrEqual => {
                            Ok((Expression::GeExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::LessThan => {
                            Ok((Expression::LtExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        Lexeme::LessThanOrEqual => {
                            Ok((Expression::LeExpr(left_expr, Box::new(right_expr)), pos))
                        }
                        _ => raise_parser_error("Expecting an operator", parser, pos, true),
                    }
                } else {
                    raise_parser_error("Expecting an expression", parser, pos, true)
                }
            }
            Some(_) => {
                let (expr, pos) = Expression::parse(parser, pos)?;
                if parser.peek(pos, Lexeme::RightParen) {
                    let (e, pos) = match expr {
                        Expression::Name(_)
                        | Expression::FuncCallExpr(_, _)
                        | Expression::RecurExpr(_)
                        | Expression::LambdaExpr(_, _)
                        | Expression::ComposeBckExpr(_, _)
                        | Expression::ComposeFwdExpr(_, _)
                        | Expression::QualifiedIdentifier(_, _) => {
                            (Expression::ParenExpr(Box::new(expr)), pos + 1)
                        }
                        _ => (expr, pos + 1),
                    };

                    match e {
                        Expression::ComposeFwdExpr(_, _) | Expression::ComposeBckExpr(_, _) => {
                            let mut args = vec![];
                            let mut pos = pos;
                            while !is_func_call_end_symbol(parser.get_token(pos)) {
                                let (a, new_pos) = Expression::parse(parser, pos)?;
                                args.push(a);
                                pos = new_pos;
                            }
                            Ok((FuncCallExpr(Box::new(e), args), pos))
                        }
                        _ => Ok((e, pos)),
                    }
                } else if parser.peek(pos, Lexeme::Comma) {
                    let mut exprs = vec![expr];
                    let mut pos = pos;
                    while parser.peek(pos, Lexeme::Comma) {
                        pos = consume_symbol(parser, pos, Lexeme::Comma)?;
                        pos = parser.skip_nl(pos);
                        let (expr, new_pos) = Expression::parse(parser, pos)?;
                        pos = new_pos;
                        exprs.push(expr);
                    }
                    pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    Ok((Expression::TupleExpr(exprs), pos))
                } else {
                    let (arg, mut pos) = Expression::parse_primary_expr(parser, pos)?;
                    let mut args = vec![arg];
                    while !parser.peek(pos, Lexeme::RightParen) {
                        let (arg, new_pos) = Expression::parse_primary_expr(parser, pos)?;
                        args.push(arg.clone());
                        pos = new_pos;
                    }
                    let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    let f_call = Expression::FuncCallExpr(Box::new(expr), args);
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
                } else if parser.peek(pos, Lexeme::DotDot) {
                    if parser.peek(pos + 1, Lexeme::RightBracket) {
                        let pos = consume_symbol(parser, pos + 1, Lexeme::RightBracket)?;
                        if exprs.len() == 2 {
                            Ok((
                                Expression::RangeExprInfinite(
                                    Box::new(exprs[0].clone()),
                                    Box::new(exprs[1].clone()),
                                ),
                                pos,
                            ))
                        } else {
                            raise_parser_error("range expression invalid", parser, pos, false)
                        }
                    } else {
                        let (expr, pos) = Expression::parse(parser, pos + 1)?;
                        return if parser.peek(pos, Lexeme::DotDot) {
                            let pos = consume_symbol(parser, pos, Lexeme::DotDot)?;
                            let pos = consume_symbol(parser, pos, Lexeme::RightBracket)?;
                            if exprs.len() == 1 {
                                Ok((
                                    Expression::RangeExprInfinite(
                                        Box::new(exprs[0].clone()),
                                        Box::new(expr),
                                    ),
                                    pos,
                                ))
                            } else {
                                raise_parser_error("range expression invalid", parser, pos, false)
                            }
                        } else {
                            let pos = consume_symbol(parser, pos, Lexeme::RightBracket)?;
                            if exprs.len() == 1 {
                                Ok((
                                    Expression::RangeExpr(
                                        Box::new(exprs[0].clone()),
                                        Box::new(expr),
                                    ),
                                    pos,
                                ))
                            } else if exprs.len() == 2 {
                                Ok((
                                    Expression::RangeExpr3(
                                        Box::new(exprs[0].clone()),
                                        Box::new(exprs[1].clone()),
                                        Box::new(expr),
                                    ),
                                    pos,
                                ))
                            } else {
                                raise_parser_error("range expression invalid", parser, pos, false)
                            }
                        };
                    }
                } else if parser.peek(pos, Lexeme::Guard) {
                    if exprs.len() != 1 {
                        return raise_parser_error(
                            "invalid list comprehension declaration",
                            parser,
                            pos,
                            false,
                        );
                    }
                    let expr = Box::new(exprs[0].clone());
                    let pos = consume_symbol(parser, pos, Lexeme::Guard)?;
                    let (guard, mut pos) = Expression::parse_list_comprehension_guard(parser, pos)?;
                    let mut guards = vec![];
                    guards.push(guard);
                    while !parser.peek(pos, Lexeme::RightBracket) {
                        pos = consume_symbol(parser, pos, Lexeme::Comma)?;
                        let (guard, new_pos) =
                            Expression::parse_list_comprehension_guard(parser, pos)?;
                        guards.push(guard);
                        pos = new_pos;
                    }
                    pos = consume_symbol(parser, pos, Lexeme::RightBracket)?;
                    Ok((Expression::ListByComprehension(expr, guards), pos))
                } else {
                    todo!()
                }
            }
            None => raise_parser_error("unexpected eof", parser, pos, true),
        }
    }

    fn parse_list_comprehension_guard(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(ListComprehensionGuard, usize)> {
        // let id = expr
        // id <- list
        // expr
        match parser.get_token(pos) {
            Some(Lexeme::Let) => {
                let pos = consume_symbol(parser, pos, Lexeme::Let)?;
                if parser.peek(pos, Lexeme::LeftParen) {
                    let pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
                    let (ids, pos) = consume_ids_sep_by(parser, pos, Lexeme::Comma)?;
                    let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                    let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
                    let (expr, pos) = Expression::parse(parser, pos)?;
                    Ok((ListComprehensionGuard::LetTuple(ids, expr), pos))
                } else {
                    let (id, pos) = consume_id(parser, pos)?;
                    let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
                    let (expr, pos) = Expression::parse(parser, pos)?;
                    Ok((ListComprehensionGuard::Let(id, expr), pos))
                }
            }
            Some(Lexeme::Id(id)) if parser.peek(pos + 1, Lexeme::BackArrow) => {
                let pos = consume_symbol(parser, pos + 1, Lexeme::BackArrow)?;
                let (expr, pos) = Expression::parse(parser, pos)?;
                Ok((ListComprehensionGuard::Generator(id, expr), pos))
            }
            Some(Lexeme::LeftParen) => {
                let p = consume_symbol(parser, pos, Lexeme::LeftParen)?;
                let r = consume_ids_sep_by(parser, p, Lexeme::Comma);
                match r {
                    Err(_) => {
                        let (expr, pos) = Expression::parse(parser, pos)?;
                        Ok((ListComprehensionGuard::Expr(expr), pos))
                    }
                    Ok((ids, p)) => {
                        let r = consume_symbol(parser, p, Lexeme::RightParen);
                        match r {
                            Err(_) => {
                                let (expr, pos) = Expression::parse(parser, pos)?;
                                Ok((ListComprehensionGuard::Expr(expr), pos))
                            }
                            Ok(pos) => {
                                let pos = consume_symbol(parser, pos , Lexeme::BackArrow)?;
                                let (expr, pos) = Expression::parse(parser, pos)?;
                                Ok((ListComprehensionGuard::TupleGenerator(ids, expr), pos))
                            }
                        }
                    }
                }

            }
            _ => {
                let (expr, pos) = Expression::parse(parser, pos)?;
                Ok((ListComprehensionGuard::Expr(expr), pos))
            }
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

            pos = consume_symbol(parser, new_pos, Lexeme::Colon)?;
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
            Some(Lexeme::LargeString(index)) => {
                match parser.get_large_string(index) {
                    None => bail!("literal string not found!! {}", index),
                    Some(s) => Ok((
                        Expression::LargeStringLiteral(s),
                        pos + 1,
                    ))
                }
            },
            Some(Lexeme::String(str)) => Ok((Expression::StringLiteral(str), pos + 1)),
            Some(Lexeme::Integer(int)) => Ok((Expression::IntegerLiteral(int), pos + 1)),
            Some(Lexeme::Float(float)) => Ok((Expression::FloatLiteral(float), pos + 1)),
            Some(Lexeme::IsoDate(date)) => Ok((Expression::DateLiteral(date), pos + 1)),
            Some(Lexeme::FormatString(f_str)) => Ok((Expression::FormatString(f_str), pos + 1)),
            Some(Lexeme::Char(chr)) => Ok((Expression::CharLiteral(chr), pos + 1)),
            Some(Lexeme::RegExp(expr)) => Ok((Expression::RegexpLiteral(expr), pos + 1)),
            Some(Lexeme::False) => Ok((Expression::False, pos + 1)),
            Some(Lexeme::True) => Ok((Expression::True, pos + 1)),
            sym => {
                todo!("parse literal sym = {:?}", sym)
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

    pub(crate) fn parse_perform(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Perform)?;
        let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
        if parser.peek(pos, Lexeme::With) {
            let pos = consume_symbol(parser, pos, Lexeme::With)?;
            let (context, pos) = Expression::parse_primary_expr(parser, pos)?;
            Ok((
                Expression::PerformExpr(Box::new(expr), None, Some(Box::new(context))),
                pos,
            ))
        } else if !is_func_call_end_symbol(parser.get_token(pos)) {
            let (args, pos) = consume_args(parser, pos)?;
            if parser.peek(pos, Lexeme::With) {
                let pos = consume_symbol(parser, pos, Lexeme::With)?;
                let (context, pos) = Expression::parse_primary_expr(parser, pos)?;
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
        let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
        if is_func_call_end_symbol(parser.get_token(pos))
            || !matches!(expr, Expression::Name(_)|Expression::NameStr(_)|Expression::ParenExpr(_))
        {
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

    pub(crate) fn parse_recur(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Recur)?;
        let mut args = vec![];
        let (expr, mut pos) = Expression::parse_prim_expr(parser, pos)?;
        args.push(expr);
        while !is_func_call_end_symbol(parser.get_token(pos)) {
            let (arg, new_pos) = Expression::parse_prim_expr(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((Expression::RecurExpr(args), pos))
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

    fn parse_prim_expr(parser: &'a Parser<'a>, pos: usize) -> ParseResult<'a> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftParen) => Expression::parse_paren_expr(parser, pos),
            Some(Lexeme::LeftBracket) => Expression::parse_list_expr(parser, pos),
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

                Ok((expr, pos))
            }
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            sym if is_func_call_end_symbol(sym) => {
                raise_parser_error("invalid token", parser, pos, true)
            }
            _ => {
                Expression::parse_dollar_func_call_expr(parser, pos)
            },
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
        let (for_part, pos) = Expression::parse_for(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Loop)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (loop_body, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::LoopExpr(for_part, Box::new(loop_body)), pos))
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
                Some(Lexeme::Loop) | Some(Lexeme::Dedent)
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
        let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
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
            bail!("empty conditional");
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
                None => bail!("invalid otherwise before other conditions"),
                Some(c) => Ok(Expression::IfExpr(
                    Box::new(c.clone()),
                    Box::new(expr.clone()),
                    Box::new(Expression::if_from(rest)?),
                )),
            }
        }
    }
}
