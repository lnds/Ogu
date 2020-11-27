use crate::backend::OguError;
use crate::lexer::tokens::Symbol;
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
    PipeFuncCall(Box<Expression>, Box<Expression>),
    PipeFirstArgFuncCall(Box<Expression>, Box<Expression>),
    PipeBackFuncCall(Box<Expression>, Box<Expression>),
    PipeBackFirstArgFuncCall(Box<Expression>, Box<Expression>),
    FuncCallWithDollar(Box<Expression>, Box<Expression>),
    FuncCallExpr(Box<Expression>, Box<Expression>),
    LambdaExpr(Vec<LambdaArg>, Box<Expression>),
    MatchesExpr(Box<Expression>, Box<Expression>),
    NoMatchesExpr(Box<Expression>, Box<Expression>),
    ReMatchExpr(Box<Expression>, Box<Expression>),
    ConsExpr(Box<Expression>, Box<Expression>),
    PowExpr(Box<Expression>, Box<Expression>),
    IndexExpr(Box<Expression>, Box<Expression>),
    DotoCall(Box<Expression>, Box<Expression>),
    DotoBackCall(Box<Expression>, Box<Expression>),
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
    OpFunc(Box<Symbol<'static>>),
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
        if parser.peek(pos, Symbol::SemiColon) {
            let mut exprs = vec![expr];
            while parser.peek(pos, Symbol::SemiColon) {
                pos = consume_symbol(parser, pos, Symbol::SemiColon)?;
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
        Expression::parse_control_expr
    );

    parse_right_assoc!(
        parse_dollar_func_call_expr,
        Symbol::Dollar,
        Expression::parse_pipe_func_call_expr
    );

    pub fn parse_control_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("Expecting an expression but found eof"),
            Some(Symbol::Cond) => Expression::parse_cond(parser, pos),
            Some(Symbol::Case) => Expression::parse_case(parser, pos),
            Some(Symbol::Do) => Expression::parse_do(parser, pos),
            Some(Symbol::Try) => Expression::parse_try_expr(parser, pos),
            Some(Symbol::Let) => Expression::parse_let(parser, pos),
            Some(Symbol::For) => Expression::parse_loop(parser, pos),
            Some(Symbol::Loop) => Expression::parse_loop(parser, pos),
            Some(Symbol::If) => Expression::parse_if(parser, pos),
            Some(Symbol::Repeat) => Expression::parse_repeat(parser, pos),
            Some(Symbol::Recur) => Expression::parse_recur(parser, pos),
            Some(Symbol::Resume) => Expression::parse_resume_expr(parser, pos),
            Some(Symbol::Reify) => Expression::parse_reify(parser, pos),
            _ => Expression::parse_lambda_expr(parser, pos),
        }
    }

    pub fn parse_cond(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Cond)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut conds = vec![];
        while !parser.peek(pos, Symbol::Dedent) {
            let (cond, new_pos) = Expression::parse_opt_otherwise(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Symbol::Arrow)?;
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
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        Ok((Expression::CondExpr(conds), pos))
    }

    fn parse_opt_otherwise(parser: &Parser, pos: usize) -> Result<(Option<Expression>, usize)> {
        if parser.peek(pos, Symbol::Otherwise) {
            Ok((None, consume_symbol(parser, pos, Symbol::Otherwise)?))
        } else {
            let (c, p) = Expression::parse_logical_expr(parser, pos)?;
            Ok((Some(c), p))
        }
    }

    pub fn parse_case(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Case)?;
        let (match_expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::Of)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let mut matches = vec![];
        while !parser.peek(pos, Symbol::Dedent) {
            let (cond, new_pos) = Expression::parse_opt_otherwise(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Symbol::Arrow)?;
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
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        Ok((Expression::CaseExpr(Box::new(match_expr), matches), pos))
    }

    pub fn parse_reify(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Reify)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Symbol::Where)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Indent)?;
        let (eq, mut pos) = Equation::parse(parser, pos, true)?;
        let mut eqs = vec![eq];
        pos = parser.skip_nl(pos);
        while !parser.peek(pos, Symbol::Dedent) {
            let (eq, new_pos) = Equation::parse(parser, pos, true)?;
            eqs.push(eq);
            pos = parser.skip_nl(new_pos);
        }
        let pos = consume_symbol(parser, pos, Symbol::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((Expression::ReifyExpr(type_id, eqs), pos))
    }

    pub fn parse_lambda_expr(parser: &Parser, pos: usize) -> ParseResult {
        if !parser.peek(pos, Symbol::Lambda) {
            Expression::parse_logical_expr(parser, pos)
        } else {
            let pos = consume_symbol(parser, pos, Symbol::Lambda)?;
            let (args, pos) = Expression::parse_lambda_args(parser, pos)?;
            let pos = consume_symbol(parser, pos, Symbol::Arrow)?;
            let pos = parser.skip_nl(pos); // skip arrow
            let (expr, pos) = Expression::parse_control_expr(parser, pos)?;
            Ok((Expression::LambdaExpr(args, Box::new(expr)), pos))
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
                let (ids, pos) = consume_ids_sep_by(parser, pos + 1, Symbol::Comma)?;
                let pos = consume_symbol(parser, pos, Symbol::RightParen)?;
                Ok((LambdaArg::Tuple(ids), pos))
            }
            Some(Symbol::Id(id)) => Ok((LambdaArg::Simple(id.to_string()), pos + 1)),
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
        Expression::parse_postfix_expr
    );

    fn parse_postfix_expr(parser: &Parser, pos: usize) -> ParseResult {
        let (expr, pos) = Expression::parse_primary_expr(parser, pos)?;
        if parser.peek(pos, Symbol::Arroba) {
            let (index, pos) = Expression::parse_logical_expr(parser, pos + 1)?;
            Ok((Expression::IndexExpr(Box::new(expr), Box::new(index)), pos))
        } else {
            Ok((expr, pos))
        }
    }

    pub fn parse_primary_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::LeftParen) => Expression::parse_paren_expr(parser, pos),
            Some(Symbol::LeftBracket) => Expression::parse_list_expr(parser, pos),
            Some(Symbol::LeftCurly) => Expression::parse_record_expr(parser, pos),
            Some(Symbol::LeftCurlyCurly) => Expression::parse_macro_expand_expr(parser, pos),
            Some(Symbol::HashCurly) => Expression::parse_dict_expr(parser, pos),
            Some(Symbol::DollarCurly) => Expression::parse_set_expr(parser, pos),
            Some(Symbol::Lazy) => Expression::parse_lazy_expr(parser, pos),
            Some(Symbol::Yield) => Expression::parse_yield_expr(parser, pos),
            Some(Symbol::Perform) => Expression::parse_perform(parser, pos),
            Some(Symbol::Not) => Expression::parse_not_expr(parser, pos),
            Some(sym) if is_literal(sym) => Expression::parse_literal_expr(parser, pos),
            Some(Symbol::TypeId(_)) => Expression::parse_ctor_expr(parser, pos),
            _ => Expression::parse_func_call_expr(parser, pos),
        }
    }

    fn parse_macro_expand_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::LeftCurlyCurly)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::RightCurlyCurly)?;
        Ok((Expression::MacroExpandExpr(Box::new(expr)), pos))
    }

    fn parse_paren_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::LeftParen)?;
        match parser.get_symbol(pos) {
            Some(Symbol::RightParen) => Ok((Expression::Unit, pos + 1)),
            Some(op) if is_basic_op(op) => {
                let (opt_expr, pos) = if parser.peek(pos + 1, Symbol::RightParen) {
                    (None, pos + 1)
                } else {
                    let (expr, pos) = Expression::parse_lambda_expr(parser, pos + 1)?;
                    (Some(Box::new(expr)), pos)
                };
                let pos = consume_symbol(parser, pos, Symbol::RightParen)?;
                match op {
                    Symbol::Cons => Ok((Expression::UnaryCons(opt_expr), pos)),
                    Symbol::Plus => Ok((Expression::UnaryAdd(opt_expr), pos)),
                    Symbol::PlusPlus => Ok((Expression::UnaryConcat(opt_expr), pos)),
                    Symbol::Minus => Ok((Expression::UnarySub(opt_expr), pos)),
                    Symbol::Mult => Ok((Expression::UnaryMul(opt_expr), pos)),
                    Symbol::Pow => Ok((Expression::UnaryPow(opt_expr), pos)),
                    Symbol::Div => Ok((Expression::UnaryDiv(opt_expr), pos)),
                    Symbol::Mod => Ok((Expression::UnaryMod(opt_expr), pos)),
                    Symbol::And => Ok((Expression::UnaryAnd(opt_expr), pos)),
                    Symbol::Or => Ok((Expression::UnaryOr(opt_expr), pos)),
                    Symbol::Not => Ok((Expression::UnaryNot(opt_expr), pos)),
                    Symbol::Equal => Ok((Expression::UnaryEq(opt_expr), pos)),
                    Symbol::NotEqual => Ok((Expression::UnaryNotEq(opt_expr), pos)),
                    Symbol::Greater => Ok((Expression::UnaryGt(opt_expr), pos)),
                    Symbol::GreaterOrEqual => Ok((Expression::UnaryGe(opt_expr), pos)),
                    Symbol::LessThan => Ok((Expression::UnaryLt(opt_expr), pos)),
                    Symbol::LessThanOrEqual => Ok((Expression::UnaryLe(opt_expr), pos)),
                    sym => Err(Error::new(OguError::ParserError(
                        ParseError::ExpectingOperator,
                    )))
                    .context(format!("expecting an operator, found {:?}", sym)),
                }
            }
            Some(_) => {
                let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                if parser.peek(pos, Symbol::RightParen) {
                    Ok((expr, pos + 1))
                } else if parser.peek(pos, Symbol::Comma) {
                    let mut exprs = vec![expr];
                    let mut pos = pos;
                    while parser.peek(pos, Symbol::Comma) {
                        pos = consume_symbol(parser, pos, Symbol::Comma)?;
                        pos = parser.skip_nl(pos);
                        let (expr, new_pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                        pos = new_pos;
                        exprs.push(expr);
                    }
                    pos = consume_symbol(parser, pos, Symbol::RightParen)?;
                    Ok((Expression::TupleExpr(exprs), pos))
                } else {
                    let (expr, pos) = Expression::parse_pipe_func_call_expr(parser, pos)?;
                    let pos = consume_symbol(parser, pos, Symbol::RightParen)?;
                    Ok((expr, pos))
                }
            }
            None => Err(Error::new(OguError::ParserError(
                ParseError::ExpressionExpected,
            )))
            .context("unexpected eof"),
        }
    }

    fn parse_list_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::LeftBracket)?;
        match parser.get_symbol(pos) {
            Some(Symbol::RightBracket) => Ok((Expression::EmptyList, pos + 1)),
            Some(_) => {
                let (exprs, pos) = consume_exprs_sep_by(parser, pos, Symbol::Comma)?;
                if parser.peek(pos, Symbol::RightBracket) {
                    Ok((Expression::ListExpr(exprs), pos + 1))
                } else if parser.peek(pos, Symbol::DotDotDot) {
                    let pos = consume_symbol(parser, pos + 1, Symbol::RightBracket)?;
                    Ok((Expression::RangeInfExpr(exprs), pos))
                } else if parser.peek(pos, Symbol::DotDot) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Symbol::RightBracket) {
                        return Ok((Expression::RangeExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Symbol::DotDotLess) {
                    let (expr, pos) = Expression::parse(parser, pos + 1)?;
                    if parser.peek(pos, Symbol::RightBracket) {
                        return Ok((Expression::RangeOpenExpr(exprs, Box::new(expr)), pos + 1));
                    }
                    todo!()
                } else if parser.peek(pos, Symbol::Guard) {
                    let pos = consume_symbol(parser, pos, Symbol::Guard)?;
                    let (eq, mut pos) = Equation::parse_back_arrow_eq(parser, pos)?;
                    let mut eqs = vec![];
                    let mut guards = vec![];
                    let mut lets = vec![];
                    eqs.push(eq);
                    while parser.peek(pos, Symbol::Comma) {
                        pos = consume_symbol(parser, pos, Symbol::Comma)?;
                        match Equation::parse_back_arrow_eq(parser, pos) {
                            Ok((eq, new_pos)) => {
                                eqs.push(eq);
                                pos = new_pos;
                            }
                            Err(_) => {
                                if parser.peek(pos, Symbol::Let) {
                                    pos = consume_symbol(parser, pos, Symbol::Let)?;
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
                    pos = consume_symbol(parser, pos, Symbol::RightBracket)?;
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
        let mut pos = consume_symbol(parser, pos, Symbol::LeftCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Symbol::RightCurly) {
            let (field, new_pos) = consume_id(parser, pos)?;
            pos = consume_symbol(parser, new_pos, Symbol::Assign)?;
            let (val, new_pos) = Expression::parse(parser, pos)?;
            if parser.peek(new_pos, Symbol::Comma) {
                pos = consume_symbol(parser, new_pos, Symbol::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((field, val));
        }
        pos = consume_symbol(parser, pos, Symbol::RightCurly)?;
        Ok((Expression::RecordExpr(pairs), pos))
    }

    fn parse_dict_expr(parser: &Parser, pos: usize) -> ParseResult {
        let mut pos = consume_symbol(parser, pos, Symbol::HashCurly)?;
        let mut pairs = vec![];
        while !parser.peek(pos, Symbol::RightCurly) {
            let (key, new_pos) = Expression::parse_primary_expr(parser, pos)?;

            pos = consume_symbol(parser, new_pos, Symbol::FatArrow)?;
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Symbol::Comma) {
                pos = consume_symbol(parser, new_pos, Symbol::Comma)?;
            } else {
                pos = new_pos;
            }
            pairs.push((key, val));
        }
        pos = consume_symbol(parser, pos, Symbol::RightCurly)?;
        Ok((Expression::DictExpr(pairs), pos))
    }

    fn parse_set_expr(parser: &Parser, pos: usize) -> ParseResult {
        let mut pos = consume_symbol(parser, pos, Symbol::DollarCurly)?;
        let mut elems = vec![];
        while !parser.peek(pos, Symbol::RightCurly) {
            let (val, new_pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(new_pos, Symbol::Comma) {
                pos = consume_symbol(parser, new_pos, Symbol::Comma)?;
            } else {
                pos = new_pos;
            }
            elems.push(val);
        }
        pos = consume_symbol(parser, pos, Symbol::RightCurly)?;
        Ok((Expression::SetExpr(elems), pos))
    }

    fn parse_lazy_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Lazy)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_yield_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Yield)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::LazyExpr(Box::new(expr)), pos))
    }

    fn parse_not_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Not)?;
        let (expr, pos) = Expression::parse(parser, pos)?;
        Ok((Expression::NotExpr(Box::new(expr)), pos))
    }

    fn parse_literal_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::LargeString(index)) => Ok((
                Expression::LargeStringLiteral(parser.get_large_string(index)),
                pos + 1,
            )),
            Some(Symbol::String(str)) => Ok((Expression::StringLiteral(str.to_string()), pos + 1)),
            Some(Symbol::Integer(int)) => {
                Ok((Expression::IntegerLiteral(int.to_string()), pos + 1))
            }
            Some(Symbol::Float(float)) => {
                Ok((Expression::FloatLiteral(float.to_string()), pos + 1))
            }
            Some(Symbol::IsoDate(date)) => Ok((Expression::DateLiteral(date.to_string()), pos + 1)),
            Some(Symbol::FormatString(f_str)) => {
                Ok((Expression::FormatString(f_str.to_string()), pos + 1))
            }
            Some(Symbol::Char(chr)) => Ok((Expression::CharLiteral(chr.to_string()), pos + 1)),
            Some(Symbol::RegExp(expr)) => {
                Ok((Expression::RegexpLiteral(expr.to_string()), pos + 1))
            }
            sym => {
                println!("parse literal sym = {:?}", sym);
                todo!()
            }
        }
    }

    fn parse_ctor_expr(parser: &Parser, pos: usize) -> ParseResult {
        match parser.get_symbol(pos) {
            Some(Symbol::TypeId(tid)) => {
                let type_id = tid.to_string();
                let mut pos = consume_symbol(parser, pos, Symbol::TypeId(tid))?;
                let mut q_ids = vec![];
                if parser.peek(pos, Symbol::Dot) {
                    while parser.peek(pos, Symbol::Dot) {
                        pos = consume_symbol(parser, pos, Symbol::Dot)?;
                        match parser.get_symbol(pos) {
                            Some(Symbol::Id(id)) => {
                                q_ids.push(Expression::Identifier(id.to_string()))
                            }
                            Some(Symbol::TypeId(tid)) => {
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
        let pos = consume_symbol(parser, pos, Symbol::Recur)?;
        let (args, pos) = consume_args(parser, pos)?;
        Ok((Expression::RecurExpr(args), pos))
    }

    pub fn parse_perform(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Perform)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        if parser.peek(pos, Symbol::With) {
            let pos = consume_symbol(parser, pos, Symbol::With)?;
            let (context, pos) = Expression::parse_prim_expr(parser, pos)?;
            Ok((
                Expression::PerformExpr(Box::new(expr), None, Some(Box::new(context))),
                pos,
            ))
        } else if !is_func_call_end_symbol(parser.get_symbol(pos)) {
            let (args, pos) = consume_args(parser, pos)?;
            if parser.peek(pos, Symbol::With) {
                let pos = consume_symbol(parser, pos, Symbol::With)?;
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
            Some(Symbol::Id(_)) => {
                let (id, mut pos) = consume_id(parser, pos)?;
                let mut fields = vec![];
                while parser.peek(pos, Symbol::Dot) {
                    pos = consume_symbol(parser, pos, Symbol::Dot)?;
                    let (id, new_pos) = consume_id(parser, pos)?;
                    fields.push(id);
                    pos = new_pos;
                }
                let (expr, pos) = if fields.is_empty() {
                    (Expression::Identifier(id), pos)
                } else {
                    (Expression::QualifedIdentifier(id, fields), pos)
                };
                if parser.peek(pos, Symbol::Dollar) {
                    let pos = consume_symbol(parser, pos, Symbol::Dollar)?;
                    let (arg, pos) = Expression::parse(parser, pos)?;
                    Ok((
                        Expression::FuncCallWithDollar(Box::new(expr), Box::new(arg)),
                        pos,
                    ))
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
        let pos = consume_symbol(parser, pos, Symbol::Let)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut equations = vec![];
        let (equation, pos) = Expression::parse_let_equation(parser, pos)?;
        equations.push(equation);
        let pos = consume_opt_symbol(parser, pos, Symbol::Comma)?;
        let mut pos = parser.skip_nl(pos);
        while !parser.peek(pos, Symbol::Dedent) && !parser.peek(pos, Symbol::In) {
            let (equation, new_pos) = Expression::parse_let_equation(parser, pos)?;
            equations.push(equation);
            pos = consume_opt_symbol(parser, new_pos, Symbol::Comma)?;
            pos = parser.skip_nl(pos);
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::In)?;
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
        let (for_part, pos) = if parser.peek(pos, Symbol::For) {
            let (eqs, pos) = Expression::parse_for(parser, pos)?;
            (Some(eqs), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let (opt_while_or_until_part, pos) = if parser.peek(pos, Symbol::While) {
            let (while_part, pos) = Expression::parse_while(parser, pos)?;
            (Some(while_part), pos)
        } else if parser.peek(pos, Symbol::Until) {
            let (until_part, pos) = Expression::parse_until(parser, pos)?;
            (Some(until_part), pos)
        } else {
            (None, pos)
        };
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Symbol::Loop)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (loop_body, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, indent)?;
        if parser.peek(pos, Symbol::Return) {
            let pos = consume_symbol(parser, pos, Symbol::Return)?;
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
        let pos = consume_symbol(parser, pos, Symbol::For)?;
        let (indent, pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        let (eq, mut pos) = Equation::parse_back_arrow_or_assign_eq(parser, pos)?;
        eqs.push(eq);
        while parser.peek(pos, Symbol::NewLine) || parser.peek(pos, Symbol::Comma) {
            pos = parser.skip_nl(pos + 1);
            if matches!(
                parser.get_symbol(pos),
                Some(Symbol::Loop)
                    | Some(Symbol::While)
                    | Some(Symbol::Until)
                    | Some(Symbol::Dedent)
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
        let pos = consume_symbol(parser, pos, Symbol::While)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::WhileExpr(Box::new(expr)), pos))
    }

    fn parse_until(parser: &Parser, pos: usize) -> Result<(LoopCond, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::Until)?;
        let (expr, pos) = Expression::parse_logical_expr(parser, pos)?;
        Ok((LoopCond::UntilExpr(Box::new(expr)), pos))
    }

    fn parse_repeat(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Repeat)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (expr, mut pos) = Expression::parse_recur_expr(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut exprs = vec![];
        exprs.push(expr);
        while parser.peek(pos, Symbol::Comma) {
            pos = consume_symbol(parser, pos, Symbol::Comma)?;
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
        if parser.peek(pos, Symbol::Let) {
            let pos = consume_symbol(parser, pos, Symbol::Let)?;
            let (id, pos) = consume_id(parser, pos)?;
            let pos = consume_symbol(parser, pos, Symbol::Assign)?;
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Var(id, expr), pos))
        } else {
            let (expr, pos) = Expression::parse(parser, pos)?;
            Ok((RecurValue::Value(expr), pos))
        }
    }

    fn parse_if(parser: &Parser, pos: usize) -> ParseResult {
        Expression::parse_inner_if(parser, pos, Symbol::If)
    }

    fn parse_inner_if(parser: &Parser, pos: usize, if_symbol: Symbol) -> ParseResult {
        let pos = consume_symbol(parser, pos, if_symbol)?;
        let (cond, pos) = Expression::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Symbol::Then)?;
        let pos = parser.skip_nl(pos);
        let (indent, pos) = parse_opt_indent(parser, pos);
        let (then_expr, pos) = Expression::parse(parser, pos)?;
        let pos = parser.skip_nl(pos);
        let pos = parse_opt_dedent(parser, pos, indent)?;
        let pos = parser.skip_nl(pos);
        if parser.peek(pos, Symbol::Else) {
            let pos = consume_symbol(parser, pos, Symbol::Else)?;
            let pos = parser.skip_nl(pos);
            let (indent, pos) = parse_opt_indent(parser, pos);
            let (else_expr, pos) = Expression::parse(parser, pos)?;
            let pos = parse_opt_dedent(parser, pos, indent)?;
            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(else_expr)),
                pos,
            ))
        } else {
            let (elif_expr, pos) = Expression::parse_inner_if(parser, pos, Symbol::Elif)?;
            Ok((
                Expression::IfExpr(Box::new(cond), Box::new(then_expr), Box::new(elif_expr)),
                pos,
            ))
        }
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

    fn parse_try_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Try)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (expr, pos) = Expression::parse(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Symbol::Handle)?;
        let pos = parser.skip_nl(pos);
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (handles, pos) = Expression::parse_handle_guards(parser, pos)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok((TryHandleExpr(Box::new(expr), handles), pos))
    }

    pub fn parse_handle_guards(parser: &Parser, pos: usize) -> Result<(Vec<HandleGuard>, usize)> {
        let mut handles = vec![];
        let mut pos = pos;
        while parser.peek(pos, Symbol::Guard) {
            let (handle, new_pos) = Expression::parse_handle_guard(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            handles.push(handle);
        }
        pos = parser.skip_nl(pos);
        Ok((handles, pos))
    }

    fn parse_handle_guard(parser: &Parser, pos: usize) -> Result<(HandleGuard, usize)> {
        let pos = consume_symbol(parser, pos, Symbol::Guard)?;
        let (expr, pos) = Expression::parse_prim_expr(parser, pos)?;
        let (ctx, args, pos) = if parser.peek(pos, Symbol::With) {
            let pos = consume_symbol(parser, pos, Symbol::With)?;
            let (args, pos) = Expression::parse_args_before_arrow(parser, pos)?;
            (None, if args.is_empty() { None } else { Some(args) }, pos)
        } else if !parser.peek(pos, Symbol::Arrow) && !parser.peek(pos, Symbol::NewLine) {
            let (ctx, pos) = Expression::parse(parser, pos)?;
            if parser.peek(pos, Symbol::With) {
                let pos = consume_symbol(parser, pos, Symbol::With)?;
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
        if parser.peek(pos, Symbol::Arrow) {
            let pos = consume_symbol(parser, pos, Symbol::Arrow)?;
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
        while !parser.peek(pos, Symbol::Arrow) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        Ok((args, pos))
    }

    fn parse_resume_expr(parser: &Parser, pos: usize) -> ParseResult {
        let pos = consume_symbol(parser, pos, Symbol::Resume)?;
        if is_func_call_end_symbol(parser.get_symbol(pos)) {
            Ok((Expression::ResumeExpr(None, None), pos))
        } else if parser.peek(pos, Symbol::With) {
            let pos = consume_symbol(parser, pos, Symbol::With)?;
            let (args, pos) = consume_args(parser, pos)?;
            Ok((Expression::ResumeExpr(None, Some(args)), pos))
        } else {
            let (new_ctx, pos) = Expression::parse_primary_expr(parser, pos)?;
            if parser.peek(pos, Symbol::With) {
                let pos = consume_symbol(parser, pos, Symbol::With)?;
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
