use std::ops::Deref;

use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticExpr;
use crate::backend::modules::symbols::exprs::case_expr::CaseExpr;
use crate::backend::modules::symbols::exprs::dict_expr::DictExpr;
use crate::backend::modules::symbols::exprs::do_expr::DoExpr;
use crate::backend::modules::symbols::exprs::guarded_expr::GuardedExpr;
use crate::backend::modules::symbols::exprs::if_expr::IfExpr;
use crate::backend::modules::symbols::exprs::lazy_call::LazyExpr;
use crate::backend::modules::symbols::exprs::let_expr::LetExpr;
use crate::backend::modules::symbols::exprs::list_comp::ListComprehension;
use crate::backend::modules::symbols::exprs::list_expr::ListExpr;
use crate::backend::modules::symbols::exprs::literals::Literal;
use crate::backend::modules::symbols::exprs::logical_expr::LogicalExpr;
use crate::backend::modules::symbols::exprs::loop_expr::LoopExpr;
use crate::backend::modules::symbols::exprs::paren_expr::ParenExpr;
use crate::backend::modules::symbols::exprs::partial_eq::PartialEqExpr;
use crate::backend::modules::symbols::exprs::partial_ord::PartialOrdExpr;
use crate::backend::modules::symbols::exprs::range_expr::RangeExpr;
use crate::backend::modules::symbols::exprs::repeat_expr::RepeatExpr;
use crate::backend::modules::symbols::exprs::tuple_expr::TupleExpr;
use crate::backend::modules::symbols::exprs::unary_op_expr::UnaryOpExpr;
use crate::backend::modules::symbols::func::func_call::FuncCallExpr;
use crate::backend::modules::symbols::func::func_compose::ComposeFunction;
use crate::backend::modules::symbols::func::func_def::Function;
use crate::backend::modules::symbols::func::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::func::recur_call::RecurCallExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{
    Expression, LambdaArg, ListComprehensionGuard, OptExprTuple, RecurValue,
};
use crate::backend::modules::symbols::exprs::index_expr::IndexExpr;

mod arithmetics;
mod case_expr;
mod comparable_trait;
mod dict_expr;
mod do_expr;
mod guarded_expr;
mod if_expr;
mod lazy_call;
mod let_expr;
mod list_comp;
mod list_expr;
mod list_guards;
mod literals;
mod logical_expr;
mod loop_expr;
mod paren_expr;
mod partial_eq;
mod partial_ord;
mod range_expr;
mod repeat_expr;
mod tuple_expr;
mod unary_op_expr;
mod index_expr;

impl<'a> From<&Expression<'a>> for Box<dyn Symbol> {
    fn from(expr: &Expression<'a>) -> Self {
        match expr {
            Expression::InvalidExpr => Literal::new_invalid(),

            Expression::IntegerLiteral(l) => Literal::new_int(l),
            Expression::FloatLiteral(f) => Literal::new_float(f),
            Expression::StringLiteral(s) => Literal::new_str(s),
            Expression::LargeStringLiteral(s) => Literal::new_str(s),
            Expression::CharLiteral(s) => Literal::new_char(s),
            Expression::DateLiteral(s) => Literal::new_date(s),
            Expression::RegexpLiteral(s) => Literal::new_regexp(s),
            Expression::Unit => Literal::new_unit(),
            Expression::True => Literal::new_true(),
            Expression::False => Literal::new_false(),

            Expression::Name(id) => IdSym::new(id),
            Expression::NameStr(id) => IdSym::new(id),

            Expression::ParenExpr(expr) => ParenExpr::make(expr),

            Expression::TupleExpr(tuple) => TupleExpr::make(vec_exprs_into(tuple)),

            Expression::EmptyList => ListExpr::new_empty(),

            Expression::ListExpr(exprs) => ListExpr::new_list(vec_exprs_into(exprs)),

            Expression::ConsExpr(a, l) => ListExpr::new_cons(a.into(), l.into()),

            Expression::ConcatExpr(l1, l2) => ListExpr::new_concat(l1.into(), l2.into()),

            Expression::RangeExpr(a, b) => RangeExpr::new_range(a.into(), b.into()),

            Expression::RangeExprInfinite(a, b) => {
                RangeExpr::new_range_infinite(a.into(), b.into())
            }

            Expression::RangeExpr3(a, b, c) => {
                RangeExpr::new_step_range(a.into(), b.into(), c.into())
            }

            Expression::ListByComprehension(e, g) => {
                ListComprehension::new_list_comp(e.into(), vec_list_comp_guards_into(g))
            }

            Expression::DictExpr(exprs) => {
                let exprs = exprs.iter().map(|(k, v)| (k.into(), v.into())).collect();
                DictExpr::new_dict(exprs)
            }

            Expression::UnaryAdd(expr) => {
                UnaryOpExpr::new_add(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnarySub(expr) => {
                UnaryOpExpr::new_sub(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryMul(expr) => {
                UnaryOpExpr::new_mul(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryDiv(expr) => {
                UnaryOpExpr::new_div(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryDivDiv(expr) => {
                UnaryOpExpr::new_int_div(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryMod(expr) => {
                UnaryOpExpr::new_mod(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryPow(expr) => {
                UnaryOpExpr::new_pow(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryEq(expr) => {
                UnaryOpExpr::new_eq(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryNotEq(expr) => {
                UnaryOpExpr::new_ne(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryGt(expr) => {
                UnaryOpExpr::new_gt(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryGe(expr) => {
                UnaryOpExpr::new_ge(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryLt(expr) => {
                UnaryOpExpr::new_lt(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryLe(expr) => {
                UnaryOpExpr::new_le(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryAnd(expr) => {
                UnaryOpExpr::new_and(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryOr(expr) => {
                UnaryOpExpr::new_or(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryNot => UnaryOpExpr::new_not(),

            Expression::UnaryCons(expr) => {
                UnaryOpExpr::new_cons(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::UnaryConcat(expr) => {
                UnaryOpExpr::new_concat(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::ComposeFwdExpr(f, g) => ComposeFunction::new_fwd(f.into(), g.into()),

            Expression::ComposeBckExpr(f, g) => ComposeFunction::new_bck(f.into(), g.into()),

            Expression::AddExpr(l, r) => ArithmeticExpr::new_add(l.into(), r.into()),
            Expression::SubExpr(l, r) => ArithmeticExpr::new_sub(l.into(), r.into()),
            Expression::MulExpr(l, r) => ArithmeticExpr::new_mul(l.into(), r.into()),
            Expression::IntDivExpr(l, r) => ArithmeticExpr::new_int_div(l.into(), r.into()),
            Expression::DivExpr(l, r) => ArithmeticExpr::new_div(l.into(), r.into()),
            Expression::ModExpr(l, r) => ArithmeticExpr::new_mod(l.into(), r.into()),
            Expression::PowExpr(l, r) => ArithmeticExpr::new_pow(l.into(), r.into()),

            Expression::NotExpr(e) => LogicalExpr::new_not(e.into()),
            Expression::AndExpr(l, r) => LogicalExpr::new_and(l.into(), r.into()),
            Expression::OrExpr(l, r) => LogicalExpr::new_or(l.into(), r.into()),

            Expression::GtExpr(l, r) => PartialOrdExpr::new_gt(l.into(), r.into()),
            Expression::GeExpr(l, r) => PartialOrdExpr::new_ge(l.into(), r.into()),
            Expression::LtExpr(l, r) => PartialOrdExpr::new_lt(l.into(), r.into()),
            Expression::LeExpr(l, r) => PartialOrdExpr::new_le(l.into(), r.into()),

            Expression::EqExpr(l, r) => PartialEqExpr::new_eq(l.into(), r.into()),
            Expression::NeExpr(l, r) => PartialEqExpr::new_ne(l.into(), r.into()),

            Expression::CaseExpr(e, cases) => CaseExpr::new(e.into(), vec_exprs_tuples_into(cases)),

            Expression::GuardedExpr(e, g) => GuardedExpr::new(e.into(), g.into()),

            Expression::IfExpr(c, t, e) => IfExpr::new(c.into(), t.into(), e.into()),

            Expression::LoopExpr(fe, expr) => LoopExpr::new(vec_eqs_into(&fe), expr.into()),

            Expression::RepeatExpr(reps) => RepeatExpr::new(vec_recur_val_into(&reps)),

            Expression::LetExpr(eqs, expr) => LetExpr::new(vec_eqs_into(eqs), expr.into()),

            Expression::DoExpr(exprs) => DoExpr::new(vec_exprs_into(exprs)),

            Expression::IndexExpr(expr, index) => IndexExpr::new(expr.into(), index.into()),

            Expression::LambdaExpr(args, expr) => {
                LambdaExpr::new(vec_lambda_args_into(args), expr.into())
            }

            Expression::LazyExpr(e) => LazyExpr::new(e.into()),
            Expression::RecurExpr(args) => RecurCallExpr::new(vec_exprs_into(args)),

            Expression::FuncCallExpr(f, args) => FuncCallExpr::new(f.into(), vec_exprs_into(args)),
            _e => {
                todo!("not implemented for: {:?}", _e);
            }
        }
    }
}

impl<'a> From<Box<Expression<'a>>> for Box<dyn Symbol> {
    fn from(expr: Box<Expression<'a>>) -> Self {
        expr.deref().into()
    }
}

impl<'a> From<&Box<Expression<'a>>> for Box<dyn Symbol> {
    fn from(expr: &Box<Expression<'a>>) -> Self {
        expr.deref().into()
    }
}

impl<'a> From<Expression<'a>> for Box<dyn Symbol> {
    fn from(expr: Expression<'a>) -> Self {
        (&expr).into()
    }
}

impl<'a> From<&Equation<'a>> for Box<dyn Symbol> {
    fn from(eq: &Equation<'a>) -> Self {
        match eq {
            Equation::Val(id, expr) => ValueSym::new(id, expr),
            Equation::Func(name, args, expr) => Function::make_box(name, args, expr),
            _e => {
                todo!("not implemented for {:?}", _e);
            }
        }
    }
}

fn vec_exprs_into(args: &[Expression]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.into()).collect()
}

fn vec_lambda_args_into(args: &[LambdaArg]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.into()).collect()
}

pub(crate) type OptSymbolTuple = (Option<Box<dyn Symbol>>, Box<dyn Symbol>);

fn vec_exprs_tuples_into(args: &[OptExprTuple]) -> Vec<OptSymbolTuple> {
    args.iter()
        .map(|(a, b)| (a.clone().map(|e| e.into()), b.into()))
        .collect()
}

fn vec_eqs_into(eqs: &[Equation]) -> Vec<Box<dyn Symbol>> {
    eqs.iter().map(|eq| eq.into()).collect()
}

fn vec_recur_val_into(eqs: &[RecurValue]) -> Vec<Box<dyn Symbol>> {
    eqs.iter().map(|eq| eq.into()).collect()
}

impl<'a> From<&LambdaArg<'a>> for Box<dyn Symbol> {
    fn from(arg: &LambdaArg<'a>) -> Self {
        match arg {
            LambdaArg::Simple(s) => IdSym::new(&s),
            _ => todo!(),
        }
    }
}

fn vec_list_comp_guards_into(guards: &[ListComprehensionGuard]) -> Vec<Box<dyn Symbol>> {
    guards.iter().map(|g| g.into()).collect()
}

impl<'a> From<&RecurValue<'a>> for Box<dyn Symbol> {
    fn from(guard: &RecurValue<'a>) -> Self {
        match guard {
            RecurValue::Value(e) => e.into(),
            RecurValue::Var(id, e) => ValueSym::make(IdSym::new_id(id), e.into()),
        }
    }
}
