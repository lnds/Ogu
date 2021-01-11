use std::ops::Deref;

use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticSym;
use crate::backend::modules::symbols::exprs::case_expr::CaseExpr;
use crate::backend::modules::symbols::exprs::do_expr::DoExpr;
use crate::backend::modules::symbols::exprs::func_call::FuncCallExpr;
use crate::backend::modules::symbols::exprs::guarded_expr::GuardedExpr;
use crate::backend::modules::symbols::exprs::if_expr::IfExpr;
use crate::backend::modules::symbols::exprs::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::exprs::let_expr::LetExpr;
use crate::backend::modules::symbols::exprs::literals::Literal;
use crate::backend::modules::symbols::exprs::logical_expr::LogicalSym;
use crate::backend::modules::symbols::exprs::paren_expr::ParenExpr;
use crate::backend::modules::symbols::exprs::partial_eq::PartialEqExpr;
use crate::backend::modules::symbols::exprs::partial_ord::PartialOrdExpr;
use crate::backend::modules::symbols::exprs::recur_call::RecurCallExpr;
use crate::backend::modules::symbols::exprs::tuple_expr::TupleExpr;
use crate::backend::modules::symbols::exprs::unary_op_expr::UnaryOpExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{Expression, LambdaArg, OptExprTuple};

mod arithmetics;
mod case_expr;
mod comparable_trait;
mod do_expr;
pub(crate) mod func_call;
mod guarded_expr;
mod if_expr;
mod lambda_expr;
mod let_expr;
mod literals;
mod logical_expr;
mod paren_expr;
mod partial_eq;
mod partial_ord;
mod recur_call;
mod tuple_expr;
mod unary_op_expr;

impl<'a> From<&Expression<'a>> for Box<dyn Symbol> {
    fn from(expr: &Expression<'a>) -> Self {
        match expr {
            Expression::InvalidExpr => Literal::new_invalid(),

            Expression::IntegerLiteral(l) => Literal::new_int(l),
            Expression::FloatLiteral(f) => Literal::new_float(f),
            Expression::StringLiteral(s) => Literal::new_str(s),
            Expression::CharLiteral(s) => Literal::new_char(s),
            Expression::DateLiteral(s) => Literal::new_date(s),
            Expression::RegexpLiteral(s) => Literal::new_regexp(s),
            Expression::Unit => Literal::new_unit(),

            Expression::Name(id) => IdSym::new(id),
            Expression::NameStr(id) => IdSym::new(id),

            Expression::ParenExpr(expr) => ParenExpr::make(expr),

            Expression::TupleExpr(tuple) => TupleExpr::make(vec_exprs_into(tuple)),

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
                UnaryOpExpr::new_intdiv(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryMod(expr) => {
                UnaryOpExpr::new_mod(expr.deref().as_ref().map(|e| e.into()))
            }
            Expression::UnaryPow(expr) => {
                UnaryOpExpr::new_pow(expr.deref().as_ref().map(|e| e.into()))
            }

            Expression::AddExpr(l, r) => ArithmeticSym::new_add(l.into(), r.into()),
            Expression::SubExpr(l, r) => ArithmeticSym::new_sub(l.into(), r.into()),
            Expression::MulExpr(l, r) => ArithmeticSym::new_mul(l.into(), r.into()),
            Expression::IntDivExpr(l, r) => ArithmeticSym::new_int_div(l.into(), r.into()),
            Expression::DivExpr(l, r) => ArithmeticSym::new_div(l.into(), r.into()),
            Expression::ModExpr(l, r) => ArithmeticSym::new_mod(l.into(), r.into()),
            Expression::PowExpr(l, r) => ArithmeticSym::new_pow(l.into(), r.into()),

            Expression::NotExpr(e) => LogicalSym::new_not(e.into()),
            Expression::AndExpr(l, r) => LogicalSym::new_and(l.into(), r.into()),
            Expression::OrExpr(l, r) => LogicalSym::new_or(l.into(), r.into()),

            Expression::GtExpr(l, r) => PartialOrdExpr::new_gt(l.into(), r.into()),
            Expression::GeExpr(l, r) => PartialOrdExpr::new_ge(l.into(), r.into()),
            Expression::LtExpr(l, r) => PartialOrdExpr::new_lt(l.into(), r.into()),
            Expression::LeExpr(l, r) => PartialOrdExpr::new_le(l.into(), r.into()),

            Expression::EqExpr(l, r) => PartialEqExpr::new_eq(l.into(), r.into()),
            Expression::NeExpr(l, r) => PartialEqExpr::new_ne(l.into(), r.into()),

            Expression::CaseExpr(e, cases) => CaseExpr::new(e.into(), vec_exprs_tuples_into(cases)),

            Expression::GuardedExpr(e, g) => GuardedExpr::new(e.into(), g.into()),

            Expression::IfExpr(c, t, e) => IfExpr::new(c.into(), t.into(), e.into()),

            Expression::LetExpr(eqs, expr) => LetExpr::new(vec_eqs_into(eqs), expr.into()),

            Expression::DoExpr(exprs) => DoExpr::new(vec_exprs_into(exprs)),

            Expression::LambdaExpr(args, expr) => {
                LambdaExpr::new(vec_lambda_args_into(args), expr.into())
            }

            Expression::RecurExpr(args) => RecurCallExpr::new(vec_exprs_into(args)),

            Expression::FuncCallExpr(f, args) => FuncCallExpr::new(f.into(), vec_exprs_into(args)),
            _e => {
                println!("not implemented for: {:?}", _e);
                todo!()
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
            Equation::Value(id, expr) => ValueSym::new(id, expr),
            _e => {
                println!("not implemented for {:?}", _e);
                todo!()
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

impl<'a> From<&LambdaArg<'a>> for Box<dyn Symbol> {
    fn from(arg: &LambdaArg<'a>) -> Self {
        match arg {
            LambdaArg::Simple(s) => IdSym::new(&s),
            _ => todo!(),
        }
    }
}
