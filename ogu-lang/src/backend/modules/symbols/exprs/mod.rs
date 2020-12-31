use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticSym;
use crate::backend::modules::symbols::exprs::idents::IdSym;
use crate::backend::modules::symbols::exprs::literals::LiteralSym;
use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use std::ops::Deref;
use crate::backend::modules::symbols::exprs::func_call::FuncCallSym;
use crate::backend::modules::symbols::exprs::if_expr::IfExprSym;
use crate::backend::modules::symbols::exprs::partial_ord::PartialOrdSym;
use crate::backend::modules::symbols::exprs::do_expr::DoExprSym;

mod arithmetics;
mod idents;
mod literals;
mod func_call;
mod if_expr;
mod partial_ord;
mod do_expr;

impl<'a> From<&Expression<'a>> for Box<dyn Symbol> {
    fn from(expr: &Expression<'a>) -> Self {
        match expr {
            Expression::IntegerLiteral(l) => LiteralSym::new_int(l),
            Expression::FloatLiteral(f) => LiteralSym::new_float(f),
            Expression::StringLiteral(s) => LiteralSym::new_str(s),

            Expression::Identifier(id) => IdSym::new(id),

            Expression::AddExpr(l, r) => ArithmeticSym::new_add(l.into(), r.into()),
            Expression::SubExpr(l, r) => ArithmeticSym::new_sub(l.into(), r.into()),
            Expression::MulExpr(l, r) => ArithmeticSym::new_mul(l.into(), r.into()),
            Expression::DivExpr(l, r) => ArithmeticSym::new_div(l.into(), r.into()),
            Expression::ModExpr(l, r) => ArithmeticSym::new_mod(l.into(), r.into()),
            Expression::PowExpr(l, r) => ArithmeticSym::new_pow(l.into(), r.into()),

            Expression::GtExpr(l, r) => PartialOrdSym::new_gt(l.into(), r.into()),
            Expression::GeExpr(l, r) => PartialOrdSym::new_ge(l.into(), r.into()),
            Expression::LtExpr(l, r) => PartialOrdSym::new_lt(l.into(), r.into()),
            Expression::LeExpr(l, r) => PartialOrdSym::new_le(l.into(), r.into()),

            Expression::IfExpr(c, t, e) =>
                IfExprSym::new(c.into(), t.into(), e.into()),

            Expression::DoExpr(exprs) =>
                DoExprSym::new(vec_exprs_into(exprs)),

            Expression::FuncCallExpr(f, args) =>
                FuncCallSym::new(f.into(), vec_exprs_into(args)),
            _e => {
                println!("not implemented for: {:?}", _e);
                todo!()
            }
        }
    }
}

impl<'a> From<&Box<Expression<'a>>> for Box<dyn Symbol> {
    fn from(expr: &Box<Expression<'a>>) -> Self {
        expr.deref().into()
    }
}

fn vec_exprs_into<'a>(args: &[Expression<'a>]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.into()).collect()
}