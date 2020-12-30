use crate::parser::ast::expressions::expression::Expression;
use crate::backend::modules::symbols::exprs::literals::LiteralSym;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticSym;
use std::ops::Deref;
use crate::backend::modules::symbols::exprs::idents::IdSym;

mod literals;
mod arithmetics;
mod idents;


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