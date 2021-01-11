use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticSym;
use crate::backend::modules::symbols::exprs::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::TypeClone;

pub(crate) struct UnaryOpExpr;

type NewFn = fn(Box<dyn Symbol>, Box<dyn Symbol>) -> Box<dyn Symbol>;

impl UnaryOpExpr {
    pub(crate) fn new_add(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_add)
    }

    pub(crate) fn new_mul(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_mul)
    }

    pub(crate) fn new_sub(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_sub)
    }

    pub(crate) fn new_div(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_div)
    }

    pub(crate) fn new_intdiv(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_int_div)
    }

    pub(crate) fn new_mod(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_mod)
    }

    pub(crate) fn new_pow(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, ArithmeticSym::new_pow)
    }

    fn make_lambda(expr: Option<Box<dyn Symbol>>, new: NewFn) -> Box<dyn Symbol> {
        match expr {
            None => {
                let x = IdSym::new_with_type("x", Some(TRAIT_NUM.clone_box()));
                let y = IdSym::new_with_type("x", Some(TRAIT_NUM.clone_box()));
                LambdaExpr::new(vec![x.clone(), y.clone()], new(x, y))
            }
            Some(expr) => {
                let x = IdSym::new_with_type("x", Some(TRAIT_NUM.clone_box()));
                LambdaExpr::new(vec![x.clone()], new(expr, x))
            }
        }
    }
}
