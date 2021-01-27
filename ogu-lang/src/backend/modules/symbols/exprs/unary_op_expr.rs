use crate::backend::modules::symbols::exprs::arithmetics::ArithmeticExpr;
use crate::backend::modules::symbols::exprs::list_expr::ListExpr;
use crate::backend::modules::symbols::exprs::logical_expr::LogicalExpr;
use crate::backend::modules::symbols::exprs::partial_eq::PartialEqExpr;
use crate::backend::modules::symbols::exprs::partial_ord::PartialOrdExpr;
use crate::backend::modules::symbols::func::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::basic_type::BOOL_TYPE;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_EQ, TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};

pub(crate) struct UnaryOpExpr;

type NewFn = fn(Box<dyn Symbol>, Box<dyn Symbol>) -> Box<dyn Symbol>;

impl UnaryOpExpr {
    pub(crate) fn new_add(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_add)
    }

    pub(crate) fn new_mul(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_mul)
    }

    pub(crate) fn new_sub(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_sub)
    }

    pub(crate) fn new_div(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_div)
    }

    pub(crate) fn new_int_div(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_int_div)
    }

    pub(crate) fn new_mod(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_mod)
    }

    pub(crate) fn new_pow(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_NUM, ArithmeticExpr::new_pow)
    }

    pub(crate) fn new_eq(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_EQ, PartialEqExpr::new_eq)
    }

    pub(crate) fn new_ne(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_EQ, PartialEqExpr::new_ne)
    }

    pub(crate) fn new_gt(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_ORD, PartialOrdExpr::new_gt)
    }

    pub(crate) fn new_ge(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_ORD, PartialOrdExpr::new_ge)
    }

    pub(crate) fn new_lt(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_ORD, PartialOrdExpr::new_lt)
    }

    pub(crate) fn new_le(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, TRAIT_ORD, PartialOrdExpr::new_le)
    }

    pub(crate) fn new_or(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, BOOL_TYPE, LogicalExpr::new_or)
    }

    pub(crate) fn new_and(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Self::make_lambda(expr, BOOL_TYPE, LogicalExpr::new_and)
    }

    pub(crate) fn new_not() -> Box<dyn Symbol> {
        let x = IdSym::new_with_type("x", Some(BOOL_TYPE.clone_box()));
        let expr = LogicalExpr::new_not(x.clone());
        LambdaExpr::new(vec![x], expr)
    }

    pub(crate) fn new_cons(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        match expr {
            None => {
                let x = IdSym::new_with_type("x", Some(TRAIT_UNKNOWN.clone_box()));
                let xs =
                    IdSym::new_with_type("xs", Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                LambdaExpr::new(vec![x.clone(), xs.clone()], ListExpr::new_cons(x, xs))
            }
            Some(expr) => {
                let xs = IdSym::new_with_type(
                    "xs",
                    expr.get_type().map(|t| ListType::new_list(t.clone_box())),
                );
                LambdaExpr::new(vec![xs.clone()], ListExpr::new_cons(expr, xs))
            }
        }
    }

    pub(crate) fn new_concat(expr: Option<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        match expr {
            None => {
                let x =
                    IdSym::new_with_type("x", Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                let xs =
                    IdSym::new_with_type("xs", Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                LambdaExpr::new(vec![x.clone(), xs.clone()], ListExpr::new_concat(x, xs))
            }
            Some(expr) => {
                let xs = IdSym::new_with_type("xs", expr.get_type());
                LambdaExpr::new(vec![xs.clone()], ListExpr::new_concat(expr, xs))
            }
        }
    }

    fn make_lambda(expr: Option<Box<dyn Symbol>>, tr: &dyn Type, new: NewFn) -> Box<dyn Symbol> {
        match expr {
            None => {
                let x = IdSym::new_with_type("x", Some(tr.clone_box()));
                let y = IdSym::new_with_type("y", Some(tr.clone_box()));
                LambdaExpr::new(vec![x.clone(), y.clone()], new(x, y))
            }
            Some(expr) => {
                let x = IdSym::new_with_type("x", Some(tr.clone_box()));
                LambdaExpr::new(vec![x.clone()], new(expr, x))
            }
        }
    }
}
