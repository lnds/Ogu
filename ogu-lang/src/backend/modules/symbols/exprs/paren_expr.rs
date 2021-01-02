use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use crate::backend::modules::symbols::exprs::func_call::FuncCallSym;

pub(crate) struct ParenExprSym {
    expr: Box<dyn Symbol>,
}

impl ParenExprSym {

    pub(crate) fn make(expr: &Expression) -> Box<dyn Symbol> {
        match expr {
            Expression::Identifier(name) =>
                FuncCallSym::new(expr.into(), vec![]),
            _ => expr.into()
        }
    }
}