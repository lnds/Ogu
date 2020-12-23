use crate::symbols::{Symbol, SymbolValue};

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Add,
    Mul,
}

impl Expr {
    pub(crate) fn make(name: &str, op: Expr, left: SymbolValue, right: SymbolValue) -> Symbol {
        Symbol::new(name, SymbolValue::BinExpr(op, Box::new(left), Box::new(right)))
    }
}

pub(crate) struct Ref {}

impl Ref {

    pub(crate) fn make(name: &str) -> Symbol {
        Symbol::new(name, SymbolValue::Ref(name.to_string()))
    }
}