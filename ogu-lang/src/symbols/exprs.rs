use crate::symbols::{Symbol, SymbolValue};

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Add,
    Mul,
}

impl Expr {
    pub(crate) fn make(name: &str, op: Expr, left: SymbolValue, right: SymbolValue) -> Symbol {
        Symbol::new(
            name,
            SymbolValue::BinExpr(op, Box::new(left), Box::new(right)),
        )
    }
}

pub(crate) enum Func {}

impl Func {
    pub(crate) fn make(name: &str, args: SymbolValue, expr: SymbolValue) -> Symbol {
        Symbol::new(name, SymbolValue::FuncDecl(Box::new(args), Box::new(expr)))
    }
}
