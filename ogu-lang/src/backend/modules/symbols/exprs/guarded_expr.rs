use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct GuardedExpr {
    expr: Box<dyn Symbol>,
    guard: Box<dyn Symbol>,
}

impl GuardedExpr {
    pub(crate) fn new(expr: Box<dyn Symbol>, guard: Box<dyn Symbol>) -> Box<Self> {
        Box::new(GuardedExpr { expr, guard })
    }
}

impl Symbol for GuardedExpr {
    fn get_name(&self) -> &str {
        "guarded_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.guard.resolve_type(scope)?;
        self.expr.resolve_type(scope)?;
        Ok(self.get_type())
    }
}
