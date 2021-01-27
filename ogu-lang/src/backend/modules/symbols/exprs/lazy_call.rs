use anyhow::Result;

use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct LazyExpr {
    expr: Box<dyn Symbol>,
}

impl LazyExpr {
    pub(crate) fn new(expr: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LazyExpr { expr })
    }
}

impl Symbol for LazyExpr {
    fn get_name(&self) -> &str {
        "lazy_call"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty);
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.expr.resolve_type(scope)?;
        Ok(self.get_type())
    }
}
