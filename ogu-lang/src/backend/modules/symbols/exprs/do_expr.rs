use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct DoExpr {
    exprs: Vec<Box<dyn Symbol>>,
}

impl DoExpr {
    pub(crate) fn new(exprs: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(DoExpr { exprs })
    }
}

impl Symbol for DoExpr {
    fn get_name(&self) -> &str {
        "do_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let last = self.exprs.iter().last()?;
        last.get_type()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        for e in self.exprs.iter_mut() {
            e.resolve_type(scope)?;
        }
        Ok(self.get_type())
    }
}
