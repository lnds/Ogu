use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct IfExpr {
    cond: Box<dyn Symbol>,
    then_expr: Box<dyn Symbol>,
    else_expr: Box<dyn Symbol>,
}

impl IfExpr {
    pub(crate) fn new(
        cond: Box<dyn Symbol>,
        then_expr: Box<dyn Symbol>,
        else_expr: Box<dyn Symbol>,
    ) -> Box<Self> {
        Box::new(IfExpr {
            cond,
            then_expr,
            else_expr,
        })
    }
}

impl Symbol for IfExpr {
    fn get_name(&self) -> &str {
        "if_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let tt = self.then_expr.get_type()?;
        let et = self.else_expr.get_type()?;
        if &*tt != &*et {
            None
        } else {
            Some(et.clone())
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.cond.resolve_type(scope)?;
        self.then_expr.resolve_type(scope)?;
        self.else_expr.resolve_type(scope)?;
        Ok(self.get_type())
    }

}
