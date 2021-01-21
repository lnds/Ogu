use crate::backend::errors::OguError;
use crate::backend::modules::types::basic_type::INVALID_TYPE;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct LoopExpr {
    decls: Option<Vec<Box<dyn Symbol>>>,
    cond: Option<Box<dyn Symbol>>,
    expr: Box<dyn Symbol>,
    ret_expr: Option<Box<dyn Symbol>>,
}

impl LoopExpr {
    pub(crate) fn new(
        decls: Option<Vec<Box<dyn Symbol>>>,
        cond: Option<Box<dyn Symbol>>,
        expr: Box<dyn Symbol>,
        ret_expr: Option<Box<dyn Symbol>>
    ) -> Box<Self> {
        Box::new(LoopExpr {
            decls,
            cond,
            expr,
            ret_expr
        })
    }
}

impl Symbol for LoopExpr {
    fn get_name(&self) -> &str {
        "loop_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        Ok(self.get_type())
    }
}
