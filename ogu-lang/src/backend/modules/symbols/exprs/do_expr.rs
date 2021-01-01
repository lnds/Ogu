use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Result, Error};
use crate::backend::errors::OguError;

#[derive(Debug, Clone)]
pub(crate) struct DoExprSym {
    exprs: Vec<Box<dyn Symbol>>,
}


impl DoExprSym {

    pub(crate) fn new(exprs: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(DoExprSym {
            exprs
        })
    }
}

impl Symbol for DoExprSym {
    fn get_name(&self) -> &str {
        "do_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let last = self.exprs.iter().last()?;
        last.get_type()
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        for e in self.exprs.iter_mut() {
            e.resolve_type(scope)?;
        }
        match self.get_type() {
            None => Err(Error::new(OguError::SymbolTableError).context(format!("cound not resolve {:?}", self))),
            Some(t) => Ok(Some(t))
        }
    }
}