use crate::backend::errors::OguError;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct IdSym {
    name: String,
    ty: Option<Box<dyn Type>>,
}

impl IdSym {
    pub(crate) fn new(id: &str) -> Box<Self> {
        Box::new(IdSym {
            name: id.to_string(),
            ty: None,
        })
    }
}

impl Symbol for IdSym {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match &self.ty {
            Some(ty) => Ok(Some(ty.clone())),
            None => match scope.resolve(&self.name) {
                None => Err(Error::new(OguError::SymbolTableError)
                    .context(format!("Symbol not found : {}", self.name))),
                Some(sym) => {
                    self.ty = sym.get_type();
                    Ok(self.get_type())
                }
            },
        }
    }
}
