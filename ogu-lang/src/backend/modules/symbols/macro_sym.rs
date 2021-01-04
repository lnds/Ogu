use crate::backend::errors::OguError;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct MacroSym {
    name: &'static str,
    ty: Option<Box<dyn Type>>,
}

impl MacroSym {
    pub(crate) fn new(name: &'static str, ty: Option<Box<dyn Type>>) -> Box<Self> {
        Box::new(MacroSym { name, ty })
    }
}

impl Symbol for MacroSym {
    fn get_name(&self) -> &'static str {
        self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match &self.ty {
            Some(ty) if !ty.is_trait() => Ok(Some(ty.clone())),
            _ => match scope.resolve(self.name) {
                None => Err(Error::new(OguError::SymbolTableError)
                    .context(format!("{} not found", self.name))),
                Some(sym) => {
                    self.ty = sym.get_type();
                    Ok(self.get_type())
                }
            },
        }
    }

    fn storable(&self) -> bool {
        true
    }

}
