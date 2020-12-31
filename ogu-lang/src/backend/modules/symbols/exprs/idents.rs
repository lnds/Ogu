use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;

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

    fn resolve_type(&mut self, scope: &dyn Scope) -> Option<Box<dyn Type>> {
        match &self.ty {
            Some(ty) => Some(ty.clone()),
            None => {
                let sym = scope.resolve(&self.name)?;
                self.ty = sym.get_type();
                sym.get_type()
            }
        }
    }
}
