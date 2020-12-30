use crate::backend::scopes::types::Type;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct MacroSym {
    name: &'static str,
    ty : Option<Box<dyn Type>>,
}

impl MacroSym {

    pub(crate) fn new(name: &'static str, ty: Option<Box<dyn Type>>) -> Box<Self> {
        Box::new(MacroSym {
            name,
            ty
        })
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

    fn resolve_type(&mut self, scope: &dyn Scope) -> Option<Box<dyn Type>> {
        match &self.ty {
            Some(ty) => Some(ty.clone()),
            None => {
                let sym = scope.resolve(self.name)?;
                self.ty = sym.get_type();
                self.get_type()
            }
        }
    }
}