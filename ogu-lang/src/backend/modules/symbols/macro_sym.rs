use crate::backend::scopes::types::Type;
use crate::backend::scopes::symbol::Symbol;

#[derive(Clone, Debug)]
pub(crate) struct MacroSym {
    name: &'static str,
    ty : Option<Box<dyn Type>>,
}

impl MacroSym {

    pub(crate) fn new(name: &'static str, ty: Option<Box<dyn Type>>) -> Self {
        MacroSym {
            name,
            ty
        }
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
}