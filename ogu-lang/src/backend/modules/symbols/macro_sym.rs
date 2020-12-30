use crate::backend::scopes::types::Type;
use crate::backend::scopes::symbol::Symbol;

#[derive(Clone)]
pub(crate) struct MacroSym<'a> {
    name: &'a str,
    ty : Option<Box<dyn Type>>,
}

impl<'a> MacroSym<'a> {

    pub(crate) fn new(name: &'a str, ty: Option<Box<dyn Type>>) -> Self {
        MacroSym {
            name,
            ty
        }
    }
}

impl<'a> Symbol for MacroSym<'static> {
    fn get_name(&self) -> &str {
        self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
       self.ty = ty.clone()
    }
}