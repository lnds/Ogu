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
            ty: None
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
}