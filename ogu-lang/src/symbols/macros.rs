use crate::types::Type;
use crate::symbols::Symbol;
use crate::types::basic::BasicType;

#[derive(Clone)]
pub(crate) struct MacroSym {
    name: String,
    ty: Box<dyn Type>,
}

impl MacroSym {

    pub fn new(name: &str, ty: BasicType) -> Box<Self> {
        Box::new(MacroSym {
            name: name.to_string(),
            ty: Box::new(ty)
        })
    }
}

impl Symbol for MacroSym {
    fn get_name(&self) -> String {
       self.name.to_string()
    }

    fn get_type(&self) -> Box<dyn Type> {
        self.ty.clone()
    }
}