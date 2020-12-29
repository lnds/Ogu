use crate::codegen::transpilers::SymbolWriter;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::Type;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct TypeAliasSym {
    name: String,
    ty: Box<dyn Type>,
}

impl TypeAliasSym {
    pub fn new(name: &str, ty: Box<dyn Type>) -> Box<Self> {
        Box::new(TypeAliasSym {
            name: name.to_string(),
            ty: ty.clone(),
        })
    }
}

impl Symbol for TypeAliasSym {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        Some(self.ty.clone())
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        unimplemented!()
    }

    fn solve_type(&self, _scope: Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        unimplemented!()
    }
}
