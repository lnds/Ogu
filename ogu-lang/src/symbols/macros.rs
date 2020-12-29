use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::basic::BasicType;
use crate::types::Type;
use anyhow::Result;
use std::fs::File;

#[derive(Clone)]
pub(crate) struct MacroSym {
    name: String,
    ty: Option<Box<dyn Type>>,
}

impl MacroSym {
    pub fn new(name: &str, ty: BasicType) -> Box<Self> {
        Box::new(MacroSym {
            name: name.to_string(),
            ty: Some(Box::new(ty)),
        })
    }
}

impl Symbol for MacroSym {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        Box::new(self.clone())
    }

    fn solve_type(&self, _scope: Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        unimplemented!()
    }
}

impl SymbolWriter for MacroSym {
    fn write_symbol(&self, _fmt: &dyn Formatter, _file: &mut File) -> Result<()> {
        unimplemented!()
    }
}
