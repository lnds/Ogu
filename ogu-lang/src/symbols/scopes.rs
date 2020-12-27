use crate::codegen::CodeGenerator;
use crate::symbols::Symbol;
use anyhow::Result;
use std::fmt::{Debug, Formatter};

pub trait Scope {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>>;
    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>>;
    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()>;
    fn get_symbols(&self) -> Vec<Box<dyn Symbol>>;
}

impl Debug for dyn Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "Scope {} {{ ", self.scope_name())?;
        for sym in self.get_symbols().iter() {
            writeln!(f, "\t{:?}", sym)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
