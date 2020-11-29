use crate::parser::ast::module::Module;
use crate::symbols::symbol_table::SymbolTable;
use anyhow::Result;

pub(crate) struct Loader {
    modules: Vec<SymbolTable>,
}

impl Loader {
    pub(crate) fn new() -> Self {
        Loader { modules: vec![] }
    }

    pub(crate) fn add(&mut self, module: &mut Module) -> Result<()> {
        let symbol_table = SymbolTable::new(module)?;
        self.modules.push(symbol_table);
        Ok(())
    }

    pub(crate) fn validate(&self) -> Result<()> {
        Ok(())
    }
}
