use crate::codegen::CodeGenerator;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Clone)]
pub(crate) struct SymbolTable {
    name: String,
    enclosing_scope: Option<Box<dyn Scope>>,
    symbols: HashMap<String, Box<dyn Symbol>>,
}

impl SymbolTable {
    pub(crate) fn new(name: &str) -> Self {
        SymbolTable {
            name: name.to_string(),
            enclosing_scope: None,
            symbols: HashMap::new(),
        }
    }
}

impl Scope for SymbolTable {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        self.symbols.insert(sym.get_name(), sym)
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        match self.symbols.get(name) {
            Some(s) => Some(s.clone()),
            None => match &self.enclosing_scope {
                None => None,
                Some(scope) => scope.resolve(name),
            },
        }
    }

    fn gen_code(&self, _generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        // do nothing
        Ok(())
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        self.symbols.values().cloned().collect()
    }
}
