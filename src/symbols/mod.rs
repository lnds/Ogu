pub(crate) mod symbols;
pub(crate) mod scopes;
pub(crate) mod types;
pub(crate) mod loader;
pub (crate) mod module;

use crate::backend::OguError;
use anyhow::{Context, Error, Result};
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use std::collections::HashMap;

pub(crate) struct SymbolTable<'a> {
    name: String,
    enclosing_scope: Option<Box<dyn Scope>>,
    symbols: HashMap<&'a str, Symbol>,
}

impl<'a> SymbolTable<'a> {

    pub(crate) fn new(name: &'a str) -> Self {
        SymbolTable { name: name.to_string(), enclosing_scope: None, symbols: HashMap::new() }
    }


}

impl<'a> Scope for SymbolTable<'a> {


    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol) {
        self.symbols.insert(sym.get_name(), sym.clone());
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.symbols.get(&name) {
            Some(s) => Some(s.clone()),
            None =>
                match &self.enclosing_scope {
                    None => None,
                    Some(scope) => scope.resolve(name)
                }
        }
    }
}

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError(msg.to_string()))).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}
