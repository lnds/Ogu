pub mod symbols;
pub mod scopes;
pub mod types;
pub mod loader;

use crate::backend::OguError;
use anyhow::{Context, Error, Result};
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    name: String,
    enclosing_scope: Option<Box<dyn Scope<'a> + 'a>>,
    symbols: HashMap<&'a str, Symbol>,
}

impl<'a> SymbolTable<'a> {

    pub(crate) fn new(name: &'a str) -> Self {
        SymbolTable { name: name.to_string(), enclosing_scope: None, symbols: HashMap::new() }
    }


}

impl<'a> Scope<'a> for SymbolTable<'a> {

    fn push(self: Box<Self>, name: &str) -> Box<dyn Scope<'a> + 'a> {
        Box::new(SymbolTable {
            name: name.to_string(),
            enclosing_scope: Some(self),
            symbols: HashMap::new(),
        })
    }

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
