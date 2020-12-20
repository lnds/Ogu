pub mod symbols;
pub mod scopes;
pub mod types;

use crate::backend::OguError;
use anyhow::{Context, Error, Result};
use crate::symbols::scopes::Scope;
use std::ops::Deref;
use crate::symbols::symbols::Symbol;
use std::collections::HashMap;


pub struct SymbolTable {
    name: &'static str,
    enclosing_scope: Option<&'static dyn Scope>,
    symbols: HashMap<&'static str, Symbol>,
}

impl SymbolTable {
    pub fn new(name: &'static str) -> Self {
        SymbolTable { name, enclosing_scope: None, symbols: HashMap::new() }
    }

}

impl Scope for SymbolTable {

    fn scope_name(&self) -> &'static str {
        self.name
    }

    fn push(&mut self, scope: &'static dyn Scope) {
        self.enclosing_scope = Some(scope);
    }

    fn pop(&mut self) -> Option<&'static dyn Scope> {
        self.enclosing_scope.map(|s| s)
    }

    fn define(&mut self, sym: Symbol) {
        self.symbols.insert(sym.get_name(), sym.clone());
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        self.symbols.get(&name).cloned()
    }
}

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError(msg.to_string()))).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}
