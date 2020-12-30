use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Clone)]
pub(crate) struct SymbolTable<'a> {
    name: &'a str,
    enclosing_scope: Option<&'a dyn Scope>,
    symbols: HashMap<String, Box<dyn Symbol>>
}

impl<'a> SymbolTable<'a> {
    pub(crate) fn new(name: &'a str, enclosing_scope: Option<&'a dyn Scope>) -> Box<Self> {
        Box::new(SymbolTable {
            name,
            enclosing_scope,
            symbols: HashMap::new()
        })
    }
}

impl Scope for SymbolTable<'static> {
    fn scope_name(&self) -> &str {
        self.name
    }

    fn get_enclosing_scope(&self) -> Option<&dyn Scope> {
        self.enclosing_scope.clone()
    }

    fn define(&mut self, sym: &dyn Symbol) -> Option<Box<dyn Symbol>> {
        self.symbols.insert(sym.get_name().to_string(),
                                         sym.clone_box())
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        self.symbols.get(name).cloned()
    }
}