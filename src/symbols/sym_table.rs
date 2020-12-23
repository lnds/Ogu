use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use std::collections::HashMap;

pub(crate) struct SymbolTable {
    name: String,
    enclosing_scope: Option<Box<dyn Scope>>,
    symbols: HashMap<String, Symbol>,
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

    fn define(&mut self, sym: Symbol) {
        self.symbols.insert(sym.get_name(), sym);
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.symbols.get(name) {
            Some(s) => Some(s.clone()),
            None => match &self.enclosing_scope {
                None => None,
                Some(scope) => scope.resolve(name),
            },
        }
    }

    fn dump(&self) {
        println!("Scope: {}", self.name);
        println!("Symbols:");
        println!("{:?}", self.symbols);
    }
}
