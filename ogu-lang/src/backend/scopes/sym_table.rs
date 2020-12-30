use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use std::collections::HashMap;

#[derive(Clone)]
pub(crate) struct SymbolTable {
    name: String,
    enclosing_scope: Option<Box<dyn Scope>>,
    symbol_table: HashMap<String, usize>,
    symbols: Vec<Box<dyn Symbol>>
}

impl SymbolTable {
    pub(crate) fn new(name: &str, enclosing_scope: Option<Box<dyn Scope>>) -> Box<Self> {
        Box::new(SymbolTable {
            name: name.to_string(),
            enclosing_scope,
            symbol_table: HashMap::new(),
            symbols: vec![]
        })
    }
}

impl Scope for SymbolTable {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn get_enclosing_scope(&self) ->  Option<Box<dyn Scope>> {
        self.enclosing_scope.clone()
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        self.symbols.push(sym.clone_box());
        let pos = self.symbols.len() - 1;
        let r = self.symbol_table.insert(sym.get_name().to_string(), pos);
        match r {
            None => None,
            Some(p) => {
                self.symbols.get(p).cloned()
            }
        }
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        match self.symbol_table.get(name) {
            Some(p) => self.symbols.get(*p).cloned(),
            None => {
                match &self.enclosing_scope {
                    None => None,
                    Some(scope) => scope.resolve(name)
                }
            }
        }
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        self.symbols.to_vec()
    }

}