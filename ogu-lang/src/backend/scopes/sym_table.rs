use std::collections::HashMap;

use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct SymbolTable {
    name: String,
    enclosing_scope: Option<Box<dyn Scope>>,
    symbol_table: HashMap<String, usize>,
    symbols: Vec<Box<dyn Symbol>>,
    function_name: Option<String>,
}

impl SymbolTable {
    pub(crate) fn new(name: &str, enclosing_scope: Option<Box<dyn Scope>>) -> Box<Self> {
        Box::new(SymbolTable {
            name: name.to_string(),
            enclosing_scope,
            symbol_table: HashMap::new(),
            symbols: vec![],
            function_name: None,
        })
    }
}

impl Scope for SymbolTable {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        if !sym.storable() {
            None
        } else {
            match self.symbol_table.get(sym.get_name()) {
                None => {
                    self.symbols.push(sym.clone_box());
                    let pos = self.symbols.len() - 1;
                    let r = self.symbol_table.insert(sym.get_name().to_string(), pos);
                    match r {
                        None => None,
                        Some(p) => self.symbols.get(p).cloned(),
                    }
                }
                Some(pos) => {
                    self.symbols[*pos] = sym.clone();
                    self.symbols.get(*pos).cloned()
                }
            }
        }
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        match self.symbol_table.get(name) {
            Some(p) => self.symbols.get(*p).cloned(),
            None => match &self.enclosing_scope {
                None => None,
                Some(scope) => scope.resolve(name),
            },
        }
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        self.symbols.to_vec()
    }

    fn set_symbols(&mut self, symbols: Vec<Box<dyn Symbol>>) {
        self.symbols = symbols.to_vec();
        self.symbol_table.clear();
        for (i, sym) in self.symbols.iter().enumerate() {
            self.symbol_table.insert(sym.get_name().to_string(), i);
        }
    }

    fn function_scope_name(&self) -> String {
        match &self.function_name {
            None => String::new(),
            Some(s) => s.to_string(),
        }
    }

    fn set_function_name(&mut self, name: &str) {
        self.function_name = Some(name.to_string())
    }

    fn resolve_seq(&self, seq: Option<Vec<Box<dyn Symbol>>>) -> Option<Vec<Box<dyn Symbol>>> {
        match seq {
            None => None,
            Some(seq) => {
                let mut result = vec![];
                for s in seq.iter() {
                    match self.resolve(s.get_name()) {
                        None => result.push(s.clone()),
                        Some(s) => result.push(s.clone_box()),
                    }
                }
                Some(result)
            }
        }
    }
}
