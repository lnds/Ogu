use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;

#[derive(Clone)]
pub(crate) struct SymbolTable<'a> {
    name: &'a str,
    enclosing_scope: Option<&'a dyn Scope>,
}

impl<'a> SymbolTable<'a> {
    pub(crate) fn new(name: &'a str, enclosing_scope: Option<&'a dyn Scope>) -> Box<Self> {
        Box::new(SymbolTable {
            name,
            enclosing_scope
        })
    }
}

impl Scope for SymbolTable<'static> {
    fn scope_name(&self) -> &str {
        unimplemented!()
    }

    fn get_enclosing_scope(&self) -> Option<&dyn Scope> {
        unimplemented!()
    }

    fn define(&mut self, sym: &dyn Symbol) -> Option<&dyn Symbol> {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<&dyn Symbol> {
        unimplemented!()
    }
}