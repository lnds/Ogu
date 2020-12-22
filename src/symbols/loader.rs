use crate::parser::ast::module::Module;
use anyhow::Result;
use crate::symbols::SymbolTable;
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use crate::symbols::types::Type;

pub(crate) struct Loader<'a> {
    current_scope: Option<Box<dyn Scope<'a> + 'a>>,
}

impl<'a> Loader<'a> {

    pub(crate) fn new() -> Loader<'a> {
        let mut symbol_table = SymbolTable::new("_ogu");
        symbol_table.define(Symbol::Macro("printf!", Type::Unit, 1));
        symbol_table.define(Symbol::Macro("print!", Type::Unit, 1));
        Loader {
            current_scope: Some(Box::new(symbol_table)),
        }
    }

    pub(crate) fn add(&mut self, module: &Module)  {
        if let Some(scope) = self.current_scope.take() {
            let name = module.get_module_name();
            self.current_scope = Some(scope.push(&name))
        }
    }

    pub(crate) fn parse(&self, module: &Module) -> Result<()> {
        match &self.current_scope {
            None => println!("NO SCOPE"),
            Some(scope) => println!("current_scope = {}", scope.scope_name())
        }
        
        todo!()
    }
}
