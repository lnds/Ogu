use crate::parser::ast::module::ModuleAst;
use anyhow::Result;
use crate::symbols::SymbolTable;
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use crate::symbols::types::Type;
use crate::symbols::module::Module;

pub(crate) struct Loader {
    current_scope: Option<Box<dyn Scope>>,
}


impl Loader {

    pub(crate) fn new() -> Self {
        let mut symbol_table = Box::new(SymbolTable::new("_ogu"));
        symbol_table.define(Symbol::Macro("printf!", Type::Unit, 1));
        symbol_table.define(Symbol::Macro("print!", Type::Unit, 1));
        Loader {
            current_scope: Some(symbol_table),
        }
    }

    pub(crate) fn get_scope(&mut self) -> Box<dyn Scope> {
        self.current_scope.take().unwrap()
    }


    pub(crate) fn push_scope(&mut self,  new_scope: Box<dyn Scope>) {
            self.current_scope = Some(new_scope);
    }

    pub(crate) fn show_scope(&self) {
        match &self.current_scope {
            Nome => println!("NADA"),
            Some(scope) => println!("SCOPE({})", scope.scope_name())
        }

    }

/*
    pub(crate) fn parse(&self, _module: &ModuleAst) -> Result<()> {
        match &self.current_scope {
            None => println!("NO SCOPE"),
            Some(scope) => println!("current_scope = {}", scope.scope_name())
        }

        todo!()
    }

 */
}
