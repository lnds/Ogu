use std::collections::{HashMap, HashSet};
use crate::symbols::types::Type;
use crate::parser::ast::module::{ModuleAst};
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use crate::parser::ast::module::exposing::Exposing;
use std::iter::FromIterator;

pub(crate) struct Module {
    name: String,
    symbols: HashMap<String, Symbol>,
    /*
    exports: ModuleExports<'a>,
    funcs: HashMap<&'a str, Func>,
    types: HashMap<&'a str, Type>,
    macros: HashMap<&'a str, Macro>,

     */
}

impl Module {

    pub(crate) fn new(module_ast: &ModuleAst) -> Self {
        println!("new module {}", module_ast.get_module_name());
        Module {
            name: module_ast.get_module_name().to_string(),
            symbols: HashMap::new(),
            /*
            exports: ModuleExports::new(module_ast),
            funcs: HashMap::new(),
            types: HashMap::new(),
            macros: HashMap::new(),
            enclosing_scope: Some(scope)

             */
        }
    }
}

impl<'a> Scope for Module {

    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol) {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        unimplemented!()
    }
}


pub(crate) enum ModuleExports<'a> {
    Nothing,
    All,
    Names(HashSet<&'a str>)
}


impl<'a> ModuleExports<'a> {

    pub(crate) fn new(module_ast: &'a ModuleAst<'a>) -> Self {
        match &module_ast.exposing {
            None => ModuleExports::Nothing,
            Some(export) =>
                match export {
                    Exposing::All => ModuleExports::All,
                    Exposing::List(v) => ModuleExports::Names(HashSet::from_iter(v.iter().cloned()))
                }
        }
    }

}

struct Func {

}

struct Macro {

}
