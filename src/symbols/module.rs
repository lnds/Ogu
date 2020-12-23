use std::collections::{HashMap, HashSet};
use crate::symbols::types::Type;
use crate::parser::ast::module::{ModuleAst};
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::{Symbol, SymbolValue};
use crate::parser::ast::module::exposing::Exposing;
use std::iter::FromIterator;
use crate::backend::Compiler;
use crate::parser::ast::module::body::{BodyAst, Declaration};
use crate::lexer::tokens::Token::HashCurly;

pub(crate) struct Module {
    name: String,
    symbols: HashMap<String, Symbol>,
    //enclosing_scope: Box<dyn Scope>,
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
        let mut symbols = HashMap::new();
        for decl in module_ast.body.declarations.iter() {
            match decl {
                Declaration::Value(name, expr) => {
                    symbols.insert(name.to_string(), Symbol::new(name, SymbolValue::Value(format!("{:?}", expr))));
                }
                _ => {}
            };
        }
        Module {
            name: module_ast.get_module_name().to_string(),
            symbols,
        }
    }


}

impl<'a> Scope for Module {

    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol){
        self.symbols.insert(sym.get_name(), sym);
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        unimplemented!()
    }

    fn dump(&self) {
        println!("Module Scope: {}", self.scope_name());
        println!("Symbols:");
        println!("{:?}", self.symbols);
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
