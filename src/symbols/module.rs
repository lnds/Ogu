use crate::backend::Compiler;
use crate::lexer::tokens::Token::HashCurly;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::body::{BodyAst, Declaration};
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::ModuleAst;
use crate::symbols::scopes::Scope;
use crate::symbols::types::Type;
use crate::symbols::values::Date;
use crate::symbols::Symbol;
use anyhow::Result;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

pub(crate) struct Module {
    name: String,
    symbols: HashMap<String, Symbol>,
}

impl Module {
    pub(crate) fn new(module_ast: &ModuleAst) -> Result<Self> {
        println!("new module {}", module_ast.get_module_name());
        let mut module = Module {
            name: module_ast.get_module_name(),
            symbols: HashMap::new()
        };
        for decl in module_ast.body.declarations.iter() {
            match decl {
                Declaration::Value(name, Expression::DateLiteral(date)) => {
                    module.define(Date::make(name, date.to_string()))
                }
                _ => {
                    todo!()
                }
            };
        }
        Ok(module)
    }
}

impl<'a> Scope for Module {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol) {
        self.symbols.insert(sym.get_name(), sym);
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        unimplemented!()
    }

    fn dump(&self) {
        println!("Module Scope: {}", self.scope_name());
        println!("Symbols:");
        println!("{:#?}", self.symbols);
    }
}

