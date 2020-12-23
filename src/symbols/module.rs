use crate::backend::Compiler;
use crate::lexer::tokens::Token::{HashCurly, Mod};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::body::{BodyAst, Declaration};
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::ModuleAst;
use crate::symbols::scopes::Scope;
use crate::symbols::types::Type;
use crate::symbols::values::{Date, Int};
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
            Module::match_decl(decl, &mut module)?;
        }
        Ok(module)
    }


    fn match_decl(decl: &Declaration, module: &mut Module) -> Result<()> {
        match decl {
            Declaration::Value(name, Expression::DateLiteral(date)) => {
                module.define(Date::make(name, date))
            }
            Declaration::Value(name, Expression::IntegerLiteral(int)) => {
                module.define(Int::make(name, int))
            }
            Declaration::Value(name, Expression::AddExpr(left, right)) => {
                println!("{:?} + {:?}", left, right);
     //           Module::match_expr(left, module);
                //module.define(Int::make(name, int))
                todo!()
            }
            _ => {
                todo!()
            }
        };
        Ok(())
    }
}

impl<'a> Scope for Module {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol) -> Option<Symbol> {
        self.symbols.insert(sym.get_name(), sym)
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

