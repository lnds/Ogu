use crate::codegen::CodeGenerator;
use crate::parser::ast::module::decls::DeclarationAst;
use crate::parser::ast::module::decls::DeclarationAst::{Function, Value};
use crate::parser::ast::module::ModuleAst;
use crate::symbols::decls::funcs::FunctionSym;
use crate::symbols::decls::values::ValueSym;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Module {
    name: String,
    symbols: HashMap<String, Box<dyn Symbol>>,
    enclosing_scope: Box<dyn Scope>,
}

impl Module {
    pub(crate) fn new(module_ast: &ModuleAst, compiler: Box<dyn Scope>) -> Result<Box<Self>> {
        let mut module = Module {
            name: module_ast.get_module_name(),
            symbols: HashMap::new(),
            enclosing_scope: compiler.clone(),
        };
        for decl in module_ast.get_decls().iter() {
            let sym = Module::define_decl(decl, Box::new(module.clone()))?;
            module.define(sym);
        }
        Ok(Box::new(module))
    }

    fn define_decl(decl: &DeclarationAst, scope: Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        let sym: Box<dyn Symbol> = match decl {
            Function(name, args, expr) => {
                FunctionSym::new(name, args, expr, scope)
            }
            Value(name, expr) => ValueSym::new(name, expr, scope),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        };
        Ok(sym)
    }
}

impl Scope for Module {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        self.symbols.insert(sym.get_name(), sym)
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        match self.symbols.get(name) {
            None => self.enclosing_scope.resolve(name),
            Some(sym) => Some(sym.clone()),
        }
    }

    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        generator.process(self)
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        self.symbols.values().cloned().collect()
    }

    fn set_symbols(&mut self, syms: HashMap<String, Box<dyn Symbol>>) {
        self.symbols = syms
    }
}
