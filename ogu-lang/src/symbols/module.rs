use crate::codegen::CodeGenerator;
use crate::parser::ast::module::body::Declaration;
use crate::parser::ast::module::ModuleAst;
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
    pub(crate) fn new(module_ast: &ModuleAst, compiler: Box<dyn Scope>) -> Result<Self> {
        let mut module = Module {
            name: module_ast.get_module_name(),
            symbols: HashMap::new(),
            enclosing_scope: compiler.clone(),
        };
        for decl in module_ast.get_decls().iter() {
            Module::define_decl(decl, &mut module)?;
        }
        Ok(module)
    }

    fn define_decl(decl: &Declaration, module: &mut dyn Scope) -> Result<()> {
        let sym = decl.clone().into();
        module.define(sym);
        Ok(())
    }

    pub(crate) fn solve_symbols_types(&self) -> Result<HashMap<String, Box<dyn Symbol>>> {
        let mut symbols = HashMap::new();
        println!("solve symbol types");
        for (name, sym) in self.symbols.iter() {
            println!("solve type for {} <- {:?}", name, sym);
            let result = sym.solve_type(self)?;
            println!("found = {:?}", result);
            symbols.insert(name.clone(), result);
        }
        Ok(symbols)
    }

    pub(crate) fn set_symbols(&mut self, symbols: HashMap<String, Box<dyn Symbol>>) {
        self.symbols = symbols
    }
}

impl<'a> Scope for Module {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        self.symbols.insert(sym.get_name(), sym)
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        println!("Module resolve name: {}", name);
        match self.symbols.get(name) {
            None => self.enclosing_scope.resolve(name),
            Some(sym) => Some(sym.clone())
        }
    }

    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        generator.process(self)
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        self.symbols.values().cloned().collect()
    }
}