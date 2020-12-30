use crate::backend::modules::symbols::funcs::FunctionSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::module::decls::DeclarationAst;
use crate::parser::ast::module::decls::DeclarationAst::{Function, Value};
use crate::parser::ast::module::ModuleAst;
use anyhow::Result;

#[derive(Debug)]
pub struct Module {
    decls: Vec<Box<dyn Symbol>>,
}

impl Module {
    pub(crate) fn new(module_ast: ModuleAst, scope: Box<dyn Scope>) -> Result<Module> {
        let mut sym_table = SymbolTable::new(&module_ast.get_module_name(), Some(scope));
        for decl in module_ast.get_decls().iter() {
            let sym = Module::define_decl(decl)?;
            sym_table.define(sym);
        }
        let mut decls = sym_table.get_symbols();
        for d in decls.iter_mut() {
            d.resolve_type(&*sym_table);
        }
        Ok(Module { decls })
    }

    fn define_decl(decl: &DeclarationAst) -> Result<Box<dyn Symbol>> {
        let sym: Box<dyn Symbol> = match decl {
            Function(name, args, expr) => FunctionSym::new(name, args, expr),
            Value(name, expr) => ValueSym::new(name, expr),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        };
        Ok(sym)
    }

    pub(crate) fn get_decls(&self) -> Vec<Box<dyn Symbol>> {
        self.decls.to_vec()
    }
}
