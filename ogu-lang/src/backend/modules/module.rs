use crate::backend::errors::OguError;
use crate::backend::modules::symbols::funcs::FunctionSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;
use crate::parser::ast::module::decls::DeclarationAst;
use crate::parser::ast::module::decls::DeclarationAst::{Function, Value};
use crate::parser::ast::module::ModuleAst;
use anyhow::{Error, Result};

#[derive(Debug)]
pub struct Module {
    decls: Vec<Box<dyn Symbol>>,
}

impl Module {
    pub(crate) fn new(module_ast: ModuleAst, scope: Box<dyn Scope>) -> Result<Module> {
        let mut sym_table = SymbolTable::new(&module_ast.get_module_name(), Some(scope));
        for decl in module_ast.get_decls().iter() {
            let sym = Module::define_decl(decl)?;
            if sym_table.define(sym.clone()).is_some() {
                return Err(Error::new(OguError::SymbolTableError).context(format!(
                    "Duplicated definition not allowed for symol = {}",
                    sym.get_name()
                )));
            }
        }
        let mut decls = sym_table.get_symbols();
        //println!("DECLS ANTES: {:#?}", decls);
        for d in decls.iter_mut() {
            d.resolve_type(&mut *sym_table)?;
            sym_table.define(d.clone_box()); // redefine symbol
        }
        Ok(Module { decls })
    }

    fn define_decl(decl: &DeclarationAst) -> Result<Box<dyn Symbol>> {
        Ok(decl.into())
    }

    pub(crate) fn get_decls(&self) -> Vec<Box<dyn Symbol>> {
        self.decls.to_vec()
    }
}

impl<'a> From<&DeclarationAst<'a>> for Box<dyn Symbol> {
    fn from(decl: &DeclarationAst<'a>) -> Self {
        match decl {
            Function(name, args, expr) => FunctionSym::new(name, args, expr),
            Value(name, expr) => ValueSym::new(name, expr),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        }
    }
}
