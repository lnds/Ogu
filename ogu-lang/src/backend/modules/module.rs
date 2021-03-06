use crate::backend::modules::symbols::func::func_def::Function;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;
use crate::parser::ast::module::decls::DeclarationAst;
use crate::parser::ast::module::ModuleAst;
use anyhow::{bail, Result};

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
                bail!(
                    "Duplicated definition not allowed for symol = {}",
                    sym.get_name()
                );
            }
        }
        let mut decls = sym_table.get_symbols();
        for d in decls.iter_mut() {
            d.resolve_type(&mut *sym_table)?;
            sym_table.define(d.clone_box()); // redefine symbol
        }
        Ok(Module { decls })
    }

    fn define_decl(decl: &DeclarationAst) -> Result<Box<dyn Symbol>> {
        match decl {
            DeclarationAst::FunctionWithGuards(_, _, _, _) => {
                bail!("internal error, function with guard leaked")
            }
            DeclarationAst::Function(name, args, expr, ft) => Function::make(name, args, expr, ft),
            DeclarationAst::Value(name, expr) => Ok(ValueSym::new(name, expr)),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        }
    }

    pub(crate) fn get_decls(&self) -> Vec<Box<dyn Symbol>> {
        self.decls.to_vec()
    }
}
