use crate::parser::ast::module::ModuleAst;
use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::module::decls::DeclarationAst;
use crate::parser::ast::module::decls::DeclarationAst::{Function, Value};
use crate::backend::scopes::scopes::Scope;
use crate::backend::modules::symbols::values::ValueSym;
use anyhow::Result;

#[derive(Debug)]
pub struct Module {
    decls: Vec<Box<dyn Symbol>>
}

impl  Module {
   pub(crate) fn new(module_ast: ModuleAst, scope: Box<dyn Scope>) -> Result<Module> {
      let mut decls = vec![];
       for decl in module_ast.get_decls().iter() {
          decls.push(Module::define_decl(decl, &*scope)?)
       }
      Ok(Module {
         decls
      })
   }

   fn define_decl(decl: &DeclarationAst, scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
      let sym: Box<dyn Symbol> =
          match decl {
         /*Function(name, args, expr) => {
            //FunctionSym::new(name, args, expr, scope)
            todo!()
         }*/
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