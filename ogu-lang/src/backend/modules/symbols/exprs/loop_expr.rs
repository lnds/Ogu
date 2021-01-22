use crate::backend::scopes::symbol::{Symbol, SymbolClone};
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::scopes::sym_table::SymbolTable;

#[derive(Clone, Debug)]
pub(crate) struct LoopExpr {
    pub(crate) decls: Vec<Box<dyn Symbol>>,
    pub(crate) expr: Box<dyn Symbol>,
}

impl LoopExpr {
    pub(crate) fn new(
        decls: Vec<Box<dyn Symbol>>,
        expr: Box<dyn Symbol>,
    ) -> Box<Self> {
        Box::new(LoopExpr {
            decls,
            expr,
        })
    }
}

impl Symbol for LoopExpr {
    fn get_name(&self) -> &str {
        "loop"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("loop", Some(scope.clone_box()));
        sym_table.define(self.clone_box());
        for d in self.decls.iter() {
            if sym_table.define(d.clone()).is_some() {
                bail!("duplicated symbol '{} on loop for declaration", d.get_name());
            }
        }
        for d in self.decls.iter_mut() {
            d.resolve_type(&mut *sym_table)?;
        }
        self.expr.resolve_type(&mut *sym_table)?;
        for d in sym_table.get_symbols() {
            d.define_into(scope);
        }
        Ok(self.get_type())
    }

    fn storable(&self) -> bool {
        true
    }
}

