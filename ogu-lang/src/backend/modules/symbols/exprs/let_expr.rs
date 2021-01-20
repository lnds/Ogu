use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Clone, Debug)]
pub(crate) struct LetExpr {
    eqs: Vec<Box<dyn Symbol>>,
    expr: Box<dyn Symbol>,
}

impl LetExpr {
    pub(crate) fn new(eqs: Vec<Box<dyn Symbol>>, expr: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LetExpr { eqs, expr })
    }
}

impl Symbol for LetExpr {
    fn get_name(&self) -> &str {
        "let_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("let", Some(scope.clone_box()));
        sym_table.set_function_name(&scope.function_scope_name());
        for e in self.eqs.iter() {
            if sym_table.define(e.clone()).is_some() {
                bail!("duplicated symbol '{} on let declaration",
                    e.get_name()
                );
            } else {
                e.define_into(scope);
            }
        }
        for e in self.eqs.iter_mut() {
            e.resolve_type(&mut *sym_table)?;
        }
        self.expr.resolve_type(&mut *sym_table)?;

        for e in self.eqs.iter_mut() {
            if let Some(sym) = sym_table.resolve(e.get_name()) {
                e.matches_types(sym.get_type());
            }
        }

        for s in sym_table.get_symbols().iter() {
            scope.define(s.clone());
        }
        Ok(self.get_type())
    }
}