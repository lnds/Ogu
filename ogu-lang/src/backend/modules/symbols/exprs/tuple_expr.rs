use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::Expression;
use anyhow::Result;
use crate::backend::scopes::sym_table::SymbolTable;

#[derive(Debug, Clone)]
pub(crate) struct TupleExprSym {
    tuple: Vec<Box<dyn Symbol>>,
}

impl TupleExprSym {
    pub(crate) fn make(tuple: Vec<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Box::new(TupleExprSym { tuple })
    }
}

impl Symbol for TupleExprSym {
    fn get_name(&self) -> &str {
        "tuple_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let ty = TupleType::new(
            self.tuple
                .iter()
                .map(|sym| sym.get_type().unwrap_or_else(|| TRAIT_UNKNOWN.clone_box()))
                .collect(),
        );
        let ty: Box<dyn Type> = Box::new(ty);
        Some(ty)
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        if let Some(ty) = ty {
            if let Some(ty) = ty.downcast_ref::<TupleType>() {
                let types = ty.get_tuple();
                for (p, sym) in self.tuple.iter_mut().enumerate() {
                    sym.set_type(types.get(p).cloned())
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("tuple", Some(scope.clone_box()));
        for s in self.tuple.iter() {
            sym_table.define(s.clone());
        }
        for s in self.tuple.iter_mut() {
            s.resolve_type(&mut *sym_table)?;
        }
        for s in sym_table.get_symbols().iter() {
            scope.define(s.clone());
        }
        Ok(self.get_type())
    }
}