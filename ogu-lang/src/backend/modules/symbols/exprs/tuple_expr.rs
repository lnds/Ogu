use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Debug, Clone)]
pub(crate) struct TupleExpr {
    pub(crate) tuple: Vec<Box<dyn Symbol>>,
    assignable: bool,
}

impl TupleExpr {
    pub(crate) fn make(tuple: Vec<Box<dyn Symbol>>) -> Box<dyn Symbol> {
        Box::new(TupleExpr {
            tuple,
            assignable: false,
        })
    }
}

impl Symbol for TupleExpr {
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

    fn matches_types(&mut self, ty: Option<Box<dyn Type>>) {
        if let Some(ty) = ty {
            if let Some(ty) = ty.downcast_ref::<TupleType>() {
                let other_tuple_vec: Vec<Box<dyn Type>> = ty.get_tuple();
                for (s, t) in self.tuple.iter_mut().zip(other_tuple_vec.iter()) {
                    match s.get_type() {
                        None => s.set_type(Some(t.clone())),
                        Some(_) => {
                            s.matches_types(Some(t.clone()));
                        }
                    }
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("tuple", Some(scope.clone_box()));
        sym_table.set_function_name(&scope.function_scope_name());
        if self.assignable {
            for s in self.tuple.iter() {
                if sym_table.define(s.clone()).is_some() {
                    bail!("symbol {:?} duplicated in tuple", s.get_name());
                }
            }
        }
        for s in self.tuple.iter_mut() {
            s.resolve_type(&mut *sym_table)?;
        }
        if self.assignable {
            for s in sym_table.get_symbols().iter() {
                scope.define(s.clone());
            }
        }
        Ok(self.get_type())
    }

    fn storable(&self) -> bool {
        self.assignable
    }

    fn set_storable(&mut self, s: bool) {
        self.assignable = s;
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        for s in self.tuple.iter() {
            s.define_into(scope);
        }
        None
    }

    fn is_seq(&self) -> bool {
        true
    }

    fn get_seq(&self) -> Option<Vec<Box<dyn Symbol>>> {
        Some(self.tuple.to_vec())
    }

    fn set_seq(&mut self, seq: Option<Vec<Box<dyn Symbol>>>) {
        if let Some(seq) = seq {
            self.tuple = seq;
        }
    }
}
