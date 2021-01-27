use anyhow::{bail, Result};

use crate::backend::modules::symbols::exprs::loop_expr::LoopExpr;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct RepeatExpr {
    reps: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl RepeatExpr {
    pub(crate) fn new(reps: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(RepeatExpr {
            reps,
            ty: Some(TRAIT_UNKNOWN.clone_box()),
        })
    }
}

impl Symbol for RepeatExpr {
    fn get_name(&self) -> &str {
        "repeat"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let recurse = scope.scope_name() == "loop";
        if let Some(loop_expr_sym) = scope.resolve("loop") {
            match loop_expr_sym.downcast_ref::<LoopExpr>() {
                None => bail!("repeat can only be called inside a loop"),
                Some(loop_expr) => {
                    let mut loop_expr = loop_expr.clone();
                    if loop_expr.decls.len() != self.reps.len() {
                        bail!("return expression doesn't match for declarations")
                    }
                    let mut sym_table = SymbolTable::new("repeat", Some(scope.clone_box()));
                    for (i, r) in self.reps.iter_mut().enumerate() {
                        match r.get_type() {
                            None => match loop_expr.decls[i].get_type() {
                                None => {
                                    r.resolve_type(scope)?;
                                    loop_expr.decls[i].set_type(r.get_type());
                                }
                                Some(t) => {
                                    r.set_type(Some(t));
                                }
                            },
                            Some(_) => match loop_expr.decls[i].get_type() {
                                None => {
                                    loop_expr.decls[i].set_type(r.get_type());
                                }
                                Some(t) => loop_expr.decls[i].matches_types(Some(t.clone_box())),
                            },
                        }
                        loop_expr.decls[i].define_into(&mut *sym_table);
                        loop_expr.decls[i].define_into(scope);
                    }
                    if recurse {
                        if loop_expr.expr.get_name() == "repeat" {
                            self.ty = loop_expr.decls[0].get_type();
                        } else {
                            loop_expr.expr.resolve_type(&mut *sym_table)?;
                            self.set_type(loop_expr.expr.get_type());
                        }
                    }

                    Ok(self.get_type())
                }
            }
        } else {
            bail!("repeat can only be called inside a loop")
        }
    }
}
