use crate::backend::errors::OguError;
use crate::backend::modules::types::basic_type::INVALID_TYPE;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::symbols::exprs::loop_expr::LoopExpr;
use crate::backend::scopes::sym_table::SymbolTable;

#[derive(Clone, Debug)]
pub(crate) struct RepeatExpr {
    reps: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl RepeatExpr {
    pub(crate) fn new(
        reps: Vec<Box<dyn Symbol>>
    ) -> Box<Self> {
        Box::new(RepeatExpr {
            reps,
            ty: Some(TRAIT_UNKNOWN.clone_box()),
        })
    }
}

impl Symbol for RepeatExpr {
    fn get_name(&self) -> &str {
        "repeat_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let recurse = scope.scope_name() == "loop";
        if let Some(mut loop_expr_sym) = scope.resolve("loop") {
            match loop_expr_sym.downcast_ref::<LoopExpr>() {
                None => bail!("repeat can only be called inside a loop"),
                Some(loop_expr) => {
                    let mut loop_expr = loop_expr.clone();
                    if let Some(decls) = &mut loop_expr.decls {
                        if decls.len() != self.reps.len() {
                            bail!("return expression doesn't match for declarations")
                        }
                        let mut sym_table = SymbolTable::new("repeat", Some(scope.clone_box()));
                        for (i, r) in self.reps.iter_mut().enumerate() {
                            println!("1. R = {:?} => {:?}", r, r.get_type());
                            println!("1. decls[{}] = {:?} => {:?}", i, decls[i], decls[i].get_type());
                            match r.get_type() {
                                None => match decls[i].get_type() {
                                    None => {
                                        r.resolve_type(scope)?;
                                        decls[i].set_type(r.get_type());
                                    }
                                    Some(t) => {
                                        r.set_type(Some(t));
                                    }
                                }
                                Some(rt) => match decls[i].get_type() {
                                    None => {
                                        println!("DEBEMOS CAMBIAR TYPE PARA {:?} a {:?}", decls[i], r.get_type());
                                        decls[i].set_type(r.get_type());
                                    }
                                    Some(t) =>
                                        decls[i].matches_types(Some(t.clone_box()))
                                }
                            }
                            println!("2. R = {:?} => {:?}", r, r.get_type());
                            println!("2. decls[{}] = {:?} => {:?}", i, decls[i], decls[i].get_type());
                            decls[i].define_into(&mut *sym_table);
                                decls[i].define_into(scope);
                        }
                        if recurse {
                            loop_expr.expr.resolve_type(&mut *sym_table)?;
                            self.set_type(loop_expr.expr.get_type());
                            println!("sym_table.resolve(num) = {:?}", sym_table.resolve("num"));
                            println!("scope.resolve(num) = {:?}", scope.resolve("num"));
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
