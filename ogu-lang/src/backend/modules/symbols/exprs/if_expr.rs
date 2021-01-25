use crate::backend::modules::types::basic_type::{INVALID_TYPE, BasicType, BOOL_TYPE};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;

#[derive(Clone, Debug)]
pub(crate) struct IfExpr {
    cond: Box<dyn Symbol>,
    then_expr: Box<dyn Symbol>,
    else_expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl IfExpr {
    pub(crate) fn new(
        cond: Box<dyn Symbol>,
        then_expr: Box<dyn Symbol>,
        else_expr: Box<dyn Symbol>,
    ) -> Box<Self> {
        Box::new(IfExpr {
            cond,
            then_expr,
            else_expr,
            ty: None,
        })
    }
}

impl Symbol for IfExpr {
    fn get_name(&self) -> &str {
        "if_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.then_expr.set_type(ty.clone());
        self.else_expr.set_type(ty.clone());
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.cond.resolve_type(scope)?;
        match self.cond.get_type() {
            None => self.cond.set_type(Some(BasicType::bool())),
            Some(t) if t.is_trait() =>   self.cond.set_type(Some(BasicType::bool())),
            Some(t) => {
                if &*t != BOOL_TYPE {
                    bail!("condition on if must be booolean, not {:?}", t)
                }
            }
        }
        self.cond.define_into(scope);
        self.then_expr.resolve_type(scope)?;
        scope.define(self.then_expr.clone());
        self.else_expr.resolve_type(scope)?;
        scope.define(self.else_expr.clone());
        match self.then_expr.get_type() {
            None => match self.else_expr.get_type() {
                None => {
                    self.ty = Some(TRAIT_UNKNOWN.clone_box());
                }
                Some(et) if &*et == INVALID_TYPE => {
                    bail!("Invalid If, Cond or Guard expression");
                }
                Some(et) => {
                    self.then_expr.set_type(Some(et.clone()));
                    self.ty = Some(et);
                }
            },
            Some(tt) => match self.else_expr.get_type() {
                None => {
                    self.else_expr.set_type(Some(tt.clone()));
                    self.ty = Some(tt);

                } // println!("THEN => {:?} ELSE None", tt),
                Some(et) if &*et == INVALID_TYPE => {
                    bail!("Invalid If or Cond");
                }
                Some(et) => {
                    if &*tt != &*et {
                        println!("ET {:?} != TT {:?}", et, tt);
                        if &*et == TRAIT_UNKNOWN && &*tt != TRAIT_UNKNOWN {
                            self.else_expr.set_type(Some(tt.clone()));
                            self.ty = Some(tt);
                        }
                        else if &*tt == TRAIT_UNKNOWN && &*et != TRAIT_UNKNOWN {
                            self.then_expr.set_type(Some(et.clone()));
                            self.ty = Some(et);
                        } else
                            if tt.is_trait() && !et.is_trait() {
                                self.then_expr.set_type(Some(et.clone()));
                                self.ty = Some(et);
                            }
                        else if et.is_trait() && !tt.is_trait() {
                            self.else_expr.set_type(Some(tt.clone()));
                            self.ty = Some(tt);
                        } else {
                            match et.compare(&*tt) {
                                TypeComparation::Incomparables => bail!("can't resolve type for if expression"),
                                TypeComparation::Inferior => {
                                    self.else_expr.set_type(Some(tt.clone()));
                                    self.ty = Some(tt);
                                }
                                TypeComparation::Same => {
                                    self.ty = Some(tt);
                                }
                                TypeComparation::Superior => {
                                    self.then_expr.set_type(Some(et.clone()));
                                    self.ty = Some(et);
                                }
                            }
                        }

                    } else {
                        self.ty = Some(tt);
                    }
                }
            },
        }
        Ok(self.get_type())
    }
}
