use crate::backend::modules::types::basic_type::{INVALID_TYPE, BasicType, BOOL_TYPE};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Clone, Debug)]
pub(crate) struct IfExpr {
    cond: Box<dyn Symbol>,
    then_expr: Box<dyn Symbol>,
    else_expr: Box<dyn Symbol>,
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
        })
    }
}

impl Symbol for IfExpr {
    fn get_name(&self) -> &str {
        "if_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let tt = self.then_expr.get_type()?;
        let et = self.else_expr.get_type()?;
        if &*tt != &*et {
            if tt.promotes(&*et) {
                Some(tt.clone())
            } else if et.promotes(&*tt) {
                Some(et.clone_box())
            } else {
                None
            }
        } else {
            Some(et.clone())
        }
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
                None => {}
                Some(et) if &*et == INVALID_TYPE => {
                    bail!("Invalid If, Cond or Guard expression");
                }
                Some(et) => {
                    self.then_expr.set_type(Some(et));
                }
            },
            Some(tt) => match self.else_expr.get_type() {
                None => {
                    self.else_expr.set_type(Some(tt));
                } // println!("THEN => {:?} ELSE None", tt),
                Some(et) if &*et == INVALID_TYPE => {
                    bail!("Invalid If or Cond");
                }
                Some(_et) => {
                    /*
                    if tt.promotes(&*et) {
                        self.then_expr.set_type(Some(tt));
                    } else if et.promotes(&*tt) {
                        self.else_expr.set_type(Some(et));
                    } else {
                    }

                     */
                }
            },
        }
        Ok(self.get_type())
    }
}
