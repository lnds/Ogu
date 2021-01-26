use crate::backend::modules::types::basic_type::{CHAR_TYPE, FLOAT_TYPE, INT_TYPE};
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeComparation};
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::list_type::ListType;

#[derive(Debug, Clone)]
pub(crate) struct RangeExpr {
    inferior: Box<dyn Symbol>,
    second: Option<Box<dyn Symbol>>,
    superior: Option<Box<dyn Symbol>>,
}

impl RangeExpr {
    pub(crate) fn new_range(inferior: Box<dyn Symbol>, superior: Box<dyn Symbol>) -> Box<Self> {
        // [1..21]
        Box::new(RangeExpr {
            inferior,
            second: None,
            superior: Some(superior),
        })
    }

    pub(crate) fn new_range_infinite(
        inferior: Box<dyn Symbol>,
        second: Box<dyn Symbol>,
    ) -> Box<Self> {
        // [1,3..]
        Box::new(RangeExpr {
            inferior,
            second: Some(second),
            superior: None,
        })
    }

    pub(crate) fn new_step_range(
        inferior: Box<dyn Symbol>,
        second: Box<dyn Symbol>,
        superior: Box<dyn Symbol>,
    ) -> Box<Self> {
        // [1,3..21]
        Box::new(RangeExpr {
            inferior,
            second: Some(second),
            superior: Some(superior),
        })
    }
}

impl Symbol for RangeExpr {
    fn get_name(&self) -> &str {
        "range_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self.inferior.get_type() {
            None => None,
            Some(ty) => Some(ListType::new_list(ty)),
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.inferior.resolve_type(scope)?;
        if let Some(second) = &mut self.second {
            second.resolve_type(scope)?;
            match second.get_type() {
                None => {
                    second.set_type(self.inferior.get_type());
                    second.define_into(scope);
                }
                Some(sty) => {
                    if let Some(ity) = self.inferior.get_type() {
                        if !ity.is_compatible_with(&*sty) {
                            bail!("range must have elementes of same type {:?} != {:?}", self.inferior.get_type(), second.get_type());
                        } else if ity.is_trait() && !sty.is_trait() {
                            self.inferior.set_type(Some(sty.clone_box()));
                        } else if sty.is_trait() && !ity.is_trait() {
                            second.set_type(Some(sty.clone_box()));
                        }
                    }
                }
            }
        }
        if let Some(sup) = &mut self.superior {
            sup.resolve_type(scope)?;
            match sup.get_type() {
                None => {
                    sup.set_type(self.inferior.get_type());
                    sup.define_into(scope);
                }
                Some(sty) => {
                    if let Some(ity) = self.inferior.get_type() {
                        if !ity.is_compatible_with(&*sty) {
                            bail!("range must have elements of same type {:?} != {:?}", self.inferior.get_type(), sup.get_type());
                        } else if ity.is_trait() && !sty.is_trait() {
                            self.inferior.set_type(Some(sty.clone_box()));
                        } else if sty.is_trait() && !ity.is_trait() {
                            sup.set_type(Some(sty.clone_box()));
                        }
                    }
                }
            }
        }

        match self.inferior.get_type() {
            None => Ok(None),
            Some(ty) => {
                if &*ty == TRAIT_NUM || &*ty == INT_TYPE || &*ty == CHAR_TYPE || &*ty == FLOAT_TYPE
                {
                    Ok(Some(ty))
                } else {
                    bail!("invalid type for range")
                }
            }
        }
    }
}
