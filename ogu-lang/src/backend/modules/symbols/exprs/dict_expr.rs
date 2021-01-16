use crate::backend::scopes::types::{Type};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::dict_type::DictType;

#[derive(Debug, Clone)]
pub(crate) enum DictExpr {
    Empty,
    Dict(Vec<(Box<dyn Symbol>, Box<dyn Symbol>)>),
}

impl DictExpr {
    pub(crate) fn new_empty() -> Box<Self> {
        Box::new(DictExpr::Empty)
    }

    pub(crate) fn new_dict(exprs: Vec<(Box<dyn Symbol>, Box<dyn Symbol>)>) -> Box<Self> {
        if exprs.is_empty() {
            DictExpr::new_empty()
        } else {
            Box::new(DictExpr::Dict(exprs))

        }
    }
}

impl Symbol for DictExpr {
    fn get_name(&self) -> &str {
        "dict_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match &self {
            DictExpr::Empty => Some(DictType::new_empty()),
            DictExpr::Dict(v) if v.is_empty() => Some(DictType::new_empty()),
            DictExpr::Dict(v) => {
                let (k, v) = &v[0];
                match k.get_type() {
                    None => None,
                    Some(kt) => match v.get_type() {
                        None => None,
                        Some(vt) =>  Some(DictType::new_dict(kt, vt))
                    }
                }

            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            DictExpr::Empty => Ok(Some(DictType::new_empty())),
            DictExpr::Dict(e) if e.is_empty() => Ok(Some(DictType::new_empty())), // weird
            DictExpr::Dict(exprs) => {
                for (k, v) in exprs.iter_mut() {
                    k.resolve_type(scope)?;
                    v.resolve_type(scope)?;
                }
                if let Some(ty) = exprs[0].0.get_type() {
                    let not_same = exprs.iter().any(|t|
                        match t.0.get_type() {
                            None => true,
                            Some(t) => &*t != &*ty
                        });
                    if not_same {
                        bail!("dict  must have all key elements of same type {:#?}", exprs);
                    }
                }
                if let Some(ty) = exprs[0].1.get_type() {
                    let not_same = exprs.iter().any(|t|
                        match t.1.get_type() {
                            None => true,
                            Some(t) => &*t != &*ty
                        });
                    if not_same {
                         bail!("dict  must have all value elements of same type {:#?}", exprs);
                    }
                }
                Ok(self.get_type())
            }
        }
    }
}
