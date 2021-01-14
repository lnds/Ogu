use crate::backend::scopes::types::{Type};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::list_type::ListType;

#[derive(Debug, Clone)]
pub(crate) enum ListExpr {
    Empty,
    List(Vec<Box<dyn Symbol>>),
}

impl ListExpr {
    pub(crate) fn new_empty() -> Box<Self> {
        Box::new(ListExpr::Empty)
    }

    pub(crate) fn new_list(exprs: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(ListExpr::List(exprs))
    }
}

impl Symbol for ListExpr {
    fn get_name(&self) -> &str {
        "list_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ListExpr::Empty => Some(ListType::new_empty()),
            ListExpr::List(v) if v.is_empty() => Some(ListType::new_empty()),
            ListExpr::List(v) => {
                match v[0].get_type() {
                    None => None,
                    Some(t) => Some(ListType::new_list(t.clone_box()))
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ListExpr::Empty => Ok(Some(ListType::new_empty())),
            ListExpr::List(e) if e.is_empty() => Ok(Some(ListType::new_empty())), // weird
            ListExpr::List(exprs) => {
                for e in exprs.iter_mut() {
                    e.resolve_type(scope)?;
                }
                let t = exprs[0].get_type();
                match t {
                    None => Ok(None),
                    Some(ty) => {
                        let not_same = exprs.iter().any(|t|
                            match t.get_type() {
                                None => true,
                                Some(t) => &*t != &*ty
                            });
                        if not_same {
                            bail!("list must have all elements of same type")
                        } else {
                            Ok(Some(ListType::new_list(ty)))
                        }
                    }
                }
            }
        }
    }
}