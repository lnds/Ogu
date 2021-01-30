use anyhow::{bail, Result};

use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_UNKNOWN, TRAIT_NUM};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use crate::backend::modules::types::basic_type::{BasicType, INT_TYPE, UINT_TYPE};

#[derive(Debug, Clone)]
pub(crate) struct IndexExpr {
    list: Box<dyn Symbol>,
    index: Box<dyn Symbol>,
}

impl IndexExpr {
    pub(crate) fn new(list: Box<dyn Symbol>, index: Box<dyn Symbol>) -> Box<Self> {
        Box::new(IndexExpr {list, index})
    }

}

impl Symbol for IndexExpr {
    fn get_name(&self) -> &str {
        "index_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self.list.get_type() {
            None => None,
            Some(lt) =>
                match lt.downcast_ref::<ListType>() {
                    None => None,
                    Some(lt) => Some(lt.ty.clone())
                }
        }
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        if let Some(lty) = &ty {
            if lty.downcast_ref::<ListType>().is_some() {
                self.list.set_type(ty);
            }
        }
    }


    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.index.resolve_type(scope)?;
        match self.list.get_type() {
            None => {},
            Some(ty) => {
                if &*ty == TRAIT_UNKNOWN {
                    self.index.set_type(Some(BasicType::int()))
                } else if &*ty != TRAIT_NUM && &*ty != INT_TYPE && &*ty != UINT_TYPE {
                    bail!("index must be a cardinal type")
                }
            }
        };
        self.list.resolve_type(scope)?;
        match self.list.get_type() {
            None => {
                self.list.set_type(Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                Ok(self.get_type())
            }
            Some(ty) => match ty.downcast_ref::<ListType>() {
                None => bail!("index expression requires a list"),
                Some(_) => Ok(self.list.get_type())
            }
        }
    }

}
