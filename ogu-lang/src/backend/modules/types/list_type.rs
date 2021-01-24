use crate::backend::scopes::types::{Type, TypeClone};
use std::ops::Deref;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;

#[derive(Debug, Clone)]
pub(crate) struct ListType {
    pub (crate) ty: Box<dyn Type>,
}

impl ListType {
    pub(crate) fn new_empty() -> Box<dyn Type> {
        Self::new_list(TRAIT_UNKNOWN.clone_box())
    }

    pub(crate) fn new_list(ty: Box<dyn Type>) -> Box<dyn Type> {
        Box::new(ListType { ty })
    }
}

impl Type for ListType {
    fn get_name(&self) -> String {
        format!("List of {}", self.ty.get_name())
    }

    fn get_signature(&self) -> String {
        self.get_name()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.clone_box())
    }

    fn promotes(&self, other: &dyn Type) -> bool {
        match other.downcast_ref::<ListType>() {
            None => false,
            Some(lt) => {
                lt.ty.promotes(&*self.ty.deref())
            }
        }
    }

    fn is_trait(&self) -> bool {
        self.ty.is_trait()
    }

    fn match_types(&mut self, other: &dyn Type) {
        println!("MATCH TYPE FOR LIST TYPE SELF = {:?} OTHER = {:?}", self, other);
        if let Some(other) = other.downcast_ref::<ListType>() {
            if self.is_trait() && other.ty.promotes(&*self.ty) {
                self.ty = other.ty.clone()
            }
        }
    }
}
