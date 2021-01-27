use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};

#[derive(Debug, Clone)]
pub(crate) struct ListType {
    pub(crate) ty: Box<dyn Type>,
}

impl ListType {
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

    fn is_compatible_with(&self, other: &dyn Type) -> bool {
        match other.downcast_ref::<ListType>() {
            None => false,
            Some(lt) => lt.ty.is_compatible_with(&*self.ty),
        }
    }

    fn is_trait(&self) -> bool {
        self.ty.is_trait()
    }

    fn match_types(&mut self, other: &dyn Type) {
        if let Some(other) = other.downcast_ref::<ListType>() {
            if self.is_trait() && other.ty.is_compatible_with(&*self.ty) {
                self.ty = other.ty.clone()
            }
        }
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        match other.downcast_ref::<ListType>() {
            None => TypeComparation::Incomparables,
            Some(other) => self.ty.compare(&*other.ty),
        }
    }
}
