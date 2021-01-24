use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::modules::types::list_type::ListType;

#[derive(Debug, Clone)]
pub(crate) struct RangeType {
    pub(crate) ty: Box<dyn Type>,
}

impl RangeType {
    pub(crate) fn new_range(ty: Box<dyn Type>) -> Box<dyn Type> {
        Box::new(RangeType { ty })
    }
}

impl Type for RangeType {
    fn get_name(&self) -> String {
        format!("Range of {}", self.ty.get_name())
    }

    fn get_signature(&self) -> String {
        self.get_name()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.clone_box())
    }

    fn promotes(&self, r: &dyn Type) -> bool {
        println!("PROMOTES RANGE TO R = {:?}", r);
        match r.downcast_ref::<RangeType>() {
            None => match r.downcast_ref::<ListType>() {
                None => false,
                Some(lt) => self.ty.promotes(&*lt.ty)
            },
            Some(r) => self.ty.promotes(&*r.ty)
        }
    }
}
