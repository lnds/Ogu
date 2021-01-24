use crate::backend::scopes::types::{Type, TypeClone};

#[derive(Debug, Clone)]
pub(crate) struct RangeType {
    ty: Box<dyn Type>,
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
        match r.downcast_ref::<RangeType>() {
            None => false,
            Some(r) => self.ty.promotes(&*r.ty)
        }
    }
}
