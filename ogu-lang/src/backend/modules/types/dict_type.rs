use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};

#[derive(Debug, Clone)]
pub(crate) enum DictType {
    Empty,
    Dict(Box<dyn Type>, Box<dyn Type>),
}

impl DictType {
    pub(crate) fn new_empty() -> Box<dyn Type> {
        Box::new(DictType::Empty)
    }

    pub(crate) fn new_dict(key_type: Box<dyn Type>, val_type: Box<dyn Type>) -> Box<dyn Type> {
        Box::new(DictType::Dict(key_type, val_type))
    }
}

impl Type for DictType {
    fn get_name(&self) -> String {
        match self {
            DictType::Empty => "EmptyDict".to_string(),
            DictType::Dict(k, v) => format!("Dict {} -> {}", k.get_name(), v.get_name()),
        }
    }

    fn get_signature(&self) -> String {
        self.get_name()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.clone_box())
    }

    fn is_compatible_with(&self, _: &dyn Type) -> bool {
        unimplemented!()
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        unimplemented!()
    }
}
