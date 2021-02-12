use crate::backend::scopes::types::{Type, TypeComparation};

#[derive(Clone, Debug)]
pub(crate) struct VariadicType {
    result: Box<dyn Type>,
}

impl Type for VariadicType {
    fn get_name(&self) -> String {
        format!("[...] -> {:?}", self.result)
    }

    fn get_signature(&self) -> String {
        format!("VariadicType <{}>", self.get_name())
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.result.clone())
    }

    fn is_compatible_with(&self, _other: &dyn Type) -> bool {
        false
    }

    fn compare(&self, _other: &dyn Type) -> TypeComparation {
        unimplemented!()
    }
}

impl VariadicType {
    pub(crate) fn new(result: Box<dyn Type>) -> Box<Self> {
        Box::new(VariadicType { result })
    }

}
