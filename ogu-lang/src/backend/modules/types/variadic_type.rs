use crate::backend::scopes::types::Type;

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

    fn promotes(&self, _other: &dyn Type) -> bool {
        false
    }
}

impl VariadicType {
    pub(crate) fn new(result: Box<dyn Type>) -> Box<Self> {
        Box::new(VariadicType { result })
    }

    pub(crate) fn new_opt(result: Box<dyn Type>) -> Option<Box<dyn Type>> {
        Some(Self::new(result))
    }
}
