use crate::backend::scopes::types::Type;

#[derive(Clone, Debug)]
pub(crate) enum BasicType {
    Unit,
    Int,
    Float,
    StaticStr,
}

impl BasicType {
    pub(crate) fn unit() -> Box<dyn Type> {
        Box::new(BasicType::Unit)
    }

    pub(crate) fn int() -> Box<dyn Type> {
        Box::new(BasicType::Int)
    }

    pub(crate) fn float() -> Box<dyn Type> {
        Box::new(BasicType::Float)
    }

    pub(crate) fn static_str() -> Box<dyn Type> {
        Box::new(BasicType::StaticStr)
    }
}

impl Type for BasicType {
    fn get_name(&self) -> String {
        match self {
            BasicType::Unit => "()".to_string(),
            BasicType::Int => "int".to_string(),
            BasicType::Float => "int".to_string(),
            BasicType::StaticStr => "&str".to_string(),
        }
    }

    fn get_signature(&self) -> String {
        format!("BasicType {}", self.get_name())
    }

    fn is_trait(&self) -> bool {
        true
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        None
    }
}

impl PartialEq for BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
