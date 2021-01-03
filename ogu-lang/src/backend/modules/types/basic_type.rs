use crate::backend::modules::types::trait_type::TRAIT_NUM;
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
            BasicType::Float => "float".to_string(),
            BasicType::StaticStr => "&str".to_string(),
        }
    }

    fn get_signature(&self) -> String {
        format!("BasicType {}", self.get_name())
    }

    fn is_trait(&self) -> bool {
        false
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(Box::new(self.clone()))
    }

    fn promotes(&self, other: &dyn Type) -> bool {
        match self {
            BasicType::Int => {
                if other == TRAIT_NUM {
                    true
                } else if let Some(ot) = other.downcast_ref::<BasicType>() {
                    match ot {
                        BasicType::Int => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            BasicType::Float => {
                if other == TRAIT_NUM {
                    true
                } else if let Some(ot) = other.downcast_ref::<BasicType>() {
                    match ot {
                        BasicType::Int | BasicType::Float => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl PartialEq for BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
