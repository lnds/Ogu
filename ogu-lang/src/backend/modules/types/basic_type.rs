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

    pub(crate) fn int() -> Box<dyn Type> { Box::new(BasicType::Int) }

    pub(crate) fn float() -> Box<dyn Type> { Box::new(BasicType::Float) }

    pub(crate) fn static_str() -> Box<dyn Type> { Box::new(BasicType::StaticStr) }

}


impl Type for BasicType {
    fn get_name(&self) -> &str {
        match self {
            BasicType::Unit => "()",
            BasicType::Int => "int",
            BasicType::Float => "int",
            BasicType::StaticStr => "&str",
        }

    }

    fn get_signature(&self) -> String {
        self.get_name().to_string()
    }
}

impl PartialEq for BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
