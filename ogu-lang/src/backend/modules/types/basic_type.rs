use crate::backend::scopes::types::Type;

#[derive(Clone, Debug)]
pub(crate) enum BasicType {
    Unit,
}

impl BasicType {

    pub(crate) fn unit() -> Box<dyn Type> {
        Box::new(BasicType::Unit)
    }
}


impl Type for BasicType {
    fn get_name(&self) -> &str {
        "()"
    }
}

