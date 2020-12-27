use crate::types::Type;

#[derive(Clone)]
pub(crate) enum BasicType {
    Unit,
    Primitive(String),
    Str,
}

impl Type for BasicType {
    fn get_name(&self) -> String {
        match self {
            BasicType::Unit => String::from("()"),
            BasicType::Primitive(s) => s.clone(),
            BasicType::Str => String::from("String"),
        }
    }
}
