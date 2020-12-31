use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) enum LiteralSym {
    Int(String),
    Float(String),
    Str(String),
}

impl LiteralSym {
    pub(crate) fn new_int(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Int(v.to_string()))
    }

    pub(crate) fn new_str(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Str(v.to_string()))
    }

    pub(crate) fn new_float(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Float(v.to_string()))
    }
}

impl Symbol for LiteralSym {
    fn get_name(&self) -> &str {
        "literal"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            LiteralSym::Int(_) => Some(BasicType::int()),
            LiteralSym::Float(_) => Some(BasicType::float()),
            LiteralSym::Str(_) => Some(BasicType::static_str()),
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, _scope: &mut dyn Scope) -> Option<Box<dyn Type>> {
        self.get_type()
    }
}
