use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum LiteralSym {
    Int(String),
    Float(String),
    Str(String),
    Char(String),
    Date(String),
    Regexp(String),
    Unit,
}

impl LiteralSym {
    pub(crate) fn new_int(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Int(v.to_string()))
    }

    pub(crate) fn new_str(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Str(v.to_string()))
    }

    pub(crate) fn new_char(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Char(v.to_string()))
    }

    pub(crate) fn new_date(d: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Date(d.to_string()))
    }

    pub(crate) fn new_regexp(r: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Regexp(r.to_string()))
    }

    pub(crate) fn new_float(v: &str) -> Box<LiteralSym> {
        Box::new(LiteralSym::Float(v.to_string()))
    }

    pub(crate) fn new_unit() -> Box<LiteralSym> {
        Box::new(LiteralSym::Unit)
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
            LiteralSym::Char(_) => Some(BasicType::char()),
            LiteralSym::Date(_) => Some(BasicType::date()),
            LiteralSym::Regexp(_) => Some(BasicType::regexp()),
            LiteralSym::Unit => Some(BasicType::unit()),
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        // do nothing
    }

    fn resolve_type(&mut self, _scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        Ok(self.get_type())
    }

}
