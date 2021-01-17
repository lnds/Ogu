use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum Literal {
    Int(String),
    Float(String),
    Str(String),
    Char(String),
    Date(String),
    Regexp(String),
    True,
    False,
    Unit,
    Invalid,
}

impl Literal {
    pub(crate) fn new_int(v: &str) -> Box<Literal> {
        Box::new(Literal::Int(v.to_string()))
    }

    pub(crate) fn new_str(v: &str) -> Box<Literal> {
        Box::new(Literal::Str(v.to_string()))
    }

    pub(crate) fn new_char(v: &str) -> Box<Literal> {
        Box::new(Literal::Char(v.to_string()))
    }

    pub(crate) fn new_date(d: &str) -> Box<Literal> {
        Box::new(Literal::Date(d.to_string()))
    }

    pub(crate) fn new_regexp(r: &str) -> Box<Literal> {
        Box::new(Literal::Regexp(r.to_string()))
    }

    pub(crate) fn new_float(v: &str) -> Box<Literal> {
        Box::new(Literal::Float(v.to_string()))
    }

    pub(crate) fn new_unit() -> Box<Literal> {
        Box::new(Literal::Unit)
    }

    pub(crate) fn new_invalid() -> Box<Literal> {
        Box::new(Literal::Invalid)
    }

    pub(crate) fn new_true() -> Box<Literal> {
        Box::new(Literal::True)
    }

    pub(crate) fn new_false() -> Box<Literal> {
        Box::new(Literal::False)
    }
}

impl Symbol for Literal {
    fn get_name(&self) -> &str {
        "literal"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            Literal::Int(s) => {
                if s.ends_with('N') {
                    Some(TRAIT_NUM.clone_box())
                } else {
                    Some(BasicType::int())
                }
            }
            Literal::Float(s) => {
                if s.ends_with('M') {
                    Some(TRAIT_NUM.clone_box())
                } else {
                    Some(BasicType::float())
                }
            }
            Literal::Str(_) => Some(BasicType::static_str()),
            Literal::Char(_) => Some(BasicType::char()),
            Literal::Date(_) => Some(BasicType::date()),
            Literal::Regexp(_) => Some(BasicType::regexp()),
            Literal::Unit => Some(BasicType::unit()),
            Literal::Invalid => None,
            Literal::True | Literal::False => Some(BasicType::bool()),
        }
    }

    fn resolve_type(&mut self, _scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        Ok(self.get_type())
    }
}
