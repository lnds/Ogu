use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::types::{Type, TypeClone};

#[derive(Clone, Debug)]
pub(crate) enum BasicType {
    Unit,
    Int,
    Float,
    StaticStr,
    Char,
    Date,
    Regexp,
    Bool,
}


pub(crate) const UNIT_TYPE: &BasicType = &BasicType::Unit;
pub(crate) const INT_TYPE: &BasicType = &BasicType::Int;
pub(crate) const FLOAT_TYPE: &BasicType = &BasicType::Float;
pub(crate) const STATIC_STR_TYPE: &BasicType = &BasicType::StaticStr;
pub(crate) const CHAR_TYPE: &BasicType = &BasicType::Char;
pub(crate) const DATE_TYPE: &BasicType = &BasicType::Date;
pub(crate) const REGEXP_TYPE: &BasicType = &BasicType::Regexp;
pub(crate) const BOOL_TYPE: &BasicType = &BasicType::Bool;



impl BasicType {
    pub(crate) fn unit() -> Box<dyn Type> {
        UNIT_TYPE.clone_box()
    }

    pub(crate) fn int() -> Box<dyn Type> {
        INT_TYPE.clone_box()
    }

    pub(crate) fn float() -> Box<dyn Type> {
        FLOAT_TYPE.clone_box()
    }

    pub(crate) fn static_str() -> Box<dyn Type> {
        STATIC_STR_TYPE.clone_box()
    }

    pub(crate) fn char() -> Box<dyn Type> {
        CHAR_TYPE.clone_box()
    }

    pub(crate) fn bool() -> Box<dyn Type> {
        BOOL_TYPE.clone_box()
    }

    pub(crate) fn date() -> Box<dyn Type> {
        DATE_TYPE.clone_box()
    }

    pub(crate) fn regexp() -> Box<dyn Type> {
        REGEXP_TYPE.clone_box()
    }

}

impl Type for BasicType {
    fn get_name(&self) -> String {
        match self {
            BasicType::Unit => "()".to_string(),
            BasicType::Int => "int".to_string(),
            BasicType::Float => "float".to_string(),
            BasicType::StaticStr => "&str".to_string(),
            BasicType::Char => "char".to_string(),
            BasicType::Bool => "bool".to_string(),
            BasicType::Date => "date".to_string(),
            BasicType::Regexp => "regexp".to_string(),
        }
    }

    fn get_signature(&self) -> String {
        format!("BasicType {}", self.get_name())
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
                    matches!(ot, BasicType::Int)
                } else {
                    false
                }
            }
            BasicType::Float => {
                if other == TRAIT_NUM {
                    true
                } else if let Some(ot) = other.downcast_ref::<BasicType>() {
                    matches!(ot, BasicType::Int | BasicType::Float)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn is_basic_type(&self) -> bool { true }

}

impl PartialEq for BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
