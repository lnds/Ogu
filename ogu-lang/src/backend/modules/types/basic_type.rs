use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{
    TraitType, TRAIT_EQ, TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN,
};
use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};

#[derive(Clone, Debug)]
pub(crate) enum BasicType {
    Unit,
    Int,
    UInt,
    Float,
    Char,
    Date,
    Regexp,
    Bool,
    Invalid,
    Undefined(String),
}

pub(crate) const UNIT_TYPE: &BasicType = &BasicType::Unit;
pub(crate) const INT_TYPE: &BasicType = &BasicType::Int;
pub(crate) const UINT_TYPE: &BasicType = &BasicType::UInt;
pub(crate) const FLOAT_TYPE: &BasicType = &BasicType::Float;
pub(crate) const CHAR_TYPE: &BasicType = &BasicType::Char;
pub(crate) const DATE_TYPE: &BasicType = &BasicType::Date;
pub(crate) const REGEXP_TYPE: &BasicType = &BasicType::Regexp;
pub(crate) const BOOL_TYPE: &BasicType = &BasicType::Bool;
pub(crate) const INVALID_TYPE: &BasicType = &BasicType::Invalid;

impl BasicType {
    pub(crate) fn unit() -> Box<dyn Type> {
        UNIT_TYPE.clone_box()
    }

    pub(crate) fn int() -> Box<dyn Type> {
        INT_TYPE.clone_box()
    }

    pub(crate) fn uint() -> Box<dyn Type> {
        UINT_TYPE.clone_box()
    }

    pub(crate) fn float() -> Box<dyn Type> {
        FLOAT_TYPE.clone_box()
    }

    pub(crate) fn static_str() -> Box<dyn Type> {
        ListType::new_list(BasicType::char())
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

    pub(crate) fn undefined(name: &str) -> Box<dyn Type> {
        Box::new(BasicType::Undefined(name.to_string()))
    }
}

impl Type for BasicType {
    fn get_name(&self) -> String {
        match self {
            BasicType::Unit => "()".to_string(),
            BasicType::Int => "int".to_string(),
            BasicType::UInt => "uint".to_string(),
            BasicType::Float => "float".to_string(),
            BasicType::Char => "char".to_string(),
            BasicType::Bool => "bool".to_string(),
            BasicType::Date => "date".to_string(),
            BasicType::Regexp => "regexp".to_string(),
            BasicType::Invalid => "invalid".to_string(),
            BasicType::Undefined(s) => s.to_string(),
        }
    }

    fn get_signature(&self) -> String {
        format!("BasicType {}", self.get_name())
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(Box::new(self.clone()))
    }

    fn is_compatible_with(&self, other: &dyn Type) -> bool {
        if self.get_signature() == other.get_signature() {
            return true;
        }
        match self {
            BasicType::Int | BasicType::UInt => {
                if other == TRAIT_NUM
                    || other == TRAIT_UNKNOWN
                    || other == TRAIT_ORD
                    || other == TRAIT_EQ
                {
                    true
                } else if let Some(ot) = other.downcast_ref::<BasicType>() {
                    matches!(ot, BasicType::Int | BasicType::UInt)
                } else {
                    false
                }
            }
            BasicType::Float => {
                if other == TRAIT_NUM
                    || other == TRAIT_UNKNOWN
                    || other == TRAIT_ORD
                    || other == TRAIT_EQ
                {
                    true
                } else {
                    matches!(other.downcast_ref::<BasicType>(), Some(BasicType::Float))
                }
            }
            BasicType::Char => other == TRAIT_UNKNOWN || other == TRAIT_ORD || other == TRAIT_EQ,
            BasicType::Bool => other == TRAIT_UNKNOWN || other == TRAIT_ORD || other == TRAIT_EQ,
            _ => other == TRAIT_UNKNOWN,
        }
    }

    fn is_basic_type(&self) -> bool {
        true
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        match other.downcast_ref::<BasicType>() {
            None => {
                if let Some(other) = other.downcast_ref::<TraitType>() {
                    if other == TRAIT_UNKNOWN
                        || (other == TRAIT_NUM
                            && matches!(
                                self,
                                BasicType::Int
                                    | BasicType::UInt
                                    | BasicType::Float
                                    | BasicType::Char
                            ))
                        || (other == TRAIT_EQ
                            && matches!(
                                self,
                                BasicType::Date
                                    | BasicType::Bool
                                    | BasicType::Regexp
                                    | BasicType::Int
                                    | BasicType::UInt
                                    | BasicType::Float
                                    | BasicType::Char
                            ))
                        || (other == TRAIT_ORD
                            && matches!(
                                self,
                                BasicType::Date
                                    | BasicType::Int
                                    | BasicType::UInt
                                    | BasicType::Float
                                    | BasicType::Char
                            ))
                    {
                        TypeComparation::Superior
                    } else {
                        TypeComparation::Incomparables
                    }
                } else {
                    TypeComparation::Incomparables
                }
            }
            Some(other) => match self {
                BasicType::Bool => match other {
                    BasicType::Bool => TypeComparation::Same,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Char => match other {
                    BasicType::Char => TypeComparation::Same,
                    BasicType::Int => TypeComparation::Inferior,
                    BasicType::UInt => TypeComparation::Inferior,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Int => match other {
                    BasicType::Int => TypeComparation::Same,
                    BasicType::Char => TypeComparation::Superior,
                    BasicType::UInt => TypeComparation::Inferior,
                    BasicType::Float => TypeComparation::Inferior,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::UInt => match other {
                    BasicType::UInt => TypeComparation::Same,
                    BasicType::Int => TypeComparation::Superior,
                    BasicType::Char => TypeComparation::Superior,
                    BasicType::Float => TypeComparation::Inferior,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Date => match other {
                    BasicType::Date => TypeComparation::Same,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Float => match other {
                    BasicType::Float => TypeComparation::Same,
                    BasicType::UInt => TypeComparation::Superior,
                    BasicType::Int => TypeComparation::Superior,
                    BasicType::Char => TypeComparation::Superior,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Regexp => match other {
                    BasicType::Regexp => TypeComparation::Same,
                    _ => TypeComparation::Incomparables,
                },
                BasicType::Unit => match other {
                    BasicType::Unit => TypeComparation::Same,
                    _ => TypeComparation::Incomparables,
                },
                _ => TypeComparation::Incomparables,
            },
        }
    }
}

impl PartialEq for BasicType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
