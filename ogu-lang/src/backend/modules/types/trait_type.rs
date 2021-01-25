use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::scopes::types::{Type, TypeComparation};

#[derive(Clone, Debug)]
pub(crate) struct TraitType {
    name: &'static str,
}

pub(crate) const TRAIT_UNKNOWN: &TraitType = &TraitType { name: "Unknown" };
pub(crate) const TRAIT_NUM: &TraitType = &TraitType { name: "Num" };
pub(crate) const TRAIT_ORD: &TraitType = &TraitType { name: "PartialOrd" };
pub(crate) const TRAIT_EQ: &TraitType = &TraitType { name: "PartialEq" };

impl TraitType {
    pub(crate) fn new_trait(name: &'static str) -> Box<dyn Type> {
        Box::new(TraitType { name })
    }
}

impl Type for TraitType {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_signature(&self) -> String {
        format!("Trait {}", self.get_name())
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        None
    }

    fn is_compatible_with(&self, other: &dyn Type) -> bool {
        if let Some(ot) = other.downcast_ref::<BasicType>() {
            ot.is_compatible_with(self)
        } else {
            self.get_signature() == other.get_signature()
                || other == TRAIT_UNKNOWN
                || self == TRAIT_UNKNOWN
        }
    }

    fn is_trait(&self) -> bool {
        true
    }

    fn match_types(&mut self, other: &dyn Type) {
        if self.name == TRAIT_UNKNOWN.name {
            if let Some(ot) = other.downcast_ref::<TraitType>() {
                self.name = ot.name
            }
        }
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        if self == TRAIT_UNKNOWN {
            TypeComparation::Inferior
        } else if other == TRAIT_UNKNOWN {
            TypeComparation::Superior
        } else if other.is_trait() {
            if self.get_name() == other.get_name() {
                TypeComparation::Same
            } else {
                TypeComparation::Incomparables
            }
        } else {
            TypeComparation::Incomparables
        }
    }
}

impl PartialEq for TraitType {
    fn eq(&self, other: &Self) -> bool {
        self.get_signature() == other.get_signature()
    }
}
