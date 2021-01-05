use crate::backend::scopes::types::Type;

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

    fn promotes(&self, other: &dyn Type) -> bool {
        other == TRAIT_UNKNOWN || self == TRAIT_UNKNOWN
    }

    fn is_trait(&self) -> bool {
        true
    }
}

impl PartialEq for TraitType {
    fn eq(&self, other: &Self) -> bool {
        self.get_signature() == other.get_signature()
    }
}
