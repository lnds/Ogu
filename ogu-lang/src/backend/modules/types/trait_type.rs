use crate::backend::scopes::types::Type;

#[derive(Clone, Debug)]
pub(crate) struct TraitType {
    name: String,
}

impl TraitType {
    pub(crate) fn new_trait(name: &str) -> Box<dyn Type> {
        Box::new(TraitType {
            name: name.to_string(),
        })
    }
}

impl Type for TraitType {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_signature(&self) -> String {
        format!("Trait {}", self.get_name())
    }
}

impl PartialEq for TraitType {
    fn eq(&self, other: &Self) -> bool {
        self.get_name() == other.get_name()
    }
}
