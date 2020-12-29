use crate::types::Type;

#[derive(Clone, Debug)]
pub(crate) struct GenericType {
    base: String,
    traits: Vec<String>,
}

impl Type for GenericType {
    fn get_name(&self) -> String {
        self.base.to_string()
    }

    fn is_generic(&self) -> bool {
        true
    }

    fn get_full_name(&self) -> String {
        format!("{} : {}", self.base, self.traits.join(" + "))
    }
}

impl GenericType {
    pub(crate) fn new(base: &str, traits: Vec<String>) -> Box<Self> {
        Box::new(GenericType {
            base: base.to_string(),
            traits,
        })
    }
}
