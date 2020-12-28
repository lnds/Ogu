use crate::types::Type;

#[derive(Clone)]
pub(crate) struct GenericType(String);

impl Type for GenericType {
    fn get_name(&self) -> String {
        self.0.to_string()
    }
}

impl GenericType {
    pub(crate) fn new(t: &str) -> Box<Self> {
        Box::new(
            GenericType(t.to_string())
        )
    }
}