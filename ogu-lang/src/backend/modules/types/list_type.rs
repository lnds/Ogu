use crate::backend::scopes::types::{Type, TypeClone};

#[derive(Debug, Clone)]
pub(crate) enum ListType {
    Empty,
    List(Box<dyn Type>)
}

impl ListType {

    pub(crate) fn new_empty() -> Box<dyn Type> {
        Box::new(ListType::Empty)
    }

    pub(crate) fn new_list(ty: Box<dyn Type>) -> Box<Self> {
        Box::new(ListType::List(ty))
    }
}

impl Type for ListType {
    fn get_name(&self) -> String {
        match self {
            ListType::Empty => "EmptyList".to_string(),
            ListType::List(t) => format!("List of {}", t.get_name()),
        }
    }

    fn get_signature(&self) -> String {
        self.get_name()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
       Some(self.clone_box())
    }

    fn promotes(&self, _: &dyn Type) -> bool {
        unimplemented!()
    }
}