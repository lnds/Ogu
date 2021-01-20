use crate::backend::scopes::types::{Type, TypeClone};
use std::ops::Deref;

#[derive(Debug, Clone)]
pub(crate) enum ListType {
    EmptyList,
    List(Box<dyn Type>),
}

impl ListType {
    pub(crate) fn new_empty() -> Box<dyn Type> {
        Box::new(ListType::EmptyList)
    }

    pub(crate) fn new_list(ty: Box<dyn Type>) -> Box<dyn Type> {
        Box::new(ListType::List(ty))
    }
}

impl Type for ListType {
    fn get_name(&self) -> String {
        match self {
            ListType::EmptyList => "EmptyList".to_string(),
            ListType::List(t) => format!("List of {}", t.get_name()),
        }
    }

    fn get_signature(&self) -> String {
        self.get_name()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.clone_box())
    }

    fn promotes(&self, other: &dyn Type) -> bool {
        match other.downcast_ref::<ListType>() {
            None => false,
            Some(ListType::EmptyList) => false,
            Some(ListType::List(ty)) => match &self {
                ListType::EmptyList => true,
                ListType::List(sty) =>
                    sty.promotes(ty.deref().deref())
            },
        }
    }

    fn match_types(&mut self, other: &dyn Type) {
        if let Some(other) = other.downcast_ref::<ListType>() {
            match other {
                ListType::EmptyList => {}
                ListType::List(ty) => {
                    match &self {
                        ListType::EmptyList => {
                            *self = ListType::List(ty.clone())
                        }
                        ListType::List(lty) => {
                            if self.is_trait() && lty.promotes(ty.deref().deref()) {
                                *self = ListType::List(ty.clone());
                            }
                        }
                    }
                },
            }
        }
    }

    fn is_trait(&self) -> bool {
        match self {
            ListType::EmptyList => true,
            ListType::List(ty) => ty.is_trait()
        }
    }

}
