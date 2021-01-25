use std::ops::Deref;

use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::types::{Type, TypeComparation};

#[derive(Clone, Debug)]
pub(crate) struct TupleType {
    pub(crate) tuple: Vec<Box<dyn Type>>,
}

impl TupleType {
    pub(crate) fn new(tuple: Vec<Box<dyn Type>>) -> Self {
        TupleType { tuple }
    }

    #[allow(dead_code)]
    pub(crate) fn new_box(tuple: Vec<Box<dyn Type>>) -> Box<dyn Type> {
        Box::new(TupleType { tuple })
    }

    pub(crate) fn get_tuple(&self) -> Vec<Box<dyn Type>> {
        self.tuple.to_vec()
    }
}

impl Type for TupleType {
    fn get_name(&self) -> String {
        self.get_signature()
    }

    fn get_signature(&self) -> String {
        format!("Tuple ({:?})", self.tuple)
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        unimplemented!()
    }

    fn is_compatible_with(&self, other: &dyn Type) -> bool {
        match other.downcast_ref::<TupleType>() {
            None => false,
            Some(other_tuple) => {
                let other_tuple_vec: Vec<Box<dyn Type>> = other_tuple.get_tuple();
                if self.tuple.len() != other_tuple_vec.len() {
                    false
                } else {
                    let mut result = true;
                    for (s, o) in self.tuple.iter().zip(other_tuple_vec.iter()) {
                        result = result && s.is_compatible_with(o.deref());
                    }
                    result
                }
            }
        }
    }

    fn match_types(&mut self, other: &dyn Type) {
        if let Some(other) = other.downcast_ref::<TupleType>() {
            let mut other_tuple_vec = other.get_tuple();
            for (s, o) in self.tuple.iter_mut().zip(other_tuple_vec.iter_mut()) {
                if TRAIT_UNKNOWN.get_signature() == s.get_signature()
                    || s.is_trait() && !o.is_trait()
                {
                    if s.is::<TupleType>() {
                        s.match_types(o.deref().deref());
                    } else {
                        *s = o.clone();
                    }
                } else {
                    s.match_types(o.deref().deref());
                }
            }
        }
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        match other.downcast_ref::<TupleType>() {
            None => TypeComparation::Incomparables,
            Some(other) => {
                if other.tuple.len() != self.tuple.len() {
                    TypeComparation::Incomparables
                } else {
                    let mut result = TypeComparation::Incomparables;
                    for (a, b) in self.tuple.iter().zip(other.tuple.iter()) {
                        match  a.compare(&*b.deref()) {
                            TypeComparation::Incomparables =>
                                return TypeComparation::Incomparables,
                            TypeComparation::Same => result = TypeComparation::Same,
                            r => result = r
                        }
                    }
                    result
                }
            }
        }
    }
}
