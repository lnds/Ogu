use std::ops::Deref;

use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::types::Type;

#[derive(Clone, Debug)]
pub(crate) struct TupleType {
    tuple: Vec<Box<dyn Type>>,
}

impl TupleType {
    pub(crate) fn new(tuple: Vec<Box<dyn Type>>) -> Self {
        TupleType { tuple }
    }

    pub(crate) fn get_tuple(&self) -> Vec<Box<dyn Type>> {
        self.tuple.to_vec()
    }
}

impl Type for TupleType {
    fn get_name(&self) -> String {
        unimplemented!()
    }

    fn get_signature(&self) -> String {
        unimplemented!()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        unimplemented!()
    }

    fn promotes(&self, other: &dyn Type) -> bool {
        match other.downcast_ref::<TupleType>() {
            None => false,
            Some(other_tuple) => {
                let other_tuple_vec: Vec<Box<dyn Type>> = other_tuple.get_tuple();
                if self.tuple.len() != other_tuple_vec.len() {
                    false
                } else {
                    let mut result = true;
                    for (s, o) in self.tuple.iter().zip(other_tuple_vec.iter()) {
                        result = result && s.promotes(o.deref());
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
                if TRAIT_UNKNOWN.get_signature() == s.get_signature() {
                    *s = o.clone();
                } else if o.get_signature() == TRAIT_UNKNOWN.get_signature() {
                    *o = s.clone();
                }
            }
        }
    }
}
