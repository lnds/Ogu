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

    fn promotes(&self, _other: &dyn Type) -> bool {
        unimplemented!()
    }
}
