use std::fmt::Debug;

pub(crate) trait Type: TypeClone + Debug + mopa::Any {
    fn get_name(&self) -> String;
    fn get_signature(&self) -> String;
    fn is_trait(&self) -> bool;
    fn resolve_expr_type(&self) -> Option<Box<dyn Type>>;
}

mopafy!(Type);

pub(crate) trait TypeClone {
    fn clone_box(&self) -> Box<dyn Type>;
}

impl<T> TypeClone for T
where
    T: 'static + Type + Clone,
{
    fn clone_box(&self) -> Box<dyn Type> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Type> {
    fn clone(&self) -> Box<dyn Type> {
        self.clone_box()
    }
}

impl PartialEq for Box<dyn Type> {
    fn eq(&self, other: &Self) -> bool {
        self.get_signature() == other.get_signature()
    }
}
