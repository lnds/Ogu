use anyhow::Result;
use std::fmt::{Debug, Formatter};

pub(crate) mod basic;

pub trait Type: TypeClone {
    fn get_name(&self) -> String;
}

pub trait TypeClone {
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

impl Debug for dyn Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Type {}", self.get_name())?;
        Ok(())
    }
}