use std::fmt::{Debug, Formatter};
use anyhow::Result;

pub(crate) mod basic;

pub trait Type : TypeClone {

    fn get_name(&self) -> String;

}

pub trait TypeClone {
    fn clone_box(&self) -> Box<dyn Type>;
}


impl<T> TypeClone for T
where T: 'static + Type + Clone,
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