use anyhow::Result;
use std::fmt::{Debug, Formatter};

pub(crate) mod basic;
pub(crate) mod generic;

pub trait Type: TypeClone  {
    fn get_name(&self) -> String;
    fn is_generic(&self) -> bool;
    fn get_full_name(&self) -> String;
    fn signature(&self) -> String;
    fn is_equivalent(&self, other: &dyn Type) -> bool;
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



pub(crate) fn promote_type(lt: &dyn Type, rt: &dyn Type) -> Option<Box<dyn Type>> {
    if lt.is_generic() {
        return Some(rt.clone_box());
    }
    if rt.is_generic() {
        return Some(lt.clone_box());
    }
    if lt.is_equivalent(rt) {
        return Some(rt.clone_box());
    }
    None
}
