#![allow(clippy::transmute_ptr_to_ref)]

use std::fmt::Debug;

use anyhow::Result;

use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

pub(crate) trait Symbol: SymbolClone + Debug + mopa::Any {
    fn get_name(&self) -> &str;
    fn get_type(&self) -> Option<Box<dyn Type>>;
    fn set_type(&mut self, _: Option<Box<dyn Type>>) {}
    fn matches_types(&mut self, _: Option<Box<dyn Type>>) {}
    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>>;
    fn storable(&self) -> bool {
        false
    }
    fn set_storable(&mut self, _: bool) {}
    fn is_curry(&self) -> bool { false }
    fn get_curry(&self) -> Option<Box<dyn Symbol>> { None }
}

mopafy!(Symbol);

pub(crate) trait SymbolClone {
    fn clone_box(&self) -> Box<dyn Symbol>;
}

impl<T> SymbolClone for T
where
    T: 'static + Symbol + Clone,
{
    fn clone_box(&self) -> Box<dyn Symbol> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Symbol> {
    fn clone(&self) -> Box<dyn Symbol> {
        self.clone_box()
    }
}
