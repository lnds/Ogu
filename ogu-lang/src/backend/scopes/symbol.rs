use crate::backend::scopes::types::Type;
use std::fmt::Debug;
use crate::backend::scopes::Scope;
use anyhow::Result;

pub(crate) trait Symbol: SymbolClone + Debug {
    fn get_name(&self) -> &str;
    fn get_type(&self) -> Option<Box<dyn Type>>;
    fn set_type(&mut self, ty: Option<Box<dyn Type>>);
    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>>;
}

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
