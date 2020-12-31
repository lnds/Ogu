use crate::backend::scopes::symbol::Symbol;
use std::fmt::Debug;

pub(crate) trait Scope: ScopeClone + Debug {
    fn scope_name(&self) -> &str;
    fn get_enclosing_scope(&self) -> Option<Box<dyn Scope>>;
    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>>;
    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>>;
    fn get_symbols(&self) -> Vec<Box<dyn Symbol>>;
    fn set_symbols(&mut self, symbols: Vec<Box<dyn Symbol>>);
}

pub(crate) trait ScopeClone {
    fn clone_box(&self) -> Box<dyn Scope>;
}

impl<T> ScopeClone for T
where
    T: 'static + Scope + Clone,
{
    fn clone_box(&self) -> Box<dyn Scope> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Scope> {
    fn clone(&self) -> Box<dyn Scope> {
        self.clone_box()
    }
}
