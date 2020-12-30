use crate::backend::scopes::symbol::Symbol;

pub(crate) trait Scope: ScopeClone {
    fn scope_name(&self) -> &str;
    fn get_enclosing_scope(&self) -> Option<&dyn Scope>;
    fn define(&mut self, sym: &dyn Symbol) -> Option<Box<Symbol>>;
    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>>;
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