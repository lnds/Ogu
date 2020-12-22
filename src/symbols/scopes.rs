use crate::symbols::symbols::Symbol;

pub trait Scope<'a> {
    fn push_scope(self: Box<Self>, name: &str) -> Box<dyn Scope<'a> + 'a>;
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
}