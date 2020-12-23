use crate::symbols::symbols::Symbol;

pub trait Scope<'a> {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
}