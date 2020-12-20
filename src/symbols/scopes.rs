use crate::symbols::symbols::Symbol;

pub trait Scope {
    fn scope_name(&self) -> &'static str;
    fn push(&mut self, scope: &'static dyn Scope);
    fn pop(&mut self) -> Option<&'static dyn Scope>;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
}