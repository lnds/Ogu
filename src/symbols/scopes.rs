use crate::symbols::symbols::Symbol;

pub trait Scope {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
}