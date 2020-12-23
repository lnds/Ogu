use crate::symbols::Symbol;

pub(crate) trait Scope {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
    fn dump(&self);
}
