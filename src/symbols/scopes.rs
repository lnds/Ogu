use crate::symbols::symbols::Symbol;
use crate::backend::Compiler;

pub(crate) trait Scope {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol);
    fn resolve(&self, name: &str) -> Option<Symbol>;
    fn dump(&self);
}