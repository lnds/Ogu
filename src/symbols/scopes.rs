use crate::symbols::Symbol;
use crate::codegen::CodeGenerator;
use anyhow::Result;

pub(crate) trait Scope {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Symbol) -> Option<Symbol>;
    fn resolve(&self, name: &str) -> Option<Symbol>;
    fn dump(&self);
    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()>;
}
