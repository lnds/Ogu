pub mod transpilers;

use crate::symbols::scopes::Scope;
use anyhow::Result;

pub trait CodeGenerator {
    fn process(&mut self, module: &dyn Scope) -> Result<()>;
}
