pub mod transpilers;

use anyhow::Result;
use crate::symbols::scopes::Scope;

pub trait CodeGenerator {

    fn process(&mut self, module: &dyn Scope) -> Result<()>;
}