pub mod transpilers;

use crate::symbols::module::Module;
use anyhow::Result;

pub trait CodeGenerator {

    fn process(&mut self, module: &Module) -> Result<()>;
}