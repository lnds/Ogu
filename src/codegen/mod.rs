pub(crate) mod transpilers;

use crate::symbols::module::Module;
use anyhow::Result;

pub(crate) trait CodeGenerator {

    fn process(&mut self, module: &Module) -> Result<()>;
}