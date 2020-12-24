use std::path::PathBuf;
use anyhow::Result;
use crate::symbols::module::Module;

pub(crate) mod rust_transpiler;

trait Transpiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()>;
    fn dump(&mut self, module: &Module) -> Result<()>;
}