use std::path::PathBuf;
use anyhow::Result;
use crate::symbols::module::Module;

pub mod rust_transpiler;

pub trait Transpiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()>;
    fn dump(&mut self, module: &Module) -> Result<()>;
}