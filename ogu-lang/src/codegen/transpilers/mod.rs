use std::path::PathBuf;
use anyhow::Result;
use crate::symbols::scopes::Scope;

pub mod rust_transpiler;

pub trait Transpiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()>;
    fn dump(&mut self, module: &dyn Scope) -> Result<()>;
}