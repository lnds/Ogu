use crate::symbols::scopes::Scope;
use crate::types::Type;
use anyhow::Result;
use std::fs::File;
use std::path::PathBuf;

pub mod rust_transpiler;

pub trait Transpiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()>;
    fn dump(&mut self, module: &dyn Scope) -> Result<()>;
}

pub trait Formatter {
    fn format_func_header(&self, name: String, args: String, ty: String) -> String;
    fn format_type(&self, ty: Box<dyn Type>) -> String;
}

pub trait SymbolWriter {
    fn write_symbol(&self, fmt: &Box<dyn Formatter>, file: &mut File) -> Result<()>;
}
