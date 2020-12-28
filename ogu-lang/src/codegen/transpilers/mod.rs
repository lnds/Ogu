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
    fn format_id(&self, id: &str) -> String;
    fn format_str(&self, s: &str) -> String;
    fn format_int(&self, s: &str) -> String;
    fn format_func_call(&self, f: &str, args: &str) -> String;
    fn format_const_decl_header(&self, name: &str, ty: &str) -> String;
}

pub trait SymbolWriter {
    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<()>;
}
