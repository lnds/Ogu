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
    fn format_generic_func_header(
        &self,
        name: String,
        params: String,
        args: String,
        ty: String,
    ) -> String;
    fn format_func_arg(&self, name: String, ty: &dyn Type) -> String;
    fn format_type(&self, ty: &dyn Type) -> String;
    fn format_type_with_traits(&self, ty: &dyn Type) -> String;
    fn format_id(&self, id: &str) -> String;
    fn format_str(&self, s: &str) -> String;
    fn format_int(&self, s: &str) -> String;
    fn format_func_call(&self, f: &str, args: &str) -> String;
    fn format_const_decl_header(&self, name: &str, ty: &str) -> String;
    fn format_if_expr(&self, cond: &str, then_part: &str, else_part: &str) -> String;
    fn format_bin_op(&self, op: &str, l: &str, r: &str) -> String;
    fn format_eq(&self, l: &str, r: &str) -> String;
    fn format_ne(&self, l: &str, r: &str) -> String;
    fn format_ge(&self, l: &str, r: &str) -> String;
    fn format_gt(&self, l: &str, r: &str) -> String;
    fn format_le(&self, l: &str, r: &str) -> String;
    fn format_lt(&self, l: &str, r: &str) -> String;
    fn format_add(&self, l: &str, r: &str) -> String;
    fn format_sub(&self, l: &str, r: &str) -> String;
    fn format_mul(&self, l: &str, r: &str) -> String;
    fn format_div(&self, l: &str, r: &str) -> String;
    fn format_mod(&self, l: &str, r: &str) -> String;
    fn format_val_decl(&self, v: &str, expr: &str) -> String;

}

pub trait SymbolWriter {
    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<()>;
}
