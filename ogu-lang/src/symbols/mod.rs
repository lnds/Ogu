pub(crate) mod decls;
pub(crate) mod exprs;
pub(crate) mod macros;
pub(crate) mod module;
pub mod scopes;
pub(crate) mod sym_table;

use crate::backend::errors::OguError;
use crate::codegen::transpilers::SymbolWriter;
use crate::symbols::scopes::Scope;
use crate::types::Type;
use anyhow::{Context, Error, Result};
use std::fmt::{Debug};

pub trait Symbol: SymbolClone + Debug {
    fn get_name(&self) -> String;
    fn get_type(&self) -> Option<Box<dyn Type>>;
    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter>;
    fn solve_type(&self, scope: &dyn Scope) -> Result<Box<dyn Symbol>>;
}

pub trait SymbolClone {
    fn clone_box(&self) -> Box<dyn Symbol>;
}

impl<T> SymbolClone for T
where
    T: 'static + Symbol + Clone,
{
    fn clone_box(&self) -> Box<dyn Symbol> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Symbol> {
    fn clone(&self) -> Box<dyn Symbol> {
        self.clone_box()
    }
}

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError)).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}
