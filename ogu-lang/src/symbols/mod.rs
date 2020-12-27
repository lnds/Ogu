pub(crate) mod decls;
pub(crate) mod macros;
pub(crate) mod module;
pub mod scopes;
pub(crate) mod sym_table;

use crate::backend::errors::OguError;
use crate::codegen::transpilers::SymbolWriter;
use crate::types::Type;
use anyhow::{Context, Error, Result};
use std::fmt::{Debug, Formatter};

pub trait Symbol: SymbolClone {
    fn get_name(&self) -> String;
    fn get_type(&self) -> Option<Box<dyn Type>>;
    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter>;
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

impl Debug for dyn Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "Symbol {{ name: {}, type: {:?} }}",
            self.get_name(),
            self.get_type()
        )?;
        Ok(())
    }
}

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError(msg.to_string()))).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}
