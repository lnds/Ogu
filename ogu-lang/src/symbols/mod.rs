pub mod scopes;
pub(crate) mod module;
pub(crate) mod sym_table;
pub(crate) mod macros;

use crate::backend::errors::OguError;
use anyhow::{Context, Error, Result};
use crate::types::Type;
use std::fmt::{Debug, Formatter};

pub trait Symbol : SymbolClone {
    fn get_name(&self) -> String;
    fn get_type(&self)  -> Box<dyn Type>;
}

pub trait SymbolClone {
    fn clone_box(&self) -> Box<dyn Symbol>;
}

impl<T> SymbolClone for T
where T: 'static + Symbol + Clone,
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
        write!(f, "Symbol {{ name: {}, type: {} }}", self.get_name(), self.get_type().get_name())?;
        Ok(())
    }
}

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError(msg.to_string()))).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}

