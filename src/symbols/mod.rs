pub(crate) mod module;
pub(crate) mod scopes;
pub(crate) mod sym_table;
pub(crate) mod types;
pub(crate) mod values;

use crate::backend::errors::OguError;
use crate::backend::Compiler;
use crate::symbols::scopes::Scope;
use crate::symbols::types::Type;
use anyhow::{Context, Error, Result};
use std::collections::HashMap;

pub(crate) fn raise_symbol_table_error<T>(msg: &str, symbol: String, module: String) -> Result<T> {
    Err(Error::new(OguError::SymbolTableError(msg.to_string()))).context(format!(
        "Error: {}. Symbol: {}, Module: {}.",
        msg, symbol, module
    ))
}

#[derive(Debug, Clone)]
pub(crate) struct Symbol {
    name: String,
    value: SymbolValue,
}

#[derive(Debug, Clone)]
pub(crate) enum SymbolValue {
    Macro(Type, usize),
    Date(String),
}

impl Symbol {
    pub(crate) fn new(name: &str, value: SymbolValue) -> Self {
        Symbol {
            name: name.to_string(),
            value,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
    }
}

pub(crate) struct Macro {}

impl Macro {
    pub fn make(name: &str, ty: Type, args: usize) -> Symbol {
        Symbol::new(name, SymbolValue::Macro(ty, args))
    }
}
