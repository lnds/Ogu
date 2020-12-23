use crate::symbols::types::Type;
use crate::symbols::{Symbol, SymbolValue};

pub(crate) struct Date {}

impl Date {
    pub(crate) fn make(name: &str, value: String) -> Symbol {
        Symbol::new(name, SymbolValue::Date(value))
    }
}
