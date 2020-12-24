use crate::symbols::{Symbol, SymbolValue};

pub(crate) struct Date {}

impl Date {
    pub(crate) fn make(name: &str, value: &str) -> Symbol {
        Symbol::new(name, SymbolValue::Date(value.to_string()))
    }
}

pub(crate) struct Int {}

impl Int {
    pub(crate) fn make(name: &str, value: &str) -> Symbol {
        Symbol::new(name, SymbolValue::Int(value.to_string()))
    }
}

pub(crate) struct Str {}

impl Str {
    pub(crate) fn make(name: &str, value: &str) -> Symbol {
        Symbol::new(name, SymbolValue::Str(value.to_string()))
    }
}
