use crate::symbols::types::Type;

#[derive(Debug, Clone)]
pub(crate) struct Symbol{
    name: String,
    value: SymbolValue,
}

#[derive(Debug, Clone)]
pub(crate) enum SymbolValue {
    NativeType(Type),
    Macro(Type,  usize ),
    Value(String)
}

impl Symbol {

    pub(crate) fn new(name: &str, value: SymbolValue) -> Self {
        Symbol { name: name.to_string(), value }
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
    }
}