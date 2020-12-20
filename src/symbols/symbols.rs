use crate::symbols::types::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    NativeType(&'static str, Type),
    Macro(&'static str, Type,  usize ),
}

impl Symbol {

    pub fn get_name(&self) -> &'static str {
        match *self {
            Symbol::NativeType(name, ..) => name,
            Symbol::Macro(name, ..) => name,
        }
    }
}