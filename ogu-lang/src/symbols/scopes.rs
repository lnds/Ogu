use crate::codegen::CodeGenerator;
use crate::symbols::Symbol;
use anyhow::Result;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

pub trait Scope: ScopeClone {
    fn scope_name(&self) -> &str;
    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>>;
    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>>;
    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()>;
    fn get_symbols(&self) -> Vec<Box<dyn Symbol>>;
    fn set_symbols(&mut self, syms: HashMap<String, Box<dyn Symbol>>);
}

pub(crate) fn solve_symbols_types(
    scope: &Box<dyn Scope>,
) -> Result<HashMap<String, Box<dyn Symbol>>> {
    let mut symbols = HashMap::new();
    let scope_symbols = scope.get_symbols();
    for sym in scope_symbols.iter() {
        let result = sym.solve_type(scope)?;
        symbols.insert(sym.get_name(), result);
    }
    Ok(symbols)
}

pub trait ScopeClone {
    fn clone_box(&self) -> Box<dyn Scope>;
}

impl<T> ScopeClone for T
where
    T: 'static + Scope + Clone,
{
    fn clone_box(&self) -> Box<dyn Scope> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Scope> {
    fn clone(&self) -> Box<dyn Scope> {
        self.clone_box()
    }
}

impl Debug for dyn Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "Scope {} {{ ", self.scope_name())?;
        for sym in self.get_symbols().iter() {
            writeln!(f, "\t{:?}", sym)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
