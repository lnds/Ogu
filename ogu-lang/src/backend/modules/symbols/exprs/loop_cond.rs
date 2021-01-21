use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) enum LoopGuard {
    Until(Box<dyn Symbol>),
    While(Box<dyn Symbol>),
}

impl Symbol for LoopGuard {
    fn get_name(&self) -> &str {
        unimplemented!()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        unimplemented!()
    }

    fn resolve_type(&mut self, _: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        unimplemented!()
    }
}



