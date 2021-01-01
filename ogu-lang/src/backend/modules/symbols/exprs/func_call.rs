use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Result, Error};
use crate::backend::errors::OguError;

#[derive(Clone, Debug)]
pub(crate) struct FuncCallSym {
    func: Box<dyn Symbol>,
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>
}

impl FuncCallSym {
    pub(crate) fn new(func: Box<dyn Symbol>, args: Vec<Box<dyn Symbol>>) -> Box<Self>{
        Box::new(FuncCallSym {
            func,
            args,
            ty: None
        })
    }
}

impl Symbol for FuncCallSym {
    fn get_name(&self) -> &str {
        "func_call"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()

    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.func.resolve_type(scope)?;
        for a in self.args.iter_mut() {
            a.resolve_type(scope)?;
        }
        self.ty = self.func.get_type();
        /// TODO: simplify type from func_type
        Ok(self.get_type())
    }
}
