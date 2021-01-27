use anyhow::Result;

use crate::backend::modules::symbols::func::func_call::FuncCallExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct RecurCallExpr {
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl RecurCallExpr {
    pub(crate) fn new(args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(RecurCallExpr { args, ty: None })
    }
}

impl Symbol for RecurCallExpr {
    fn get_name(&self) -> &str {
        "recur_call"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty;
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let func_name = scope.function_scope_name();
        let mut func_call: Box<dyn Symbol> =
            FuncCallExpr::new(IdSym::new(&func_name), self.args.to_vec());
        func_call.resolve_type(scope)?;
        self.set_type(func_call.get_type());
        Ok(self.get_type())
    }
}
