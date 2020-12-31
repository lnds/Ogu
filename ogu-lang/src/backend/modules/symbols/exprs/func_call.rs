use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;

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

    fn resolve_type(&mut self, scope: &dyn Scope) -> Option<Box<dyn Type>> {
        self.func.resolve_type(scope);
        self.ty = self.func.get_type();
        self.get_type()
    }
}
