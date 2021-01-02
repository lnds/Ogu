use crate::backend::modules::types::func_type::FuncType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct FuncCallSym {
    func: Box<dyn Symbol>,
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl FuncCallSym {
    pub(crate) fn new(func: Box<dyn Symbol>, args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(FuncCallSym {
            func,
            args,
            ty: None,
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
        let ft = self.func.resolve_type(scope)?;
        if let Some(ft) = ft {
            match ft.downcast_ref::<FuncType>() {
                None => {
                    self.ty = match self.func.get_type() {
                        None => None,
                        Some(t) => t.resolve_expr_type(),
                    };
                }
                Some(ft) => {
                    println!("ft is = {:?}", ft);

                    for a in self.args.iter_mut() {
                        a.resolve_type(scope)?;
                    }
                    self.ty = match self.func.get_type() {
                        None => None,
                        Some(t) => t.resolve_expr_type(),
                    };
                }
            }
        }
        Ok(self.get_type())
    }
}
