use crate::backend::errors::OguError;
use crate::backend::modules::symbols::funcs::FunctionSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::variadic_type::VariadicType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct FuncCallSym {
    func: Box<dyn Symbol>,
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl FuncCallSym {
    pub(crate) fn new(func: Box<dyn Symbol>, args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        println!("new func call {:?} {:?}", func, args);
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

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty;
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let ft = self.func.resolve_type(scope)?;
        if let Some(ft) = ft {
            println!("RESOLVED as : {:#?}", self);

            if let Some(ft) = ft.downcast_ref::<FuncType>() {
                println!("ft is = {:?}", ft);
                match ft.get_args() {
                    None => {
                        if !self.args.is_empty() {
                            return Err(Error::new(OguError::SemanticError).context(format!(
                                "function {} need no args",
                                self.func.get_name()
                            )));
                        }
                    }
                    Some(ft_args) if ft_args.len() > self.args.len() => {
                        return Err(Error::new(OguError::SemanticError).context(format!(
                            "function {} receive more args than needed",
                            self.func.get_name()
                        )))
                    }
                    Some(ft_args) if ft_args.len() < self.args.len() => {
                        println!("probably curry");
                        todo!()
                    }
                    Some(ft_args) => {
                        for (p, a) in self.args.iter_mut().enumerate() {
                            a.set_type(Some(ft_args[p].clone()));
                            scope.define(a.clone());
                        }
                        if let Some(func) = scope.resolve(self.func.get_name()) {
                            if let Some(func) = func.downcast_ref::<FunctionSym>() {
                                let mut f = func.clone();
                                f.replace_args(self.args.to_vec(), scope)?;
                                if self.func.get_type() != f.get_type() {
                                    self.func = Box::new(f);
                                    scope.define(self.func.clone()); // ojp !!
                                }
                            }
                        }
                    }
                }
            } else if let Some(ft) = ft.downcast_ref::<VariadicType>() {
                // TODO
                println!(
                    "!! algo hay que hacer con el variadic (tipico son las macros) {:#?}",
                    ft
                );
            }
            self.ty = match self.func.get_type() {
                None => None,
                Some(t) => t.resolve_expr_type(),
            };
        }
        Ok(self.get_type())
    }

}
