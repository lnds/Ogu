use crate::backend::modules::symbols::funcs::FunctionSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::variadic_type::VariadicType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::symbols::exprs::lambda_expr::LambdaExpr;

#[derive(Clone, Debug)]
pub(crate) struct FuncCallExpr {
    func: Box<dyn Symbol>,
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
}

impl FuncCallExpr {
    pub(crate) fn new(func: Box<dyn Symbol>, args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(FuncCallExpr {
            func,
            args,
            ty: None,
        })
    }
}

impl Symbol for FuncCallExpr {
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
        let recursive = scope.scope_name() == self.func.get_name();
        match ft {
            None => {}
            Some(ft) => {
                if let Some(ft) = ft.downcast_ref::<FuncType>() {
                    match ft.get_args() {
                        None => {
                            if !self.args.is_empty() {
                                bail!("function {} need no args", self.func.get_name());
                            }
                        }
                        Some(ft_args) if ft_args.len() > self.args.len() => bail!(
                            "function {} receive more args than needed",
                            self.func.get_name()
                        ),
                        Some(ft_args) if ft_args.len() < self.args.len() => {
                            println!("probably curry");
                            todo!()
                        }
                        Some(ft_args) => {
                            for (p, a) in self.args.iter_mut().enumerate() {
                                a.resolve_type(scope)?;
                                if a.get_type().is_none() {
                                    a.set_type(Some(ft_args[p].clone()));
                                }
                                scope.define(a.clone());
                            }
                            if let Some(func) = scope.resolve(self.func.get_name()) {
                                if let Some(func) = func.downcast_ref::<FunctionSym>() {
                                    let mut f = func.clone();
                                    f.replace_args(self.args.to_vec(), scope, !recursive)?;
                                    if self.func.get_type() != f.get_type() {
                                        self.func = Box::new(f);
                                        scope.define(self.func.clone()); // ojp !!
                                    }
                                } else if let Some(val) = func.downcast_ref::<ValueSym>() {
                                    if let Some(id) = val.expr.downcast_ref::<IdSym>() {
                                        match scope.resolve(id.get_name()) {
                                            None => bail!("FATAL could not find function: {}", id.get_name()),
                                            Some(fun) => {
                                                println!("SCOPE IS : {}", scope.scope_name());
                                                println!("FUN ES = {:?}", fun);
                                                if let Some(func) = fun.downcast_ref::<FunctionSym>() {
                                                    let mut f = func.clone();
                                                    f.replace_args(self.args.to_vec(), scope, !recursive)?;
                                                    println!("F queda asi: {:#?}", f);
                                                    if self.func.get_type() != f.get_type() {
                                                        println!("son tipos distintos!!!!!");
                                                        self.func = Box::new(f);
                                                    }
                                                }
                                            }
                                        }
                                    } else if let Some(lambda) = val.expr.downcast_ref::<LambdaExpr>() {
                                        let mut l = lambda.clone();
                                        l.replace_args(self.args.to_vec(), scope)?;
                                        if self.func.get_type() != l.get_type() {
                                            self.func = Box::new(l);
                                        }
                                    }

                                    else {
                                        bail!("FATAL: VAL {:?} is from invalid value!!", val);
                                    }
                                } else {
                                    bail!("WTF func = {:#?}", func);
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
                } else {
                    bail!("{} it's not a function", self.func.get_name());
                }
                self.ty = match self.func.get_type() {
                    None => None,
                    Some(t) => t.resolve_expr_type(),
                };
            }
        }
        Ok(self.get_type())
    }
}
