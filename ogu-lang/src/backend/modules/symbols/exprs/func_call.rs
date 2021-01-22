use crate::backend::modules::symbols::exprs::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::funcs::FunctionSym;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::variadic_type::VariadicType;
use crate::backend::scopes::symbol::{Symbol, SymbolClone};
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::symbols::exprs::func_compose::ComposeFunction;

#[derive(Clone, Debug)]
pub(crate) struct FuncCallExpr {
    func: Box<dyn Symbol>,
    args: Vec<Box<dyn Symbol>>,
    ty: Option<Box<dyn Type>>,
    curried: bool,
}

impl FuncCallExpr {
    pub(crate) fn new(func: Box<dyn Symbol>, args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(FuncCallExpr {
            func,
            args,
            ty: None,
            curried: false,
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
        let recursive = scope.function_scope_name() == self.func.get_name();
        match ft {
            None => {
                for a in self.args.iter_mut() {
                    a.resolve_type(scope)?;
                    a.define_into(scope);
                }
                if let Some(func) = scope.resolve(self.func.get_name()) {
                    if let Some(func) = func.downcast_ref::<FunctionSym>() {
                        let mut f = func.clone();
                        f.replace_args(self.args.to_vec(), scope, !recursive)?;
                        if self.func.get_type() != f.get_type() {
                            self.func = Box::new(f);
                        }
                    } else {
                        todo!("ESTE OTRO CASO func = {:?}", func);
                    }
                }
            }
            Some(ft) => {
                if let Some(ft) = ft.downcast_ref::<FuncType>() {
                    match ft.get_args() {
                        None => {
                            if !self.args.is_empty() {
                                bail!("function {} need no args", self.func.get_name());
                            }
                        }
                        Some(ft_args) if ft_args.len() < self.args.len() => bail!(
                            "function {} received more args than needed",
                            self.func.get_name()
                        ),
                        Some(ft_args) if ft_args.len() > self.args.len() => {
                            // curry
                            match scope.resolve(self.func.get_name()) {
                                None => {
                                    if let Some(compose) = self.func.downcast_ref::<ComposeFunction>() {
                                        let c = compose.clone();
                                        if self.func.get_type() != c.get_type() {
                                            self.func = Box::new(c);
                                        }
                                    } else {
                                        bail!( "can't find a function to curry {} func = {:?}", self.func.get_name(), self.func )
                                    }
                                }
                                Some(func) => match func.downcast_ref::<FunctionSym>() {
                                    None => bail!(
                                        "can't infer a function to curry {:?} => {:?}",
                                        func, func.get_type()
                                    ),
                                    Some(_) => {
                                        let n = ft_args.len() - self.args.len();
                                        let mut n_args = vec![];
                                        for (i, ft) in ft_args.iter().enumerate().take(n) {
                                            let mut sym: Box<dyn Symbol> =
                                                IdSym::new(&format!("x_{}", i));
                                            sym.set_type(Some(ft.clone()));
                                            n_args.push(sym)
                                        }
                                        let mut call_args = self.args.to_vec();
                                        call_args.append(&mut n_args.to_vec());
                                        let expr =
                                            FuncCallExpr::new(self.func.clone_box(), call_args);
                                        let mut lambda =
                                            LambdaExpr::new(n_args.to_vec(), expr.clone_box());
                                        lambda.resolve_type(scope)?;
                                        self.curried = true;
                                        self.func = lambda
                                    }
                                },
                            }
                        }
                        Some(ft_args) => {
                            for (p, a) in self.args.iter_mut().enumerate() {
                                a.resolve_type(scope)?;
                                if a.get_type().is_none() {
                                    a.set_type(Some(ft_args[p].clone()));
                                }
                                a.define_into(scope);
                            }
                            let func = match scope.resolve(self.func.get_name()) {
                                None => self.func.clone(),
                                Some(func) => func.clone()
                            };
                            if let Some(func) = func.downcast_ref::<FunctionSym>() {
                                let mut f = func.clone();
                                f.replace_args(self.args.to_vec(), scope, !recursive)?;
                                if self.func.get_type() != f.get_type() {
                                    self.func = Box::new(f);
                                }
                            } else if let Some(val) = func.downcast_ref::<ValueSym>() {
                                if let Some(id) = val.expr.downcast_ref::<IdSym>() {
                                    match scope.resolve(id.get_name()) {
                                        None => bail!(
                                                "FATAL could not find function: {}",
                                                id.get_name()
                                            ),
                                        Some(fun) => {
                                            if let Some(func) =
                                            fun.downcast_ref::<FunctionSym>()
                                            {
                                                let mut f = func.clone();
                                                f.replace_args(
                                                    self.args.to_vec(),
                                                    scope,
                                                    !recursive,
                                                )?;
                                                if self.func.get_type() != f.get_type() {
                                                    self.func = Box::new(f);
                                                }
                                            }
                                        }
                                    }
                                } else if let Some(lambda) = val.expr.downcast_ref::<LambdaExpr>() {
                                    let mut l = lambda.clone();
                                    l.replace_args(self.args.to_vec(), scope)?;
                                    println!("l.get_type = {:?}", l.get_type());
                                    if self.func.get_type() != l.get_type() {
                                        println!("replace func by lambda = {:?}", self.func);
                                        self.func = Box::new(l);
                                        println!("replace func by lambda = {:?}", self.func);
                                    }
                                } else if let Some(compose) = val.expr.downcast_ref::<ComposeFunction>() {
                                    let c = compose.clone();
                                    if self.func.get_type() != c.get_type() {
                                        self.func = Box::new(c);
                                    }
                                } else {
                                    bail!("FATAL: VAL {:?} is from invalid value!!", val);
                                }
                            } else if let Some(lambda) = func.downcast_ref::<LambdaExpr>() {
                                let mut l = lambda.clone();
                                l.replace_args(self.args.to_vec(), scope)?;
                                if self.func.get_type() != l.get_type() {
                                    self.func = Box::new(l);
                                }
                            } else if let Some(compose) = func.downcast_ref::<ComposeFunction>() {
                                let c = compose.clone();
                                if self.func.get_type() != c.get_type() {
                                    self.func = Box::new(c);
                                }
                            } else {
                                bail!("FATAL: func is invalid here: {:?}", func);
                            }
                        }
                    }
                } else if let Some(ft) = ft.downcast_ref::<VariadicType>() {
                    // TODO
                    println!("!! variadic {:#?}", ft);
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

    fn is_curry(&self) -> bool {
        self.curried
    }

    fn get_curry(&self) -> Option<Box<dyn Symbol>> {
        if self.curried {
            Some(self.func.clone_box())
        } else {
            None
        }
    }
}
