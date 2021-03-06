use anyhow::{bail, Result};

use crate::backend::modules::symbols::func::func_compose::ComposeFunction;
use crate::backend::modules::symbols::func::func_def::Function;
use crate::backend::modules::symbols::func::lambda_expr::LambdaExpr;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::symbols::values::ValueSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::types::variadic_type::VariadicType;
use crate::backend::scopes::symbol::{Symbol, SymbolClone};
use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) struct FuncCallExpr {
    func: Box<dyn Symbol>,
    pub(crate) args: Vec<Box<dyn Symbol>>,
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
        self.ty = ty.clone();
        if let Some(ty) = &self.ty {
            if let Some(ft) = self.func.get_type() {
                if let Some(ft) = ft.downcast_ref::<FuncType>() {
                    let mut ft = ft.clone();
                    if ft.result.compare(&*ty.clone()) == TypeComparation::Inferior {
                        ft.result = ty.clone();
                        self.func.set_type(Some(ft.clone_box()));
                    }
                }
            }
        }
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
                let func = if let Some(func) = scope.resolve(self.func.get_name()) {
                    func
                } else {
                    self.func.clone()
                };
                if let Some(func) = func.downcast_ref::<Function>() {
                    let mut f = func.clone();
                    f.replace_args(self.args.to_vec(), scope, !recursive)?;
                    if self.func.get_type() != f.get_type() {
                        self.func = Box::new(f);
                    }
                } else if let Some(id) = func.downcast_ref::<IdSym>() {
                    let id = id.clone();
                    if self.func.get_type() != id.get_type() {
                        self.func = Box::new(id)
                    }
                } else if let Some(lambda) = func.downcast_ref::<LambdaExpr>() {
                    let mut lambda = lambda.clone();
                    lambda.replace_args(self.args.to_vec(), scope)?;
                    if self.func.get_type() != lambda.get_type() {
                        self.func = Box::new(lambda);
                    } else {
                        let mut ty_args = vec![];
                        for a in self.args.iter_mut() {
                            match a.resolve_type(scope)? {
                                None => {
                                    ty_args.push(TRAIT_UNKNOWN.clone_box());
                                    a.define_into(scope);
                                }
                                Some(ty) => {
                                    ty_args.push(ty.clone_box());
                                    a.define_into(scope);
                                }
                            }
                        }
                        let tf = TRAIT_UNKNOWN.clone();
                        self.func
                            .set_type(Some(FuncType::new_func_type(Some(ty_args), tf.clone_box())));
                        self.func.define_into(scope);
                    }
                } else {
                    todo!("WTF!! func = {:?}", func);
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
                                None => match self.func.downcast_ref::<ComposeFunction>() {
                                    None => bail!(
                                        "can't find a function to curry {} func = {:?}",
                                        self.func.get_name(),
                                        self.func
                                    ),
                                    Some(compose) => {
                                        let mut c = compose.clone();
                                        c.replace_args(self.args.to_vec(), scope)?;

                                        if self.func.get_type() != c.get_type() {
                                            self.func = Box::new(c);
                                        }
                                        self.curried = true;
                                    }
                                },
                                Some(func) => {
                                    let n = ft_args.len() - self.args.len();
                                    let skip = ft_args.len() - n;
                                    let mut n_args = vec![];
                                    for (i, ft) in ft_args.iter().enumerate() {
                                        if i >= skip {
                                            let mut sym: Box<dyn Symbol> =
                                                IdSym::new(&format!("x_{}", i));
                                            sym.set_type(Some(ft.clone()));
                                            n_args.push(sym)
                                        }
                                    }
                                    let mut call_args = self.args.to_vec();
                                    call_args.append(&mut n_args.to_vec());
                                    let expr =
                                        FuncCallExpr::new(func.clone_box(), call_args.clone());
                                    let mut lambda =
                                        LambdaExpr::new(n_args.to_vec(), expr.clone_box());
                                    lambda.resolve_type(scope)?;
                                    self.curried = true;
                                    self.func = lambda;
                                    self.args = n_args.to_vec();
                                }
                            }
                        }
                        Some(ft_args) => {
                            for (a, ot) in self.args.iter_mut().zip(ft_args.iter()) {
                                a.resolve_type(scope)?;
                                match a.get_type() {
                                    None => a.set_type(Some(ot.clone())),
                                    Some(t) => {
                                        let ftt = ot.clone();
                                        if ftt.compare(&*t) == TypeComparation::Superior {
                                            a.set_type(Some(ftt));
                                        }
                                    }
                                }
                                a.define_into(scope);
                            }
                            let func = match scope.resolve(self.func.get_name()) {
                                None => self.func.clone(),
                                Some(func) => func.clone(),
                            };
                            if let Some(func) = func.downcast_ref::<Function>() {
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
                                            if let Some(func) = fun.downcast_ref::<Function>() {
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
                                    if self.func.get_type() != l.get_type() {
                                        self.func = Box::new(l);
                                    }
                                } else if let Some(comp) =
                                    val.expr.downcast_ref::<ComposeFunction>()
                                {
                                    let mut c = comp.clone();
                                    c.replace_args(self.args.to_vec(), scope)?;
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
                                let mut c = compose.clone();
                                c.replace_args(self.args.to_vec(), scope)?;
                                if self.func.get_type() != c.get_type() {
                                    self.func = Box::new(c);
                                }
                            } else if let Some(id) = func.downcast_ref::<IdSym>() {
                                let id = id.clone();
                                if self.func.get_type() != id.get_type() {
                                    self.func = Box::new(id)
                                }
                            } else {
                                bail!(
                                    "FATAL: func is invalid here: {:?} => {:?}",
                                    func,
                                    func.get_type()
                                );
                            }
                        }
                    }
                } else if ft.downcast_ref::<VariadicType>().is_some() {
                    // TODO
                } else if let Some(tf) = self.func.get_type() {
                    let mut ty_args = vec![];
                    for a in self.args.iter_mut() {
                        match a.resolve_type(scope)? {
                            None => {
                                ty_args.push(TRAIT_UNKNOWN.clone_box());
                                a.define_into(scope);
                            }
                            Some(ty) => {
                                ty_args.push(ty.clone_box());
                                a.define_into(scope);
                            }
                        }
                    }
                    self.func
                        .set_type(Some(FuncType::new_func_type(Some(ty_args), tf.clone_box())));
                    self.func.define_into(scope);
                } else {
                    bail!(
                        "{} => {:?} it's not a function",
                        self.func.get_name(),
                        self.func.get_type()
                    );
                }
            }
        }
        self.ty = match self.func.get_type() {
            None => None,
            Some(t) => t.resolve_expr_type(),
        };
        Ok(self.get_type())
    }

    fn get_curry(&self) -> Option<Box<dyn Symbol>> {
        if self.curried {
            Some(self.func.clone_box())
        } else {
            None
        }
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        self.func.define_into(scope);
        for a in self.args.iter() {
            a.define_into(scope);
        }
        None
    }
}
