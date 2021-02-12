use anyhow::{bail, Result};

use crate::backend::modules::symbols::func::func_def::Function;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub(crate) struct ComposeFunction {
    f: Box<dyn Symbol>,
    g: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl ComposeFunction {
    pub(crate) fn new_fwd(f: Box<dyn Symbol>, g: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ComposeFunction { f, g, ty: None })
    }

    pub(crate) fn new_bck(g: Box<dyn Symbol>, f: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ComposeFunction { f, g, ty: None })
    }

    pub(crate) fn replace_args(
        &mut self,
        args: Vec<Box<dyn Symbol>>,
        scope: &mut dyn Scope,
    ) -> Result<()> {
        if let Some(f) = self.f.downcast_ref::<Function>() {
            let mut f = f.clone();
            f.replace_args(args.clone(), scope, false)?;
            self.f = Box::new(f);
        } else if let Some(id) = self.f.downcast_ref::<IdSym>() {
            let f = scope.resolve(id.get_name());
            if let Some(f) = f {
                if let Some(f) = f.downcast_ref::<Function>() {
                    let mut f = f.clone();
                    f.replace_args(args.clone(), scope, false)?;
                    self.f = Box::new(f);
                }
            }
        }

        let mut sym_table = SymbolTable::new("compose_function", Some(scope.clone_box()));
        sym_table.set_function_name(&scope.function_scope_name());
        self.f.define_into(&mut *sym_table);
        self.resolve_type(&mut *sym_table)?;

        let g_args = match self.f.downcast_ref::<Function>() {
            Some(f) => vec![f.expr.clone_box()],
            None => match self.f.downcast_ref::<IdSym>() {
                None => vec![],
                Some(id) => match scope.resolve(id.get_name()) {
                    None => vec![],
                    Some(f) => match f.downcast_ref::<Function>() {
                        None => vec![],
                        Some(f) => vec![f.expr.clone_box()],
                    },
                },
            },
        };

        if let Some(g) = self.g.downcast_ref::<Function>() {
            let mut g = g.clone();
            g.replace_args(g_args.clone(), scope, false)?;
            self.g = Box::new(g);
        } else if let Some(id) = self.g.downcast_ref::<IdSym>() {
            let g = scope.resolve(id.get_name());
            if let Some(g) = g {
                if let Some(g) = g.downcast_ref::<Function>() {
                    let mut g = g.clone();
                    g.replace_args(g_args.clone(), scope, false)?;
                    self.g = Box::new(g);
                }
            }
        }
        self.g.define_into(&mut *sym_table);
        self.resolve_type(&mut *sym_table)?;

        Ok(())
    }
}

impl Symbol for ComposeFunction {
    fn get_name(&self) -> &str {
        "compose_function"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.f.resolve_type(scope)?;
        match self.f.get_curry() {
            None => {}
            Some(fc) => self.f = fc,
        }
        self.g.resolve_type(scope)?;
        match self.g.get_curry() {
            None => {}
            Some(gc) => self.g = gc,
        }

        match self.f.get_type() {
            None => {
                if let Some(gt) = self.g.get_type() {
                    match gt.downcast_ref::<FuncType>() {
                        None => bail!("can't compose a non function"),
                        Some(gt) => {
                            // f : a -> b ( f args -> b )
                            // g : b -> c ( g args -> c )       g : c -> d -> e == g c d = e => f : ? -> c -> d
                            // f >> g : a -> c
                            let bt = match &gt.args {
                                None => BasicType::unit(),
                                Some(v) if v.is_empty() => BasicType::unit(),
                                Some(v) if v.len() == 1 => v[0].clone(),
                                Some(v) => v[v.len() - 1].clone(),
                            };
                            let at = match &gt.args {
                                None => None,
                                Some(v) if v.is_empty() => None,
                                Some(v) if v.len() == 1 => Some(vec![TRAIT_UNKNOWN.clone_box()]),
                                Some(v) => {
                                    let mut args = vec![TRAIT_UNKNOWN.clone_box()];
                                    args.append(&mut v.to_vec());
                                    Some(args)
                                }
                            };
                            let ct = gt.result.clone();
                            self.f.set_type(Some(FuncType::new_func_type(at.clone(), bt)));
                            self.ty = Some(FuncType::new_func_type(at, ct));
                        }
                    }
                }
            }
            Some(ft) => {
                match ft.downcast_ref::<FuncType>() {
                    None => {
                        bail!(
                            "can't compose a non function ft = {:?}\n curried {}\n",
                            ft,
                            self.f.get_curry().is_some()
                        )
                    }
                    Some(ft) => match self.g.get_type() {
                        None => {
                            // f: a -> b
                            // g: b -> c
                            // f >> g : a -> c
                            let a = ft.args.clone();
                            let c = TRAIT_UNKNOWN.clone_box();
                            self.g
                                .set_type(Some(FuncType::new_func_type(a.clone(), ft.result.clone())));
                            self.ty = Some(FuncType::new_func_type(a, c));
                        }
                        Some(gt) => {
                            match gt.downcast_ref::<FuncType>() {
                                None => bail!("can't compose a non function"),
                                Some(gt) => {
                                    // f: a -> b
                                    // g: b -> c
                                    // f >> g : a -> c == g(f a)
                                    let b = vec![ft.result.clone()];
                                    match &gt.args {
                                        None => bail!("can't compose a function that doesn't receive arguments"),
                                        Some(args) => {
                                            if args.iter().zip(b.iter()).any(|(at, bt)| !at.is_compatible_with(bt.deref().deref())) {
                                                bail!("can't compose functions, arguments are incompatible gt.args = {:?} b != {:?}", gt.args, b);
                                            }
                                            let c = gt.result.clone();
                                            self.ty = Some(FuncType::new_func_type(Some(b), c));
                                        }
                                    }
                                }
                            }
                        }
                    },
                }
            }
        }
        Ok(self.get_type())
    }
}
