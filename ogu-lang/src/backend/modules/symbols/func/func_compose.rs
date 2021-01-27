use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Debug, Clone)]
pub(crate) struct ComposeFunction {
    kind: ComposeKind,
    ty: Option<Box<dyn Type>>,
}

#[derive(Debug, Clone)]
enum ComposeKind {
    Fwd(Box<dyn Symbol>, Box<dyn Symbol>), // (a -> b) -> (b -> c) -> a -> c
    Bck(Box<dyn Symbol>, Box<dyn Symbol>), // (a -> b) -> (b -> c) -> a -> c
}

impl ComposeFunction {
    pub(crate) fn new_fwd(f: Box<dyn Symbol>, g: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ComposeFunction {
            kind: ComposeKind::Fwd(f, g),
            ty: None,
        })
    }

    pub(crate) fn new_bck(f: Box<dyn Symbol>, g: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ComposeFunction {
            kind: ComposeKind::Bck(f, g),
            ty: None,
        })
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
        match &mut self.kind {
            ComposeKind::Bck(g, f) | ComposeKind::Fwd(f, g) => {
                f.resolve_type(scope)?;
                g.resolve_type(scope)?;
                match f.get_type() {
                    None => {
                        if let Some(gt) = g.get_type() {
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
                                        Some(v) if v.len() == 1 => {
                                            Some(vec![TRAIT_UNKNOWN.clone_box()])
                                        }
                                        Some(v) => {
                                            let mut args = vec![TRAIT_UNKNOWN.clone_box()];
                                            args.append(&mut v.to_vec());
                                            Some(args)
                                        }
                                    };
                                    let ct = gt.result.clone();
                                    f.set_type(FuncType::new_opt(at.clone(), bt));
                                    self.ty = FuncType::new_opt(at, ct);
                                }
                            }
                        }
                    }
                    Some(ft) => {
                        match ft.downcast_ref::<FuncType>() {
                            None => bail!("can't compose a non function"),
                            Some(ft) => match g.get_type() {
                                None => {
                                    // f: a -> b
                                    // g: b -> c
                                    // f >> g : a -> c
                                    let a = ft.args.clone();
                                    let c = TRAIT_UNKNOWN.clone_box();
                                    g.set_type(FuncType::new_opt(a.clone(), ft.result.clone()));
                                    self.ty = FuncType::new_opt(a, c);
                                }
                                Some(gt) => {
                                    match gt.downcast_ref::<FuncType>() {
                                        None => bail!("can't compose a non function"),
                                        Some(gt) => {
                                            // f: a -> b
                                            // g: b -> c
                                            // f >> g : a -> c
                                            let b = Some(vec![ft.result.clone()]);
                                            if gt.args != b {
                                                bail!("can't compose functions, arguments are incompatible");
                                            }
                                            let c = gt.result.clone();
                                            self.ty = FuncType::new_opt(b, c);
                                        }
                                    }
                                }
                            },
                        }
                    }
                }
            }
        }
        Ok(self.get_type())
    }
}
