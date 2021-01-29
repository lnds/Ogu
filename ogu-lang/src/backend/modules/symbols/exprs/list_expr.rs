use anyhow::{bail, Result};

use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;

#[derive(Debug, Clone)]
pub(crate) struct ListExpr {
    kind: ListExprKind,
    ty: Option<Box<dyn Type>>,
}

#[derive(Debug, Clone)]
enum ListExprKind {
    EmptyList,
    List(Vec<Box<dyn Symbol>>),
    Cons(Box<dyn Symbol>, Box<dyn Symbol>),
    Concat(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl ListExpr {
    pub(crate) fn new_empty() -> Box<Self> {
        Box::new(ListExpr {
            kind: ListExprKind::EmptyList,
            ty: None,
        })
    }

    pub(crate) fn new_list(exprs: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(ListExpr {
            kind: ListExprKind::List(exprs),
            ty: None,
        })
    }

    pub(crate) fn new_cons(atom: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListExpr {
            kind: ListExprKind::Cons(atom, list),
            ty: None,
        })
    }

    pub(crate) fn new_concat(list1: Box<dyn Symbol>, list2: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListExpr {
            kind: ListExprKind::Concat(list1, list2),
            ty: None,
        })
    }
}

impl Symbol for ListExpr {
    fn get_name(&self) -> &str {
        "list_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty;
    }

    fn matches_types(&mut self, aty: Option<Box<dyn Type>>) {
        if let Some(bty) = aty {
            if let Some(lt) = bty.downcast_ref::<ListType>() {
                match &mut self.kind {
                    ListExprKind::EmptyList => {}
                    ListExprKind::List(vec) => {
                        for v in vec.iter_mut() {
                            v.matches_types(Some(lt.ty.clone()))
                        }
                    }
                    ListExprKind::Cons(a, b) => {
                        a.matches_types(Some(lt.ty.clone()));
                        b.matches_types(Some(lt.clone_box()));
                    }
                    ListExprKind::Concat(a, b) => {
                        a.matches_types(Some(lt.ty.clone()));
                        b.matches_types(Some(lt.clone_box()));
                    }
                }
                self.ty = Some(lt.clone_box());
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match &mut self.kind {
            ListExprKind::EmptyList => {
                self.ty = Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()))
            }
            ListExprKind::List(e) if e.is_empty() => {
                self.ty = Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()))
            } // weird
            ListExprKind::List(exprs) => {
                for e in exprs.iter_mut() {
                    e.resolve_type(scope)?;
                }
                let ty = exprs[0].get_type();
                match ty {
                    None => {}
                    Some(ty) => {
                        let not_same = exprs.iter().any(|t| match t.get_type() {
                            None => true,
                            Some(t) => !ty.is_compatible_with(&*t),
                        });
                        if not_same {
                            bail!("list must have all elements of same type")
                        } else {
                            self.ty = Some(ListType::new_list(ty))
                        }
                    }
                }
            }
            ListExprKind::Cons(atom, list) => {
                atom.resolve_type(scope)?;
                list.resolve_type(scope)?;
                match atom.get_type() {
                    None => match list.get_type() {
                        None => {
                            atom.set_type(Some(TRAIT_UNKNOWN.clone_box()));
                            atom.define_into(scope);
                            list.set_type(Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                            list.define_into(scope);
                            self.ty = Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()));
                        }
                        Some(lt) => match lt.downcast_ref::<ListType>() {
                            None => bail!("attempt to make a cons without a list lt = {:?}", lt),
                            _ => self.ty = list.get_type(),
                        },
                    },
                    Some(at) => {
                        match list.get_type() {
                            None => {
                                list.set_type(Some(ListType::new_list(at)));
                                scope.define(list.clone());
                                self.ty = list.get_type()
                            }
                            Some(lt) => match lt.downcast_ref::<ListType>() {
                                None => {
                                    bail!("attempt to make a cons without a list lt = {:?}", lt)
                                }
                                Some(lt) if !lt.ty.is_compatible_with(&*at.clone()) => {
                                    bail!("incompatible types in cons expression lt = {:?}, at = {:?}", lt, at)
                                }
                                Some(lt) if lt.ty.get_signature() != at.get_signature() => {
                                    if at.is_trait() && !at.is_compatible_with(&*lt.ty) {
                                        atom.set_type(Some(lt.ty.clone()));
                                        atom.define_into(scope);
                                    } else if lt.ty.is_trait() {
                                        list.set_type(Some(ListType::new_list(at.clone_box())));
                                        list.define_into(scope);
                                    } else {
                                        bail!("cons expression of different list types at = {:?}. lt={:?}", at, lt);
                                    }
                                    self.ty = list.get_type();
                                }
                                Some(_) => {
                                    self.ty = list.get_type();
                                }
                            },
                        }
                    }
                }
            }
            ListExprKind::Concat(list1, list2) => {
                list1.resolve_type(scope)?;
                list2.resolve_type(scope)?;
                match list1.get_type() {
                    None => match list2.get_type() {
                        None => {
                            list1.set_type(Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                            list2.set_type(Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())));
                            list1.define_into(scope);
                            list2.define_into(scope);
                            self.ty = Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()));
                        }
                        Some(_) => {
                            list1.set_type(list2.get_type());
                            list1.define_into(scope);
                            self.ty = list2.get_type();
                        }
                    },
                    Some(lt1) => match list2.get_type() {
                        None => {
                            list2.set_type(list1.get_type());
                            list2.define_into(scope);
                            self.ty = list1.get_type();
                        }
                        Some(lt2) => match lt1.downcast_ref::<ListType>() {
                            None => bail!("concat expression left side is not a list"),
                            Some(lt1) => match lt2.downcast_ref::<ListType>() {
                                None => bail!("concat expression right side is not a list"),
                                Some(lt2) if lt1.ty != lt2.ty.clone() => {
                                    if lt1.ty.is_trait() {
                                        list1.set_type(list2.get_type());
                                        list1.define_into(scope);
                                    } else if lt2.ty.is_trait() {
                                        list2.set_type(list1.get_type());
                                        list2.define_into(scope);
                                    } else {
                                        bail!("concat expression of different list types lt1 = {:?} lt2={:?}", lt1, lt2);
                                    }
                                    self.ty = list1.get_type()
                                }
                                _ => self.ty = list1.get_type(),
                            },
                        },
                    },
                }
            }
        };
        Ok(self.get_type())
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        match &self.kind {
            ListExprKind::EmptyList => {}
            ListExprKind::List(v) => {
                for e in v.iter() {
                    e.define_into(scope);
                }
            }
            ListExprKind::Cons(a, l) => {
                a.define_into(scope);
                l.define_into(scope);
            }
            ListExprKind::Concat(l, r) => {
                l.define_into(scope);
                r.define_into(scope);
            }
        };
        None
    }

    fn is_seq(&self) -> bool {
        true
    }
}
