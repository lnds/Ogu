use crate::backend::modules::types::list_type::ListType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Debug, Clone)]
pub(crate) enum ListExpr {
    EmptyList,
    List(Vec<Box<dyn Symbol>>),
    Cons(Box<dyn Symbol>, Box<dyn Symbol>),
    Concat(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl ListExpr {
    pub(crate) fn new_empty() -> Box<Self> {
        Box::new(ListExpr::EmptyList)
    }

    pub(crate) fn new_list(exprs: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(ListExpr::List(exprs))
    }

    pub(crate) fn new_cons(atom: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListExpr::Cons(atom, list))
    }

    pub(crate) fn new_concat(list1: Box<dyn Symbol>, list2: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListExpr::Concat(list1, list2))
    }
}

impl Symbol for ListExpr {
    fn get_name(&self) -> &str {
        "list_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ListExpr::EmptyList => Some(ListType::new_empty()),
            ListExpr::List(v) if v.is_empty() => Some(ListType::new_empty()),
            ListExpr::List(v) => match v[0].get_type() {
                None => None,
                Some(t) => Some(ListType::new_list(t.clone_box())),
            },
            ListExpr::Cons(a, l) => match l.get_type() {
                None => None,
                Some(lt) => match lt.downcast_ref::<ListType>() {
                    None => None,
                    Some(ListType::EmptyList) => match a.get_type() {
                        None => None,
                        Some(ty) => Some(ListType::new_list(ty.clone_box())),
                    },
                    _ => l.get_type(),
                },
            },
            ListExpr::Concat(l1, l2) => match l1.get_type() {
                None => l2.get_type(),
                Some(l1t) => match l2.get_type() {
                    None => l1.get_type(),
                    Some(l2t) => match l1t.downcast_ref::<ListType>() {
                        None => None,
                        Some(_) => match l2t.downcast_ref::<ListType>() {
                            None => l1.get_type(),
                            Some(ListType::EmptyList) => l1.get_type(),
                            _ => l2.get_type(),
                        },
                    },
                },
            },
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ListExpr::EmptyList => Ok(Some(ListType::new_empty())),
            ListExpr::List(e) if e.is_empty() => Ok(Some(ListType::new_empty())), // weird
            ListExpr::List(exprs) => {
                for e in exprs.iter_mut() {
                    e.resolve_type(scope)?;
                }
                let t = exprs[0].get_type();
                match t {
                    None => Ok(None),
                    Some(ty) => {
                        let not_same = exprs.iter().any(|t| match t.get_type() {
                            None => true,
                            Some(t) => &*t != &*ty,
                        });
                        if not_same {
                            bail!("list must have all elements of same type")
                        } else {
                            Ok(Some(ListType::new_list(ty)))
                        }
                    }
                }
            }
            ListExpr::Cons(atom, list) => {
                atom.resolve_type(scope)?;
                list.resolve_type(scope)?;
                match atom.get_type() {
                    None => match list.get_type() {
                        None => Ok(None),
                        Some(lt) => match lt.downcast_ref::<ListType>() {
                            None => bail!("attempt to make a cons without a list"),
                            Some(ListType::EmptyList) => Ok(None),
                            _ => Ok(list.get_type()),
                        },
                    },
                    Some(at) => match list.get_type() {
                        None => {
                            list.set_type(Some(ListType::new_list(at)));
                            scope.define(list.clone_box());
                            Ok(list.get_type())
                        }
                        Some(lt) => match lt.downcast_ref::<ListType>() {
                            None => bail!("attempt to make a cons without a list"),
                            Some(ListType::EmptyList) => {
                                Ok(Some(ListType::new_list(at.clone_box())))
                            }
                            Some(ListType::List(ty)) if *ty != at => {
                                bail!("incompatible types in cons expression")
                            }
                            Some(ListType::List(ty)) => Ok(Some(ty.clone())),
                        },
                    },
                }
            }
            ListExpr::Concat(list1, list2) => {
                list1.resolve_type(scope)?;
                list2.resolve_type(scope)?;
                match list1.get_type() {
                    None => match list2.get_type() {
                        None => Ok(None),
                        Some(lt2) => {
                            list1.set_type(Some(lt2.clone_box()));
                            Ok(list2.get_type())
                        }
                    },
                    Some(lt1) => match list2.get_type() {
                        None => Ok(None),
                        Some(lt2) => match lt1.downcast_ref::<ListType>() {
                            None => bail!("concat expression left side is not a list"),
                            Some(ListType::EmptyList) => match lt2.downcast_ref::<ListType>() {
                                None => bail!("concat expression right side is not a list"),
                                Some(ListType::EmptyList) => Ok(Some(ListType::new_empty())),
                                _ => Ok(list2.get_type()),
                            },
                            Some(ListType::List(ty1)) => match lt2.downcast_ref::<ListType>() {
                                None => bail!("concat expression right side is not a list"),
                                Some(ListType::EmptyList) => Ok(list1.get_type()),
                                Some(ListType::List(ty2)) if ty1 != ty2 => {
                                    bail!("concat expression of different list types")
                                }
                                _ => Ok(list1.get_type()),
                            },
                        },
                    },
                }
            }
        }
    }

    fn define_into(&self, scope: &mut dyn Scope) {
        match self {
            ListExpr::EmptyList => {}
            ListExpr::List(v) => {
                for e in v.iter() {
                    e.define_into(scope)
                }
            }
            ListExpr::Cons(a, l) => {
                a.define_into(scope);
                l.define_into(scope);
            }
            ListExpr::Concat(l, r) => {
                l.define_into(scope);
                r.define_into(scope)
            }
        }
    }

    fn is_seq(&self) -> bool {
        true
    }
}
