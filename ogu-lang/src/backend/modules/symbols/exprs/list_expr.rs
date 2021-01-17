use crate::backend::scopes::types::{Type};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::types::list_type::ListType;

#[derive(Debug, Clone)]
pub(crate) enum ListExpr {
    Empty,
    List(Vec<Box<dyn Symbol>>),
    Cons(Box<dyn Symbol>, Box<dyn Symbol>),
    Concat(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl ListExpr {
    pub(crate) fn new_empty() -> Box<Self> {
        Box::new(ListExpr::Empty)
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
            ListExpr::Empty => Some(ListType::new_empty()),
            ListExpr::List(v) if v.is_empty() => Some(ListType::new_empty()),
            ListExpr::List(v) => {
                match v[0].get_type() {
                    None => None,
                    Some(t) => Some(ListType::new_list(t.clone_box()))
                }
            }
            ListExpr::Cons(_, l) => {
                match l.get_type() {
                    None => None,
                    Some(t) => Some(t)
                }
            }
            ListExpr::Concat(l1, l2) => {
                match l1.get_type() {
                    None => l2.get_type(),
                    Some(t) => Some(t)
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ListExpr::Empty => Ok(Some(ListType::new_empty())),
            ListExpr::List(e) if e.is_empty() => Ok(Some(ListType::new_empty())), // weird
            ListExpr::List(exprs) => {
                for e in exprs.iter_mut() {
                    e.resolve_type(scope)?;
                }
                let t = exprs[0].get_type();
                match t {
                    None => Ok(None),
                    Some(ty) => {
                        let not_same = exprs.iter().any(|t|
                            match t.get_type() {
                                None => true,
                                Some(t) => &*t != &*ty
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
                        Some(lt) => {
                            match lt.downcast_ref::<ListType>() {
                                None => bail!("attempt to make a cons without a list"),
                                Some(lt) => {
                                    match lt {
                                        ListType::Empty => Ok(None),
                                        ListType::List(ty) => {
                                            Ok(Some(ty.clone()))
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Some(at) => match list.get_type()  {
                        None => Ok(None),
                        Some(lt) => {
                            match lt.downcast_ref::<ListType>() {
                                None => bail!("attempt to make a cons without a list"),
                                Some(lt) => {
                                    match lt {
                                        ListType::Empty => Ok(Some(ListType::new_list(at.clone_box()))),
                                        ListType::List(ty) => {
                                            if *ty != at {
                                                bail!("incompatible types in cons expression")
                                            } else {
                                                Ok(Some(ty.clone()))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
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
                    }
                    Some(lt1) => match list2.get_type() {
                        None => Ok(None),
                        Some(lt2) => {
                            if let Some(lt1) = lt1.downcast_ref::<ListType>() {
                                if let Some(lt2) = lt2.downcast_ref::<ListType>() {
                                    match lt1 {
                                        ListType::Empty => match lt2 {
                                            ListType::Empty => Ok(Some(ListType::new_empty())),
                                            ListType::List(_) => {
                                                Ok(list2.get_type())
                                            }
                                        }
                                        ListType::List(ty1) => match lt2 {
                                            ListType::Empty => Ok(list1.get_type()),
                                            ListType::List(ty2) => {
                                                if ty1 != ty2 {
                                                    bail!("concat expression of different list types")
                                                } else {
                                                    Ok(list1.get_type())
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    bail!("concat expression right side is not a list")
                                }
                            }
                            else {
                                bail!("concat expression left side is not a list")
                            }

                        }
                    }
                }
            }
        }
    }
}