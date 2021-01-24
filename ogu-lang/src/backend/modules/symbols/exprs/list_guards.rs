use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::ListComprehensionGuard;
use anyhow::{bail, Result};

#[derive(Debug, Clone)]
pub(crate) enum ListGuard {
    Generator(Box<dyn Symbol>, Box<dyn Symbol>),
    // id <- expr
    TupleGenerator(Vec<Box<dyn Symbol>>, Box<dyn Symbol>),
    // (id, id..) <- expr
    Let(Box<dyn Symbol>, Box<dyn Symbol>),
    // let id = expr
    LetTuple(Vec<Box<dyn Symbol>>, Box<dyn Symbol>),
    // let id = expr
    Expr(Box<dyn Symbol>),                       // , expr
}

impl ListGuard {
    pub(crate) fn new_generator(id: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Generator(id, list))
    }

    pub(crate) fn new_tuple_generator(
        ids: Vec<Box<dyn Symbol>>,
        list: Box<dyn Symbol>,
    ) -> Box<Self> {
        Box::new(ListGuard::TupleGenerator(ids, list))
    }

    pub(crate) fn new_let(id: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Let(id, list))
    }

    pub(crate) fn new_let_tuple(ids: Vec<Box<dyn Symbol>>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::LetTuple(ids, list))
    }

    pub(crate) fn new_expr(expr: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Expr(expr))
    }
}

impl Symbol for ListGuard {
    fn get_name(&self) -> &str {
        "list_guard"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match &self {
            ListGuard::Let(id, l) | ListGuard::Generator(id, l) => match id.get_type() {
                None => match l.get_type() {
                    None => Some(ListType::new_list(TRAIT_UNKNOWN.clone_box())),
                    Some(_) => l.get_type(),
                },
                Some(_) => id.get_type(),
            },
            ListGuard::LetTuple(t, _) | ListGuard::TupleGenerator(t, _) => {
                if t.iter().any(|e| e.get_type().is_none()) {
                    None
                } else {
                    Some(TupleType::new_box(
                        t.iter().flat_map(|e| e.get_type()).collect(),
                    ))
                }
            }
            ListGuard::Expr(e) => e.get_type(),
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ListGuard::Generator(id, lst) => {
                lst.resolve_type(scope)?;
                if let Some(lt) = lst.get_type() {
                    if let Some(lt) = lt.downcast_ref::<ListType>() {
                        match id.get_type() {
                            None => id.set_type(Some(lt.ty.clone_box())),
                            Some(ty) => {
                                if ty != lt.ty.clone() {
                                    id.matches_types(Some(lt.ty.clone_box()));
                                }
                            }
                        }
                    } else {
                        bail!("generator must come from a list");
                    }
                }
                id.define_into(scope);
            }
            ListGuard::TupleGenerator(tuple, lst) => {
                lst.resolve_type(scope)?;
                if let Some(lt) = lst.get_type() {
                    match lt.downcast_ref::<ListType>() {
                        None => bail!("generator must come from a list"),
                        Some(lt) =>
                            match lt.ty.downcast_ref::<TupleType>() {
                                None => bail!("right side of expression must be a list of tuples"),
                                Some(tuple_type) => {
                                    if tuple_type.tuple.len() != tuple.len() {
                                        bail!("left side must be a tuple of len = {}", tuple.len())
                                    } else {
                                        for (p, e) in tuple.iter_mut().enumerate() {
                                            e.set_type(Some(tuple_type.tuple[p].clone()))
                                        }
                                    }
                                }
                            }
                    }
                }
                for e in tuple.iter() {
                    e.define_into(scope);
                }
            }
            ListGuard::Let(id, val) => {
                val.resolve_type(scope)?;
                if let Some(vt) = val.get_type() {
                    id.set_type(Some(vt.clone()));
                }
                id.define_into(scope);
            }
            ListGuard::LetTuple(t, val) => {
                val.resolve_type(scope)?;
                for e in t.iter_mut() {
                    e.resolve_type(scope)?;
                }
                for e in t.iter() {
                    e.define_into(scope);
                }
            }
            ListGuard::Expr(expr) => {
                expr.resolve_type(scope)?;
                expr.define_into(scope);
            }
        }
        Ok(self.get_type())
    }

    fn define_into(&self, scope: &mut dyn Scope) {
        match self {
            ListGuard::Generator(id, _) => {
                id.define_into(scope);
            }
            ListGuard::TupleGenerator(tuple, _) => {
                for e in tuple.iter() {
                    e.define_into(scope);
                }
            }
            ListGuard::Let(id, _) => {
                id.define_into(scope);
            }
            ListGuard::LetTuple(tuple, _) => {
                for e in tuple.iter() {
                    e.define_into(scope);
                }
            }
            ListGuard::Expr(expr) => {
                expr.define_into(scope);
            }
        }
    }

    fn is_seq(&self) -> bool {
        true
    }
}

impl<'a> From<&ListComprehensionGuard<'a>> for Box<dyn Symbol> {
    fn from(guard: &ListComprehensionGuard<'a>) -> Self {
        match guard {
            ListComprehensionGuard::Generator(id, e) => {
                ListGuard::new_generator(IdSym::new(id), e.into())
            }
            ListComprehensionGuard::TupleGenerator(ids, e) => {
                let ids = ids.iter().map(|id| IdSym::new_id(id)).collect();
                ListGuard::new_tuple_generator(ids, e.into())
            }
            ListComprehensionGuard::Let(id, e) => ListGuard::new_let(IdSym::new(id), e.into()),
            ListComprehensionGuard::LetTuple(ids, e) => {
                let ids = ids.iter().map(|id| IdSym::new_id(id)).collect();
                ListGuard::new_let_tuple(ids, e.into())
            }
            ListComprehensionGuard::Expr(e) => ListGuard::new_expr(e.into()),
        }
    }
}
