use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::trait_type::TraitType;
use crate::backend::scopes::Scope;
use anyhow::{Result, Error};
use crate::backend::errors::OguError;

#[derive(Clone, Debug)]
pub(crate) enum PartialOrdSym {
    Gt(Box<dyn Symbol>, Box<dyn Symbol>),
    Ge(Box<dyn Symbol>, Box<dyn Symbol>),
    Lt(Box<dyn Symbol>, Box<dyn Symbol>),
    Le(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl PartialOrdSym {

    pub(crate) fn new_gt(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialOrdSym::Gt(l, r))
    }

    pub(crate) fn new_ge(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialOrdSym::Ge(l, r))
    }

    pub(crate) fn new_lt(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialOrdSym::Lt(l, r))
    }

    pub(crate) fn new_le(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialOrdSym::Le(l, r))
    }

}

impl Symbol for PartialOrdSym {
    fn get_name(&self) -> &str {
        "partial_ord"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            PartialOrdSym::Gt(l, r)
            |  PartialOrdSym::Ge(l, r)
            |  PartialOrdSym::Lt(l, r)
            |  PartialOrdSym::Le(l, r) => {
                match l.get_type() {
                    None => match r.get_type() {
                        None => Some(TraitType::new("PartialOrd")),
                        Some(rt) => Some(rt.clone())
                    }
                    Some(lt) =>
                        match r.get_type() {
                            None => Some(lt.clone()),
                            Some(rt) => {
                                if lt != rt.clone() {
                                    None
                                } else {
                                    Some(rt.clone())
                                }
                            }

                        }
                }

            }
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            PartialOrdSym::Gt(l, r)
            |  PartialOrdSym::Ge(l, r)
            |  PartialOrdSym::Lt(l, r)
            |  PartialOrdSym::Le(l, r) => {
                match l.resolve_type(scope)? {
                    None => match r.resolve_type(scope)? {
                        None => {
                            r.set_type(Some(TraitType::new("PartialOrd")));
                            l.set_type(Some(TraitType::new("PartialOrd")));
                            scope.define(l.clone());
                            scope.define(r.clone());
                        },
                        Some(rt) => {
                            l.set_type(Some(rt.clone()));
                            scope.define(l.clone());
                        }
                    }
                    Some(lt) =>
                        if r.resolve_type(scope)?.is_none() {
                            r.set_type(Some(lt.clone()));
                            scope.define(r.clone());
                        }
                };
                Ok(self.get_type())
            }
        }
    }
}