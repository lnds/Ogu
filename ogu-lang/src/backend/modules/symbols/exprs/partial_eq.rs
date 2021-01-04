use crate::backend::modules::types::trait_type::TRAIT_EQ;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum PartialEqSym {
    Eq(Box<dyn Symbol>, Box<dyn Symbol>),
    Ne(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl PartialEqSym {
    pub(crate) fn new_eq(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialEqSym::Eq(l, r))
    }

    pub(crate) fn new_ne(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialEqSym::Ne(l, r))
    }
}

impl Symbol for PartialEqSym {
    fn get_name(&self) -> &str {
        "partial_eq"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            PartialEqSym::Eq(l, r) | PartialEqSym::Ne(l, r) => match l.get_type() {
                None => match r.get_type() {
                    None => Some(TRAIT_EQ.clone_box()),
                    Some(rt) => Some(rt.clone()),
                },
                Some(lt) => match r.get_type() {
                    None => Some(lt.clone()),
                    Some(rt) if lt.is_trait() => Some(rt.clone()),
                    Some(rt) if rt.is_trait() => Some(lt.clone()),
                    Some(rt) => {
                        if lt == rt.clone() {
                            Some(rt.clone())
                        } else {
                            println!("WTF l = {:?} => {:?} r = {:?} => {:?} ", l, lt, r, rt);
                            None
                        }
                    }
                },
            },
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            PartialEqSym::Eq(l, r) | PartialEqSym::Ne(l, r) => {
                match l.resolve_type(scope)? {
                    None => match r.resolve_type(scope)? {
                        None => {
                            r.set_type(Some(TRAIT_EQ.clone_box()));
                            l.set_type(Some(TRAIT_EQ.clone_box()));
                            scope.define(l.clone());
                            scope.define(r.clone());
                        }
                        Some(rt) => {
                            l.set_type(Some(rt.clone()));
                            scope.define(l.clone());
                        }
                    },
                    Some(lt) => match r.resolve_type(scope)? {
                        None => {
                            r.set_type(Some(lt.clone()));
                            scope.define(r.clone());
                        }
                        Some(rt) if lt.is_trait() && !rt.is_trait() => {
                            l.set_type(Some(rt.clone()));
                            scope.define(l.clone());
                        }
                        Some(rt) if rt.is_trait() && !lt.is_trait() => {
                            r.set_type(Some(lt.clone()));
                            scope.define(r.clone());
                        }
                        _ => {}
                    },
                };
                Ok(self.get_type())
            }
        }
    }

}
