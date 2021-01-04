use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum ArithmeticSym {
    Add(Box<dyn Symbol>, Box<dyn Symbol>),
    Sub(Box<dyn Symbol>, Box<dyn Symbol>),
    Mul(Box<dyn Symbol>, Box<dyn Symbol>),
    Div(Box<dyn Symbol>, Box<dyn Symbol>),
    IntDiv(Box<dyn Symbol>, Box<dyn Symbol>),
    Mod(Box<dyn Symbol>, Box<dyn Symbol>),
    Pow(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl ArithmeticSym {
    pub(crate) fn new_add(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Add(l, r))
    }

    pub(crate) fn new_sub(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Sub(l, r))
    }
    pub(crate) fn new_mul(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Mul(l, r))
    }

    pub(crate) fn new_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Div(l, r))
    }

    pub(crate) fn new_int_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::IntDiv(l, r))
    }

    pub(crate) fn new_mod(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Mod(l, r))
    }

    pub(crate) fn new_pow(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<ArithmeticSym> {
        Box::new(ArithmeticSym::Pow(l, r))
    }
}

impl Symbol for ArithmeticSym {
    fn get_name(&self) -> &str {
        "arithmetic operation"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ArithmeticSym::Add(l, r)
            | ArithmeticSym::Sub(l, r)
            | ArithmeticSym::Mul(l, r)
            | ArithmeticSym::Mod(l, r)
            | ArithmeticSym::Pow(l, r) => match l.get_type() {
                None => match r.get_type() {
                    None => Some(TRAIT_NUM.clone_box()),
                    Some(rt) => Some(rt.clone()),
                },
                Some(lt) => match r.get_type() {
                    None => Some(lt.clone()),
                    Some(rt) if lt.is_trait() => Some(rt.clone()),
                    Some(rt) if rt.is_trait() => Some(lt.clone()),
                    Some(rt) => {
                        if lt == rt.clone() {
                            Some(rt.clone())
                        } else if (lt == BasicType::float() && rt == BasicType::int())
                            || (lt == BasicType::int() && rt == BasicType::float())
                        {
                            Some(BasicType::float())
                        } else {
                            Some(TRAIT_NUM.clone_box())
                        }
                    }
                },
            },
            ArithmeticSym::IntDiv(l, r) => {
                if l.get_type().is_none() && r.get_type().is_none() {
                    None
                } else {
                    Some(BasicType::int())
                }
            }
            ArithmeticSym::Div(l, r) => {
                if l.get_type().is_none() && r.get_type().is_none() {
                    None
                } else {
                    Some(BasicType::float())
                }
            }
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {}

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ArithmeticSym::Add(l, r)
            | ArithmeticSym::Sub(l, r)
            | ArithmeticSym::Mul(l, r)
            | ArithmeticSym::IntDiv(l, r)
            | ArithmeticSym::Div(l, r)
            | ArithmeticSym::Mod(l, r)
            | ArithmeticSym::Pow(l, r) => {
                match l.resolve_type(scope)? {
                    None => match r.resolve_type(scope)? {
                        None => {
                            r.set_type(Some(TRAIT_NUM.clone_box()));
                            l.set_type(Some(TRAIT_NUM.clone_box()));
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
                }
                Ok(self.get_type())
            }
        }
    }

    fn storable(&self) -> bool {
        false
    }

    fn set_storable(&mut self, _s: bool) {

    }
}
