use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
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
    pub(crate) fn new_add(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::Add(l, r))
    }

    pub(crate) fn new_sub(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::Sub(l, r))
    }

    pub(crate) fn new_mul(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::Mul(l, r))
    }

    pub(crate) fn new_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::Div(l, r))
    }

    pub(crate) fn new_int_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::IntDiv(l, r))
    }

    pub(crate) fn new_mod(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticSym::Mod(l, r))
    }

    pub(crate) fn new_pow(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
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
                    Some(rt) if lt.is_trait() && &*lt != TRAIT_NUM => Some(rt.clone()),
                    Some(rt) if rt.is_trait() && &*rt != TRAIT_NUM => Some(lt.clone()),
                    Some(rt) => {
                        if lt == rt.clone() {
                            Some(rt.clone())
                        } else if lt == BasicType::int() && rt == BasicType::uint()
                            || lt == BasicType::uint() && rt == BasicType::int()
                        {
                            Some(BasicType::uint())
                        } else if (lt == BasicType::float() && rt == BasicType::int())
                            || (lt == BasicType::float() && rt == BasicType::uint())
                            || (lt == BasicType::int() && rt == BasicType::float())
                            || (lt == BasicType::uint() && rt == BasicType::float())
                        {
                            Some(BasicType::float())
                        } else {
                            Some(TRAIT_NUM.clone_box())
                        }
                    }
                },
            },
            ArithmeticSym::IntDiv(l, r) => match l.get_type() {
                None => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*rt == TRAIT_NUM {
                            Some(TRAIT_NUM.clone_box())
                        } else {
                            Some(BasicType::int())
                        }
                    }
                },
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == TRAIT_NUM && &*rt == TRAIT_NUM {
                            Some(TRAIT_NUM.clone_box())
                        } else {
                            Some(BasicType::int())
                        }
                    }
                },
            },
            ArithmeticSym::Div(l, r) => match l.get_type() {
                None => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*rt == TRAIT_NUM {
                            Some(TRAIT_NUM.clone_box())
                        } else {
                            Some(BasicType::float())
                        }
                    }
                },
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == TRAIT_NUM && &*rt == TRAIT_NUM {
                            Some(TRAIT_NUM.clone_box())
                        } else {
                            Some(BasicType::float())
                        }
                    }
                },
            },
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ArithmeticSym::Add(l, r)
            | ArithmeticSym::Sub(l, r)
            | ArithmeticSym::Mul(l, r)
            | ArithmeticSym::IntDiv(l, r)
            | ArithmeticSym::Div(l, r)
            | ArithmeticSym::Mod(l, r)
            | ArithmeticSym::Pow(l, r) => {
                resolve_comparable(l, r, scope, TRAIT_NUM)?;
                Ok(self.get_type())
            }
        }
    }
}
