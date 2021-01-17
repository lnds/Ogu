use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum ArithmeticExpr {
    Add(Box<dyn Symbol>, Box<dyn Symbol>),
    Sub(Box<dyn Symbol>, Box<dyn Symbol>),
    Mul(Box<dyn Symbol>, Box<dyn Symbol>),
    Div(Box<dyn Symbol>, Box<dyn Symbol>),
    IntDiv(Box<dyn Symbol>, Box<dyn Symbol>),
    Mod(Box<dyn Symbol>, Box<dyn Symbol>),
    Pow(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl ArithmeticExpr {
    pub(crate) fn new_add(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Add(l, r))
    }

    pub(crate) fn new_sub(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Sub(l, r))
    }

    pub(crate) fn new_mul(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Mul(l, r))
    }

    pub(crate) fn new_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Div(l, r))
    }

    pub(crate) fn new_int_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::IntDiv(l, r))
    }

    pub(crate) fn new_mod(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Mod(l, r))
    }

    pub(crate) fn new_pow(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr::Pow(l, r))
    }
}

impl Symbol for ArithmeticExpr {
    fn get_name(&self) -> &str {
        "arithmetic operation"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ArithmeticExpr::Add(l, r)
            | ArithmeticExpr::Sub(l, r)
            | ArithmeticExpr::Mul(l, r)
            | ArithmeticExpr::Mod(l, r)
            | ArithmeticExpr::Pow(l, r) => match l.get_type() {
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
            ArithmeticExpr::IntDiv(l, r) => match l.get_type() {
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
            ArithmeticExpr::Div(l, r) => match l.get_type() {
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
            ArithmeticExpr::Add(l, r)
            | ArithmeticExpr::Sub(l, r)
            | ArithmeticExpr::Mul(l, r)
            | ArithmeticExpr::IntDiv(l, r)
            | ArithmeticExpr::Div(l, r)
            | ArithmeticExpr::Mod(l, r)
            | ArithmeticExpr::Pow(l, r) => {
                resolve_comparable(l, r, scope, TRAIT_NUM)?;
                Ok(self.get_type())
            }
        }
    }

    fn define_into(&self, scope: &mut dyn Scope) {
        match self {
            ArithmeticExpr::Add(l, r)
            | ArithmeticExpr::Sub(l, r)
            | ArithmeticExpr::Mul(l, r)
            | ArithmeticExpr::IntDiv(l, r)
            | ArithmeticExpr::Div(l, r)
            | ArithmeticExpr::Mod(l, r)
            | ArithmeticExpr::Pow(l, r) => {
                scope.define(l.clone());
                scope.define(r.clone());
            }
        }
    }
}
