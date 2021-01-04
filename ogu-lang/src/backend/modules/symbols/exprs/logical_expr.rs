use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::{TRAIT_ORD, TRAIT_EQ};

#[derive(Clone, Debug)]
pub(crate) enum LogicalSym {
    Not(Box<dyn Symbol>),
    And(Box<dyn Symbol>, Box<dyn Symbol>),
    Or(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl LogicalSym {

    pub(crate) fn new_not(expr: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LogicalSym::Not(expr))
    }

    pub(crate) fn new_and(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LogicalSym::And(l, r))
    }

    pub(crate) fn new_or(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LogicalSym::Or(l, r))
    }

}

impl Symbol for LogicalSym {
    fn get_name(&self) -> &str {
        "logical_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            LogicalSym::Not(expr) => {
                match expr.get_type() {
                    None => Some(BasicType::bool()),
                    Some(et) if &*et == TRAIT_ORD || &*et == TRAIT_EQ => Some(BasicType::bool()),
                    _ => None
                }
            }
            LogicalSym::And(l, r)
            | LogicalSym::Or(l, r) => {
                match l.get_type() {
                    None => match r.get_type() {
                        None => Some(BasicType::bool()),
                        Some(rt) if &*rt == TRAIT_ORD || &*rt == TRAIT_EQ => Some(BasicType::bool()),
                        _ => None
                    }
                    Some(lt) => match r.get_type() {
                        None if &*lt == TRAIT_ORD || &*lt == TRAIT_EQ => Some(BasicType::bool()),
                        None => None,
                        Some(rt) if &*rt == TRAIT_ORD || &*rt == TRAIT_EQ => Some(BasicType::bool()),
                        _ => None
                    }
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        Ok(self.get_type())
    }

}