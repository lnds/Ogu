use anyhow::Result;

use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_ORD;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) enum PartialOrdExpr {
    Gt(Box<dyn Symbol>, Box<dyn Symbol>),
    Ge(Box<dyn Symbol>, Box<dyn Symbol>),
    Lt(Box<dyn Symbol>, Box<dyn Symbol>),
    Le(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl PartialOrdExpr {
    pub(crate) fn new_gt(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(PartialOrdExpr::Gt(l, r))
    }

    pub(crate) fn new_ge(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(PartialOrdExpr::Ge(l, r))
    }

    pub(crate) fn new_lt(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(PartialOrdExpr::Lt(l, r))
    }

    pub(crate) fn new_le(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(PartialOrdExpr::Le(l, r))
    }
}

impl Symbol for PartialOrdExpr {
    fn get_name(&self) -> &str {
        "partial_ord"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            PartialOrdExpr::Gt(l, r)
            | PartialOrdExpr::Ge(l, r)
            | PartialOrdExpr::Lt(l, r)
            | PartialOrdExpr::Le(l, r) => match l.get_type() {
                None => None,
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == &*rt.clone()
                            || lt.is_compatible_with(&*rt)
                            || rt.is_compatible_with(&*lt)
                        {
                            Some(BasicType::bool())
                        } else {
                            None
                        }
                    }
                },
            },
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            PartialOrdExpr::Gt(l, r)
            | PartialOrdExpr::Ge(l, r)
            | PartialOrdExpr::Lt(l, r)
            | PartialOrdExpr::Le(l, r) => {
                resolve_comparable(l, r, scope, TRAIT_ORD)?;
                Ok(self.get_type())
            }
        }
    }
}
