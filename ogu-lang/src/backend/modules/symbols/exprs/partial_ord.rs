use crate::backend::modules::types::trait_type::TRAIT_ORD;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use anyhow::Result;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;

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
            | PartialOrdSym::Ge(l, r)
            | PartialOrdSym::Lt(l, r)
            | PartialOrdSym::Le(l, r) => match l.get_type() {
                None => None,
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == &*rt.clone() || lt.promotes(&*rt) || rt.promotes(&*lt) {
                            Some(BasicType::bool())
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
            PartialOrdSym::Gt(l, r)
            | PartialOrdSym::Ge(l, r)
            | PartialOrdSym::Lt(l, r)
            | PartialOrdSym::Le(l, r) => {
                resolve_comparable(l, r, scope, TRAIT_ORD)?;
                Ok(self.get_type())
            }
        }
    }

}

