use crate::backend::modules::types::trait_type::{TRAIT_EQ};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;

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
                None => None,
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == &*rt || lt.promotes(&*rt) || rt.promotes(&*lt){
                            Some(BasicType::bool())
                        } else  {
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
                resolve_comparable(l, r, scope, TRAIT_EQ)?;
                Ok(self.get_type())
            }
        }
    }

}
