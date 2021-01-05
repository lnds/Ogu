use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_EQ;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) enum PartialEqExpr {
    Eq(Box<dyn Symbol>, Box<dyn Symbol>),
    Ne(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl PartialEqExpr {
    pub(crate) fn new_eq(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialEqExpr::Eq(l, r))
    }

    pub(crate) fn new_ne(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<Self> {
        Box::new(PartialEqExpr::Ne(l, r))
    }
}

impl Symbol for PartialEqExpr {
    fn get_name(&self) -> &str {
        "partial_eq"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            PartialEqExpr::Eq(l, r) | PartialEqExpr::Ne(l, r) => match l.get_type() {
                None => None,
                Some(lt) => match r.get_type() {
                    None => None,
                    Some(rt) => {
                        if &*lt == &*rt || lt.promotes(&*rt) || rt.promotes(&*lt) {
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
            PartialEqExpr::Eq(l, r) | PartialEqExpr::Ne(l, r) => {
                resolve_comparable(l, r, scope, TRAIT_EQ)?;
                Ok(self.get_type())
            }
        }
    }
}
