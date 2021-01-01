use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Result, Error};
use crate::backend::errors::OguError;

#[derive(Clone, Debug)]
pub(crate) enum ArithmeticSym {
    Add(Box<dyn Symbol>, Box<dyn Symbol>),
    Sub(Box<dyn Symbol>, Box<dyn Symbol>),
    Mul(Box<dyn Symbol>, Box<dyn Symbol>),
    Div(Box<dyn Symbol>, Box<dyn Symbol>),
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
            | ArithmeticSym::Div(l, r)
            | ArithmeticSym::Mod(l, r)
            | ArithmeticSym::Pow(l, r) => {
                let tl = l.get_type()?;
                let tr = r.get_type()?;
                if tl == tr.clone() {
                    Some(tr.clone())
                }
                else if (tl == BasicType::float() && tr == BasicType::int())
                    || (tl == BasicType::int() && tr == BasicType::float())
                {
                    Some(BasicType::float())
                } else {
                    None
                }
            }
        }
    }


    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self.get_type() {
            Some(ty) => Ok(Some(ty.clone())),
            None => match self {
                ArithmeticSym::Add(l, r)
                | ArithmeticSym::Sub(l, r)
                | ArithmeticSym::Mul(l, r)
                | ArithmeticSym::Div(l, r)
                | ArithmeticSym::Mod(l, r)
                | ArithmeticSym::Pow(l, r) => {
                    l.resolve_type(scope)?;
                    r.resolve_type(scope)?;
                    match self.get_type() {
                        None => Err(Error::new(OguError::SymbolTableError).context(format!("could not resolve {:?}", self))),
                        Some(t) => Ok(Some(t))
                    }
                }
            },
        }
    }
}
