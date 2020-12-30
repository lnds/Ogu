use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::basic_type::BasicType;

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
            ArithmeticSym::Add(l,r )
            |ArithmeticSym::Sub(l,r )
            |ArithmeticSym::Mul(l,r )
            |ArithmeticSym::Div(l,r )
            |ArithmeticSym::Mod(l,r )
            |ArithmeticSym::Pow(l,r ) => {
                let tl = l.get_type()?;
                let tr = r.get_type()?;
                if &tl == &tr {
                    Some(tr.clone())
                } else {
                    if (tl == BasicType::float() && tr == BasicType::int())
                        ||(tl == BasicType::int() && tr == BasicType::float()) {
                        Some(BasicType::float())
                    } else {
                        None
                    }
                }
            }
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }
}