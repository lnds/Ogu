use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
use crate::backend::modules::types::basic_type::{FLOAT_TYPE, INT_TYPE};
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct ArithmeticExpr {
    op: Op,
    ty: Option<Box<dyn Type>>,
}

#[derive(Clone, Debug)]
enum Op {
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
        Box::new(ArithmeticExpr {
            op: Op::Add(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_sub(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::Sub(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_mul(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::Mul(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::Div(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_int_div(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::IntDiv(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_mod(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::Mod(l, r),
            ty: None,
        })
    }

    pub(crate) fn new_pow(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ArithmeticExpr {
            op: Op::Pow(l, r),
            ty: None,
        })
    }
}

impl Symbol for ArithmeticExpr {
    fn get_name(&self) -> &str {
        "arithmetic operation"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone();
        if self.ty.is_some() {
            match &mut self.op {
                Op::Add(l, r)
                | Op::Sub(l, r)
                | Op::Mul(l, r)
                | Op::IntDiv(l, r)
                | Op::Div(l, r)
                | Op::Mod(l, r)
                | Op::Pow(l, r) => {
                    l.set_type(ty.clone());
                    r.set_type(ty.clone());
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let r = match &mut self.op {
            Op::Div(l, r) => {
                self.ty = resolve_comparable(l, r, scope, TRAIT_NUM)?;
                if l.get_type() == Some(INT_TYPE.clone_box())
                    || r.get_type() == Some(INT_TYPE.clone_box())
                {
                    self.ty = Some(FLOAT_TYPE.clone_box());
                }
                Ok(self.get_type())
            }
            Op::IntDiv(l, r) => {
                self.ty = resolve_comparable(l, r, scope, TRAIT_NUM)?;
                if l.get_type() == Some(FLOAT_TYPE.clone_box())
                    || r.get_type() == Some(FLOAT_TYPE.clone_box())
                {
                    self.ty = Some(INT_TYPE.clone_box());
                }
                Ok(self.get_type())
            }
            Op::Add(l, r) | Op::Sub(l, r) | Op::Mul(l, r) | Op::Mod(l, r) | Op::Pow(l, r) => {
                self.ty = resolve_comparable(l, r, scope, TRAIT_NUM)?;
                Ok(self.get_type())
            }
        };
        r
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        match &self.op {
            Op::Add(l, r)
            | Op::Sub(l, r)
            | Op::Mul(l, r)
            | Op::IntDiv(l, r)
            | Op::Div(l, r)
            | Op::Mod(l, r)
            | Op::Pow(l, r) => {
                l.define_into(scope);
                r.define_into(scope);
            }
        }
        None
    }
}
