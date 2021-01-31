use anyhow::Result;

use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::Expression;

#[derive(Clone, Debug)]
pub(crate) struct ValueSym {
    pub(crate) name: Box<dyn Symbol>,
    pub(crate) expr: Box<dyn Symbol>,
}

impl ValueSym {
    pub(crate) fn new(name: &Expression, expr: &Expression) -> Box<Self> {
        let mut name: Box<dyn Symbol> = name.into();
        name.set_storable(true);
        Box::new(ValueSym {
            name: name.clone(),
            expr: expr.into(),
        })
    }

    pub(crate) fn make(name: Box<dyn Symbol>, expr: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(ValueSym { name, expr })
    }
}

impl Symbol for ValueSym {
    fn get_name(&self) -> &str {
        self.name.get_name()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, t: Option<Box<dyn Type>>) {
        match self.name.get_type() {
            None => self.name.set_type(t.clone()),
            Some(tr) if tr.is_trait() => self.name.set_type(t.clone()),
            _ => {}
        }

        match self.expr.get_type() {
            None => self.expr.set_type(t.clone()),
            Some(tr) if tr.is_trait() => self.expr.set_type(t.clone()),
            _ => {}
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.expr.resolve_type(scope)?;
        if let Some(curry) = self.expr.get_curry() {
            self.expr = curry;
        }
        self.name.resolve_type(scope)?;
        self.name.set_type(self.expr.get_type());

        Ok(self.get_type())
    }

    fn storable(&self) -> bool {
        true
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        if self.expr.is_seq() {
            self.expr.define_into(scope);
        } else {
            scope.define(self.expr.clone());
        }
        if self.name.is_seq() {
            self.name.define_into(scope)
        } else {
            scope.define(self.name.clone())
        }
    }
}
