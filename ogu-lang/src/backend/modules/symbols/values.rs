use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::Expression;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct ValueSym {
    name: Box<dyn Symbol>,
    expr: Box<dyn Symbol>,
}

impl ValueSym {
    pub(crate) fn new(name: &Expression, expr: &Expression) -> Box<Self> {
        Box::new(ValueSym {
            name: name.into(),
            expr: expr.into(),
        })
    }
}

impl Symbol for ValueSym {
    fn get_name(&self) -> &str {
        &self.name.get_name()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.name.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.expr.resolve_type(scope)?;
        self.name.resolve_type(scope)?;
        self.set_type(self.expr.get_type());
        Ok(self.get_type())
    }
}
