use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::Expression;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct ValueSym {
    name: String,
    expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl ValueSym {
    pub(crate) fn new(name: &str, expr: &Expression) -> Box<Self> {
        let expr: Box<dyn Symbol> = expr.into();
        Box::new(ValueSym {
            name: name.to_string(),
            expr: expr.clone(),
            ty: expr.get_type(),
        })
    }
}

impl Symbol for ValueSym {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match &self.ty {
            Some(ty) if !ty.is_trait() => Ok(Some(ty.clone())),
            _ => {
                self.expr.resolve_type(scope)?;
                self.set_type(self.expr.get_type());
                Ok(self.get_type())
            }
        }
    }
}
