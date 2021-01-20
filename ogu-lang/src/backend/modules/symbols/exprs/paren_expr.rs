use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::expression::Expression;
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct ParenExpr {
    expr: Box<dyn Symbol>,
}

impl ParenExpr {
    pub(crate) fn make(expr: &Expression) -> Box<dyn Symbol> {
        match expr {
            Expression::Name(_) => Box::new(ParenExpr { expr: expr.into() }),
            _ => expr.into(),
        }
    }
}

impl Symbol for ParenExpr {
    fn get_name(&self) -> &str {
        self.expr.get_name()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self.expr.get_type() {
            None => None,
            Some(t) => t.resolve_expr_type(),
        }
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        self.expr.resolve_type(scope)
    }

    fn storable(&self) -> bool {
        self.expr.storable()
    }

    fn set_storable(&mut self, s: bool) {
        self.expr.set_storable(s);
    }
}
