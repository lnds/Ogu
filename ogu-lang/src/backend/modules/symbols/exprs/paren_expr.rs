use crate::backend::scopes::symbol::Symbol;
use crate::parser::ast::expressions::expression::Expression;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct ParenExprSym {
    expr: Box<dyn Symbol>,
}

impl ParenExprSym {

    pub(crate) fn make(expr: &Expression) -> Box<dyn Symbol> {
        match expr {
            Expression::Identifier(_) =>
                Box::new(ParenExprSym {
                    expr: expr.into()
                }),
            _ => expr.into()
        }
    }
}

impl Symbol for ParenExprSym {
    fn get_name(&self) -> &str {
        self.expr.get_name()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self.expr.get_type() {
            None => None,
            Some(t) => t.resolve_expr_type()
        }
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.expr.set_type(ty)
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let res = self.expr.resolve_type(scope);
        println!("RESOLVE TYPE PAREN for {:?} => {:?}", self.expr, res);
        res
    }
}