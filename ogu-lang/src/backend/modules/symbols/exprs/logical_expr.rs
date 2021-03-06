use anyhow::Result;

use crate::backend::modules::symbols::exprs::comparable_trait::resolve_comparable;
use crate::backend::modules::types::basic_type::{BasicType, BOOL_TYPE};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

#[derive(Clone, Debug)]
pub(crate) enum LogicalExpr {
    Not(Box<dyn Symbol>),
    And(Box<dyn Symbol>, Box<dyn Symbol>),
    Or(Box<dyn Symbol>, Box<dyn Symbol>),
}

impl LogicalExpr {
    pub(crate) fn new_not(expr: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(LogicalExpr::Not(expr))
    }

    pub(crate) fn new_and(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(LogicalExpr::And(l, r))
    }

    pub(crate) fn new_or(l: Box<dyn Symbol>, r: Box<dyn Symbol>) -> Box<dyn Symbol> {
        Box::new(LogicalExpr::Or(l, r))
    }
}

impl Symbol for LogicalExpr {
    fn get_name(&self) -> &str {
        "logical_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            LogicalExpr::Not(expr) => match expr.get_type() {
                Some(et) if &*et == BOOL_TYPE => Some(BasicType::bool()),
                _ => None,
            },
            LogicalExpr::And(l, r) | LogicalExpr::Or(l, r) => match l.get_type() {
                None => None,
                Some(lt) => match r.get_type() {
                    Some(rt) if &*lt == BOOL_TYPE || &*rt == BOOL_TYPE => Some(BasicType::bool()),
                    _ => None,
                },
            },
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            LogicalExpr::Not(expr) => {
                match expr.resolve_type(scope)? {
                    None => {
                        expr.set_type(Some(BasicType::bool()));
                        scope.define(expr.clone());
                    }
                    Some(_) => {
                        // nothing
                    }
                }
            }
            LogicalExpr::And(l, r) | LogicalExpr::Or(l, r) => {
                resolve_comparable(l, r, scope, BOOL_TYPE)?;
            }
        }
        Ok(self.get_type())
    }
}
