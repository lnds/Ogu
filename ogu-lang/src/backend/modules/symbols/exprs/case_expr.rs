use crate::backend::errors::OguError;
use crate::backend::modules::symbols::exprs::OptSymbolTuple;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct CaseExpr {
    selector: Box<dyn Symbol>,
    cases: Vec<OptSymbolTuple>,
}

impl CaseExpr {
    pub(crate) fn new(selector: Box<dyn Symbol>, cases: Vec<OptSymbolTuple>) -> Box<Self> {
        Box::new(CaseExpr { selector, cases })
    }
}

impl Symbol for CaseExpr {
    fn get_name(&self) -> &str {
        "case_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        if self.cases.is_empty() {
            None
        } else {
            self.cases[0].1.get_type()
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut expr_type = None;
        let mut cond_type = None;
        for (c, e) in self.cases.iter_mut() {
            if let Some(c) = c {
                c.resolve_type(scope);
                if cond_type.is_none() {
                        cond_type = c.get_type();
                }
            }
            e.resolve_type(scope)?;
            if expr_type.is_none() {
                expr_type = e.get_type();
            }
        }
        let storable = self.selector.storable();
        self.selector.set_storable(true);
        if self.selector.get_type().is_none() {
            self.selector.set_type(cond_type.clone());
        }
        self.selector.matches_types(cond_type);
        self.selector.resolve_type(scope)?;
        self.selector.set_storable(storable);
        scope.define(self.selector.clone());
        Ok(self.get_type())
    }
}
