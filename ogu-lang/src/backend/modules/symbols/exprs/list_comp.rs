use crate::backend::modules::types::list_type::ListType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) struct ListComprehension {
    expr: Box<dyn Symbol>,
    guards: Vec<Box<dyn Symbol>>,
}

impl ListComprehension {
    pub(crate) fn new_list_comp(expr: Box<dyn Symbol>, guards: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(ListComprehension { expr, guards })
    }
}

impl Symbol for ListComprehension {
    fn get_name(&self) -> &str {
        "list_comprehension"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type().map(|t| ListType::new_list(t.clone()))
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("list_comprehension", Some(scope.clone_box()));
        for g in self.guards.iter_mut() {
            g.resolve_type(scope)?;
            g.define_into(&mut *sym_table);
        }
        self.expr.resolve_type(&mut *sym_table)?;
        Ok(self.get_type())
    }

    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        self.expr.define_into(scope);
        for g in self.guards.iter() {
            g.define_into(scope);
        }
        None
    }

    fn is_seq(&self) -> bool {
        true
    }
}
