use crate::backend::modules::symbols::exprs::OptSymbolTuple;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct CaseExpr {
    selector: Box<dyn Symbol>,
    cases: Vec<OptSymbolTuple>,
    ty: Option<Box<dyn Type>>,
}

impl CaseExpr {
    pub(crate) fn new(selector: Box<dyn Symbol>, cases: Vec<OptSymbolTuple>) -> Box<Self> {
        Box::new(CaseExpr {
            selector,
            cases,
            ty: None,
        })
    }
}

impl Symbol for CaseExpr {
    fn get_name(&self) -> &str {
        "case_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut expr_type = None;
        let mut cond_type = None;

        if let Some(sym) = scope.resolve(self.selector.get_name()) {
            cond_type = sym.get_type();
            if sym.get_type().is_some() {
                self.selector.set_type(sym.get_type());
            }
        } else if self.selector.is_seq() {
            let syms = scope.resolve_seq(self.selector.get_seq());
            self.selector.set_seq(syms);
            cond_type = self.selector.get_type();
        }
        for (c, e) in self.cases.iter_mut() {
            let mut sym_table = SymbolTable::new("cond", Some(scope.clone_box()));
            self.selector.define_into(&mut *sym_table);
            sym_table.set_function_name(&scope.function_scope_name());
            if let Some(c) = c {
                if let Some(c) = c.downcast_ref::<IdSym>() {
                    c.define_into(&mut *sym_table);
                } else if c.is_seq() {
                    let mut t = c.clone();
                    t.matches_types(cond_type.clone());
                    t.define_into(&mut *sym_table);
                }
            }

            let r = e.resolve_type(&mut *sym_table);
            if r.is_err() {
                e.resolve_type(scope)?;
            }
            match &mut expr_type {
                None => {
                    expr_type = e.get_type();
                    e.define_into(&mut *sym_table);
                }
                Some(t) => {
                    if let Some(et) = e.get_type() {
                        if t.is_trait() && !et.is_trait() {
                            expr_type = Some(et.clone_box());
                        } else {
                            t.match_types(&*et);
                            expr_type = Some(t.clone());
                        }
                    }
                }
            };
            if let Some(c) = c {
                let r = c.resolve_type(&mut *sym_table);
                if r.is_err() {
                    c.resolve_type(scope)?;
                }
                if cond_type.is_none() {
                    cond_type = c.get_type();
                } else {
                    let mut t = cond_type.clone().unwrap();
                    if let Some(ct) = c.get_type() {
                        t.match_types(&*ct);
                        cond_type = Some(t);
                        c.matches_types(cond_type.clone());
                    }
                }
            }
        }
        self.ty = expr_type;
        let storable = self.selector.storable();
        self.selector.set_storable(true); // because of tuples
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
