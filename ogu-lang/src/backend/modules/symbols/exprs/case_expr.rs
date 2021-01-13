use crate::backend::modules::symbols::exprs::tuple_expr::TupleExpr;
use crate::backend::modules::symbols::exprs::OptSymbolTuple;
use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::{Symbol, SymbolClone};
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;

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
        println!("selector = {:?}", self.selector.get_name());
        println!("selector = {:?}", self.selector);

        for (c, e) in self.cases.iter_mut() {
            let mut sym_table = SymbolTable::new("cond", Some(scope.clone_box()));
            sym_table.set_function_name(&scope.function_scope_name());
            if let Some(c) = c {
                if let Some(c) = c.downcast_ref::<IdSym>() {
                    sym_table.define(c.clone_box());
                } else if let Some(t) = c.downcast_ref::<TupleExpr>() {
                    for s in t.tuple.iter() {
                        sym_table.define(s.clone());
                    }
                }
            }
            let r = e.resolve_type(scope);
            if r.is_err() {
                e.resolve_type(&mut *sym_table)?;
            }
            if expr_type.is_none() {
                expr_type = e.get_type();
            }

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
                    }
                }
            }

            /*
            println!("SYM TABLE FOR \nc= {:?}\ne= {:?}\n{:#?}", c, e, sym_table);
            if let Some(cc) = c {

                if let Some(cc) = c.downcast_ref::<IdSym>() {
                    println!("sc = {:?}", sym_table.resolve(cc.get_name()));
                }
                else if let Some(t) = cc.downcast_ref::<TupleExpr>() {
                    let mut t = t.clone();
                    println!("T BEFORE: {:#?}", t);
                    for s in t.tuple.iter_mut() {
                        if let Some(sym) = sym_table.resolve(s.get_name()) {
                            if s.get_type() != sym.get_type() {
                                s.set_type(sym.get_type())
                            }
                        }
                    }
                    println!("T = {:#?}", t);
                }


            }

             */
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
        println!("SELECTOR TYPE FINAL = {:#?}", self.selector.get_type());
        println!("SELF TYPE = {:?}", self.get_type());
        Ok(self.get_type())
    }
}
