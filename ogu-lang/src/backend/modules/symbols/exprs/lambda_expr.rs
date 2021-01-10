use crate::backend::modules::types::func_type::FuncType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};

#[derive(Debug, Clone)]
pub(crate) struct LambdaExpr {
    args: Vec<Box<dyn Symbol>>,
    expr: Box<dyn Symbol>,
}

impl LambdaExpr {
    pub(crate) fn new(args: Vec<Box<dyn Symbol>>, expr: Box<dyn Symbol>) -> Box<Self> {
        Box::new(LambdaExpr { args, expr })
    }

    fn define_arg(&mut self, sym: Box<dyn Symbol>) -> Result<Option<Box<dyn Symbol>>> {
        match self
            .args
            .iter_mut()
            .find(|a| a.get_name() == sym.get_name())
        {
            None => Ok(None),
            Some(a) => match a.get_type() {
                None => {
                    *a = sym.clone_box();
                    Ok(Some(a.clone_box()))
                }
                Some(at) => match sym.get_type() {
                    None => Ok(None),
                    Some(st) => {
                        if !st.promotes(&*at) {
                            bail!(
                                "incompatible  type for argument {}, st = {:?} at = {:?}",
                                a.get_name(),
                                st,
                                at
                            )
                        } else {
                            Ok(Some(a.clone_box()))
                        }
                    }
                },
            },
        }
    }
}

impl Symbol for LambdaExpr {
    fn get_name(&self) -> &str {
        "lambda_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        let ty = FuncType::make(&Some(self.args.clone()), &*self.expr);
        match ty {
            None => None,
            Some(ty) => {
                let tb: Box<dyn Type> = ty;
                Some(tb)
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("lambda_expr", Some(scope.clone_box()));
        for a in self.args.iter() {
            sym_table.define(a.clone());
        }
        self.expr.resolve_type(&mut *sym_table)?;

        for s in sym_table.get_symbols() {
            self.define_arg(s.clone_box())?;
        }
        Ok(self.get_type())
    }
}
