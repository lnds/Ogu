use crate::backend::modules::symbols::func::swap_args;
use crate::backend::modules::symbols::idents::IdSym;
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
    ty: Option<Box<FuncType>>,
}

impl LambdaExpr {
    pub(crate) fn new(args: Vec<Box<dyn Symbol>>, expr: Box<dyn Symbol>) -> Box<Self> {
        let ty = FuncType::make(&Some(args.clone()), &*expr);
        Box::new(LambdaExpr { args, expr, ty })
    }

    pub(crate) fn replace_args(
        &mut self,
        args: Vec<Box<dyn Symbol>>,
        scope: &mut dyn Scope,
    ) -> Result<()> {
        let msg = "lambda expression";
        self.args = swap_args(msg,&self.args, &args)?;
        self.resolve_type(scope)?;
        Ok(())
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
                        if !st.is_compatible_with(&*at) {
                            bail!(
                                "incompatible  type for argument {}, st = {:?} at = {:?} in lambda expression",
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
        match &self.ty {
            None => None,
            Some(ty) => {
                let tb: Box<dyn Type> = ty.clone();
                Some(tb)
            }
        }
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        if self.ty.is_none() {
            if let Some(ty) = ty {
                if let Some(ty) = ty.downcast_ref::<FuncType>() {
                    self.ty = Some(Box::new(ty.clone()))
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new("lambda_expr", Some(scope.clone_box()));
        sym_table.set_function_name(&scope.function_scope_name());
        for a in self.args.iter() {
            a.define_into(&mut *sym_table);
        }
        self.expr.resolve_type(&mut *sym_table)?;
        let curry = self.expr.is_curry();
        if let Some(curry) = self.expr.get_curry() {
            self.expr = curry;
            self.set_type(self.expr.get_type())
        }

        for s in sym_table.get_symbols() {
            self.define_arg(s.clone_box())?;
        }
        if !curry {
            let ty = FuncType::make(&Some(self.args.clone()), &*self.expr);
            if let Some(ft1) = self.ty.clone() {
                if let Some(ft2) = ty.clone() {
                    if !ft1.result.is_compatible_with(&*ft2.result) {
                        bail!(
                        "incompatible type for return type in lambda expression {:#?}\n\n ft1 = {:?}\n\n ft2 = {:?}\n\n",
                        self,
                        ft1,
                        ft2
                    )
                    }
                }
            }
            self.ty = ty;
        }
        Ok(self.get_type())
    }
}
