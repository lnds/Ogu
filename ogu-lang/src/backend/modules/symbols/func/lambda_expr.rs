use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeComparation};
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
        self.check_args_can_be_replaced(&self.args, &args)?;
        let mut new_args: Vec<Box<dyn Symbol>> = vec![];
        for (a, b) in self.args.iter().zip(args.iter()) {
            if self.subtype(a.get_type(), b.get_type())? {
                new_args.push(IdSym::new_with_type(
                    a.get_name(),
                    b.get_type().clone(),
                ))
            } else {
                new_args.push(a.clone())
            }

        }
        self.args = new_args;
        self.resolve_type(scope)?;
        Ok(())
    }

    fn subtype(&self, a: Option<Box<dyn Type>>, b: Option<Box<dyn Type>>) -> Result<bool> {
        match a {
            None => match b {
                None => Ok(true),
                Some(_) => Ok(true)
            },
            Some(at) => match b {
                None => Ok(true),
                Some(bt) => {
                    let c = at.compare(&*bt);
                    match c {
                        TypeComparation::Superior => Ok(false),
                        TypeComparation::Incomparables => bail!("incompatible type for arg substitution in lambda expression, expecting {}, found {}", at.get_name(), bt.get_name()),
                        _ => Ok(true)
                    }
                }
            }
        }
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

    fn check_args_can_be_replaced(&self, own_args: &[Box<dyn Symbol>], args: &[Box<dyn Symbol>]) -> Result<()> {
        if own_args.len() != args.len() {
            println!("OWN ARGS = {:?}\nARGS = {:?}\n", own_args, args);
            bail!("wrong arguments passed in lambda expression")
        }
        for (a, b) in own_args.iter().zip(args.iter()) {
            if let (Some(ta), Some(tb)) = (a.get_type(), b.get_type()) {
                if &*ta != &*tb {
                    if !ta.is_compatible_with(&*tb) && !tb.is_compatible_with(&*ta) {
                        bail!("incompatible args passed for lambda expression\n TA = {:?}\n TB = {:?}", ta, tb)
                    }
                }
            }
        }
        Ok(())
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
