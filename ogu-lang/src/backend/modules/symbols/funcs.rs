use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeComparation};
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::decls::FuncTypeAst;
use anyhow::{bail, Result};
use crate::backend::compiler::default_sym_table;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Option<Vec<Box<dyn Symbol>>>,
    expr: Box<dyn Symbol>,
    ty: Option<Box<FuncType>>,
}

impl FunctionSym {
    pub(crate) fn make(
        name: &str,
        args: &Args,
        expr: &Expression,
        ft: &Option<FuncTypeAst>,
    ) -> Result<Box<dyn Symbol>> {
        let expr: Box<dyn Symbol> = expr.into();
        let mut args: Option<Vec<Box<dyn Symbol>>> = match args {
            Args::Void => None,
            Args::Many(args) => Some(vec_args_into(args)),
        };

        let ty = match ft {
            None => FuncType::make(&args, &*expr),
            Some(ft) => FuncType::check_and_make(name, ft, &mut args)?,
        };
        Ok(Box::new(FunctionSym {
            name: name.to_string(),
            args,
            expr,
            ty,
        }))
    }

    pub(crate) fn make_box(
        name: &str,
        args: &Args,
        expr: &Expression,
    ) -> Box<dyn Symbol> {
        let expr: Box<dyn Symbol> = expr.into();
        let args: Option<Vec<Box<dyn Symbol>>> = match args {
            Args::Void => None,
            Args::Many(args) => Some(vec_args_into(args)),
        };
        let ty = FuncType::make(&args, &*expr);
        Box::new(FunctionSym {
            name: name.to_string(),
            args,
            expr,
            ty,
        })
    }

    pub(crate) fn replace_args(
        &mut self,
        args: Vec<Box<dyn Symbol>>,
        scope: &mut dyn Scope,
        resolve: bool,
    ) -> Result<()> {
        if let Some(own_args) = &self.args {
            self.check_args_can_be_replaced(&own_args[..], &args)?;

            let mut new_args: Vec<Box<dyn Symbol>> = vec![];
            for (a, b) in own_args.iter().zip(args.iter()) {
                if self.subtype(a.get_type(), b.get_type())? {
                    new_args.push(IdSym::new_with_type(
                        a.get_name(),
                        b.get_type().clone(),
                    ))
                } else {
                    new_args.push(IdSym::new_with_type(
                        a.get_name(),
                        a.get_type().clone(),
                    ))
                }
            }
            self.args = Some(new_args);
            if resolve {
                self.resolve_type(scope)?;
            }
        }
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
                        TypeComparation::Incomparables => bail!("incompatible type for arg substitution in function {}, expecting {}, found {}", self.name, at.get_name(), bt.get_name()),
                        _ => Ok(true)
                    }
                }
            }
        }
    }

    fn check_args_can_be_replaced(&self, own_args: &[Box<dyn Symbol>], args: &[Box<dyn Symbol>]) -> Result<()> {
        if own_args.len() != args.len() {
            bail!("wrong arguments passed")
        }
        for (a, b) in own_args.iter().zip(args.iter()) {
            if let (Some(ta), Some(tb)) = (a.get_type(), b.get_type()) {
                if &*ta != &*tb {
                    if !ta.is_compatible_with(&*tb) && !tb.is_compatible_with(&*ta) {
                        bail!("incompatible args passed for function {}\n TA = {:?}\n TB = {:?}", self.name, ta, tb)
                    }
                }
            }
        }
        Ok(())
    }

    fn define_arg(&mut self, sym: Box<dyn Symbol>) -> Result<Option<Box<dyn Symbol>>> {
        match &mut self.args {
            None => Ok(None),
            Some(args) => {
                match args.iter_mut().find(|a| a.get_name() == sym.get_name()) {
                    None => Ok(None),
                    Some(a) => {
                        match a.get_type() {
                            None => {
                                *a = sym.clone_box();
                                Ok(Some(a.clone_box()))
                            }
                            Some(at) => match sym.get_type() {
                                None => Ok(None),
                                Some(st) => {
                                    if !st.is_compatible_with(&*at) {
                                        bail!("incompatible  type for argument {}, st = {:?} at = {:?} in function {}",
                                            a.get_name(), st, at, self.name)
                                    } else {
                                        Ok(Some(a.clone_box()))
                                    }
                                }
                            },
                        }
                    }
                }
            }
        }
    }
}

impl Symbol for FunctionSym {
    fn get_name(&self) -> &str {
        &self.name
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
                    println!("SET TYPE FUNC {}\n FROM: {:?}\n TO: {:?}\n", self.name, self.ty, ty);
                    self.ty = Some(Box::new(ty.clone()))
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new(&self.name, Some(scope.clone_box()));
        sym_table.set_function_name(&self.name);

        if let Some(args) = &self.args {
            for a in args.iter() {
                sym_table.define(a.clone());
            }
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
            let ty = FuncType::make(&self.args, &*self.expr);
            if let Some(ft1) = self.ty.clone() {
                if let Some(ft2) = ty.clone() {
                    if !ft1.result.is_compatible_with(&*ft2.result) {
                        bail!(
                        "incompatible type for return type in function {}\n\n ft1 = {:?}\n\n ft2 = {:?}\n\n",
                        self.name,
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

    fn storable(&self) -> bool {
        true
    }
}

impl<'a> From<Arg<'a>> for Box<dyn Symbol> {
    fn from(arg: Arg<'a>) -> Self {
        match arg {
            Arg::Simple(s) => IdSym::new(s),
            Arg::SimpleStr(s) => IdSym::new(&s),
            _ => panic!("Invalid Arg, internal parser error"),
        }
    }
}

pub(crate) fn vec_args_into(args: &[Arg]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.clone().into()).collect()
}
