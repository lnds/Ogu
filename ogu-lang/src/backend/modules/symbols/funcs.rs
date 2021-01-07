use crate::backend::modules::symbols::idents::IdSym;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use anyhow::Result;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Option<Vec<Box<dyn Symbol>>>,
    expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: &Args, expr: &Expression) -> Box<Self> {
        let ty: Option<Box<dyn Type>> = FuncType::from_ast_opt(args, expr);
        let expr: Box<dyn Symbol> = expr.into();
        let args: Option<Vec<Box<dyn Symbol>>> = match args {
            Args::Void => None,
            Args::Many(args) => Some(vec_args_into(args)),
        };

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
    ) -> Result<()> {
        if let Some(own_args) = &self.args {
            let mut new_args: Vec<Box<dyn Symbol>> = vec![];
            for (p, a) in own_args.iter().enumerate() {
                new_args.push(IdSym::new_with_type(
                    a.get_name(),
                    args[p].get_type().clone(),
                ))
            }
            self.args = Some(new_args);
            self.resolve_type(scope)?;
        }
        Ok(())
    }

    fn define_arg(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        match &mut self.args {
            None => None,
            Some(args) => {
                for a in args.iter_mut() {
                    if a.get_name() == sym.get_name() {
                        *a = sym.clone_box();
                        return Some(a.clone_box());
                    }
                }
                None
            }
        }
    }
}

impl Symbol for FunctionSym {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let mut sym_table = SymbolTable::new(&self.name, Some(scope.clone_box()));

        if let Some(args) = &self.args {
            for a in args.iter() {
                sym_table.define(a.clone());
            }
        }

        println!("scope antes de expr resolve = {:#?}", sym_table);
        self.expr.resolve_type(&mut *sym_table)?;
        println!("scope despues de expr resolve = {:#?}", sym_table);

        for s in sym_table.get_symbols() {
            self.define_arg(s.clone_box());
        }
        let ty: Option<Box<dyn Type>> = FuncType::make(&self.args, &*self.expr);
        self.ty = ty;
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
            _ => todo!(),
        }
    }
}

pub(crate) fn vec_args_into<'a>(args: &[Arg<'a>]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.clone().into()).collect()
}
