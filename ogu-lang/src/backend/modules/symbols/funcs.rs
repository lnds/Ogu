use crate::backend::modules::symbols::exprs::idents::IdSym;
use crate::backend::modules::types::basic_type::BasicType;
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
    args: Box<ArgsSym>,
    expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: &Args, expr: &Expression) -> Box<Self> {
        let ty: Option<Box<dyn Type>> = FuncType::from_ast_opt(args, expr);
        let expr: Box<dyn Symbol> = expr.into();
        let args: Box<ArgsSym> = args.clone().into();
        Box::new(FunctionSym {
            name: name.to_string(),
            args,
            expr,
            ty,
        })
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
        match &self.ty {
            Some(ty) if !ty.is_trait() => Ok(Some(ty.clone())),
            _ => {
                let mut sym_table = SymbolTable::new(&self.name, Some(scope.clone_box()));
                if let ArgsSym::Many(args) = &*self.args {
                    for a in args.iter() {
                        sym_table.define(a.clone());
                    }
                }
                self.expr.resolve_type(&mut *sym_table)?;
                for s in sym_table.get_symbols() {
                    self.args.define(s.clone_box());
                }
                let ty: Option<Box<dyn Type>> = FuncType::make(&*self.args, &*self.expr);
                self.ty = ty;
                Ok(self.get_type())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ArgsSym {
    Unit,
    Many(Vec<Box<dyn Symbol>>),
}

impl ArgsSym {
    fn new_unit() -> Box<Self> {
        Box::new(ArgsSym::Unit)
    }

    fn new_many(args: Vec<Box<dyn Symbol>>) -> Box<Self> {
        Box::new(ArgsSym::Many(args))
    }
}

impl Symbol for ArgsSym {
    fn get_name(&self) -> &str {
        "arg"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ArgsSym::Unit => Some(BasicType::unit()),
            _ => todo!(),
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, _scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        todo!()
    }
}

impl Scope for ArgsSym {
    fn scope_name(&self) -> &str {
        unimplemented!()
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        match self {
            ArgsSym::Unit => None,
            ArgsSym::Many(args) => {
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

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        match self {
            ArgsSym::Unit => None,
            ArgsSym::Many(args) => args.iter().find(|s| s.get_name() == name).cloned(),
        }
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        unimplemented!()
    }

    fn set_symbols(&mut self, _symbols: Vec<Box<dyn Symbol>>) {
        unimplemented!()
    }
}

impl<'a> From<Args<'a>> for Box<ArgsSym> {
    fn from(args: Args<'a>) -> Self {
        match args {
            Args::Void => ArgsSym::new_unit(),
            Args::Many(arg_vec) => ArgsSym::new_many(vec_args_into(&arg_vec)),
        }
    }
}

impl<'a> From<Arg<'a>> for Box<dyn Symbol> {
    fn from(arg: Arg<'a>) -> Self {
        match arg {
            Arg::Simple(s) => IdSym::new(s),
            _ => todo!(),
        }
    }
}

pub(crate) fn vec_args_into<'a>(args: &[Arg<'a>]) -> Vec<Box<dyn Symbol>> {
    args.iter().map(|a| a.clone().into()).collect()
}
