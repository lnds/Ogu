use crate::backend::errors::OguError;
use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::codegen::CodeGenerator;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use crate::symbols::exprs::ExprSym;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::generic::GenericType;
use crate::types::Type;
use anyhow::{Error, Result};
use std::collections::hash_map::RandomState;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Vec<ArgSym>,
    expr: Box<ExprSym>,
    ty: Option<Box<dyn Type>>,
    enclosing_scope: Box<dyn Scope>,
}

impl FunctionSym {
    pub(crate) fn new(
        name: &str,
        args: Args,
        expr: Expression,
        enclosing_scope: Box<dyn Scope>,
    ) -> Box<Self> {
        Box::new(FunctionSym {
            name: name.to_string(),
            args: args.into(),
            expr: expr.into(),
            ty: None,
            enclosing_scope,
        })
    }

    pub(crate) fn get_args(&self) -> Vec<ArgSym> {
        self.args.to_vec()
    }

    fn solve_args_types(&self, args: &[ArgSym]) -> Vec<ArgSym> {
        let mut result = vec![];
        for a in args.iter() {
            match a.get_type() {
                None => result.push(ArgSym::new(
                    &a.get_name(),
                    Some(GenericType::new("T", self.find_traits(&a.get_name()))),
                )),
                Some(_) => result.push(a.clone()),
            }
        }
        result.to_vec()
    }

    fn find_traits(&self, name: &str) -> Vec<String> {
        self.expr.find_traits(name)
    }
}

impl Scope for FunctionSym {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, _sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        let sym = self.args.iter().find(|a| a.get_name() == name).cloned();
        match sym {
            None => self.enclosing_scope.resolve(name),
            Some(sym) => Some(Box::new(sym)),
        }
    }

    fn gen_code(&self, _generator: &mut Box<dyn CodeGenerator>) -> Result<(), Error> {
        unimplemented!()
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        let mut result: Vec<Box<dyn Symbol>> = vec![];
        for a in self.args.iter() {
            result.push(Box::new(a.clone()));
        }
        result
    }

    fn set_symbols(&mut self, _syms: HashMap<String, Box<dyn Symbol>>) {
        unimplemented!()
    }
}

impl Symbol for FunctionSym {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        Box::new(self.clone())
    }

    fn solve_type(&self, _scope: &Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        match &self.ty {
            Some(_) => Ok(Box::new(self.clone())),
            None => {
                let mut sym = FunctionSym {
                    name: self.name.clone(),
                    args: self.solve_args_types(&self.args),
                    expr: self.expr.clone(),
                    ty: self.get_type(),
                    enclosing_scope: self.enclosing_scope.clone(),
                };
                let sym_scope: Box<dyn Scope> = Box::new(sym.clone());
                let sym_expr = self.expr.solve_type(&sym_scope)?;
                sym.ty = sym_expr.get_type();
                Ok(Box::new(sym))
            }
        }
    }
}

impl SymbolWriter for FunctionSym {
    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<()> {
        match self.get_type() {
            None => Err(Error::new(OguError::CodeGenError)
                .context(format!("Symbol {:?} has no type", self.get_name()))),
            Some(func_type) => {
                let mut args = vec![];
                for a in self.args.iter() {
                    args.push(fmt.format_func_arg(
                        a.get_name(),
                        a.get_type().ok_or_else(|| {
                            Error::new(OguError::CodeGenError)
                                .context(format!("Argument {:?} has no type", a.get_name()))
                        })?,
                    ));
                }
                let args = args.join(", ");
                let mut s_params = HashSet::new();
                for a in self.args.iter() {
                    match a.get_type() {
                        None => {
                            return Err(Error::new(OguError::CodeGenError)
                                .context(format!("Arg {:?} has no type", a.get_name())))
                        }
                        Some(ty) => {
                            if ty.is_generic() {
                                s_params.insert(fmt.format_type_with_traits(&ty));
                            }
                        }
                    }
                }
                let s_params = s_params.iter().cloned().collect::<Vec<String>>();
                let s_params = if s_params.is_empty() {
                    String::new()
                } else {
                    s_params.join(", ")
                };
                let s_func_type = fmt.format_type(&func_type);
                let header =
                    fmt.format_generic_func_header(self.get_name(), s_params, args, s_func_type);

                writeln!(file, "{} {{", header)?;
                self.expr.write_symbol(fmt, file)?;
                writeln!(file, "}}")?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ArgSym {
    name: String,
    ty: Option<Box<dyn Type>>,
}

impl ArgSym {
    fn new(name: &str, ty: Option<Box<dyn Type>>) -> Self {
        ArgSym {
            name: name.to_string(),
            ty,
        }
    }
}

impl Symbol for ArgSym {
    fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        unimplemented!()
    }

    fn solve_type(&self, _scope: &Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        unimplemented!()
    }
}

impl<'a> From<Args<'a>> for Vec<ArgSym> {
    fn from(args: Args<'a>) -> Self {
        match args {
            Args::Void => vec![],
            Args::Many(args) => {
                let mut result = vec![];
                for a in args.iter() {
                    result.push(a.clone().into());
                }
                result
            }
        }
    }
}

impl<'a> From<Arg<'a>> for ArgSym {
    fn from(arg: Arg<'a>) -> Self {
        match arg {
            Arg::Simple(id) => ArgSym::new(id, None),
            _ => todo!(),
        }
    }
}
