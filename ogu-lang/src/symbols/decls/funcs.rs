use crate::backend::errors::OguError;
use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use crate::symbols::exprs::ExprSym;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::Type;
use anyhow::{Error, Result};
use std::fs::File;
use std::io::Write;
use crate::codegen::CodeGenerator;
use crate::types::generic::GenericType;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Vec<ArgSym>,
    expr: Box<ExprSym>,
    ty: Option<Box<dyn Type>>,
    enclosing_scope: Box<dyn Scope>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: Args, expr: Expression, enclosing_scope: Box<dyn Scope>) -> Box<Self> {
        Box::new(FunctionSym {
            name: name.to_string(),
            args: args.into(),
            expr: expr.into(),
            ty: None,
            enclosing_scope
        })
    }

    pub(crate) fn get_args(&self) -> Vec<ArgSym> {
        self.args.to_vec()
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
            Some(sym) => Some(Box::new(sym.clone()))
        }
    }

    fn gen_code(&self, _generator: &mut Box<dyn CodeGenerator>) -> Result<(), Error> {
        unimplemented!()
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        let mut result : Vec<Box<dyn Symbol>> = vec![];
        for a in self.args.iter() {
            result.push(Box::new(a.clone()));
        }
        result
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

    fn solve_type(&self, _scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
        match &self.ty {
            Some(_) => Ok(Box::new(self.clone())),
            None => {
                let sym_expr = self.expr.solve_type(self)?;
                Ok(Box::new(FunctionSym {
                    name: self.name.clone(),
                    args: self.args.clone(),
                    expr: self.expr.clone(),
                    ty: sym_expr.get_type(),
                    enclosing_scope: self.enclosing_scope.clone()
                }))
            }
        }
    }
}

impl SymbolWriter for FunctionSym {
    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<()> {
        let func_type = fmt.format_type(self.get_type().ok_or_else(|| {
            Error::new(OguError::CodeGenError)
                .context(format!("Symbol {:?} has no type", self.get_name()))
        })?);
        let mut args = vec![];
        for a in self.args.iter() {
            args.push(fmt.format_func_arg(a.get_name(), a.get_type().ok_or_else(||{
                Error::new(OguError::CodeGenError)
                    .context(format!("Argument {:?} has no type", a.get_name()))
            })?));
        }
        let args = args.join(", ");
        let header = fmt.format_func_header(self.get_name(), args, func_type);
        writeln!(file, "{} {{", header)?;
        self.expr.write_symbol(fmt, file)?;
        writeln!(file, "}}")?;
        Ok(())
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

    fn solve_type(&self, _scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
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
            Arg::Simple(id) => ArgSym::new(id, Some(GenericType::new("T"))),
            _ => todo!(),
        }
    }
}
