use crate::backend::errors::OguError;
use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;
use crate::symbols::exprs::ExprSym;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::Type;
use anyhow::{Error, Result};
use std::fs::File;
use std::io::Write;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Vec<ArgSym>,
    expr: Box<ExprSym>,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: Args, expr: Expression) -> Box<Self> {
        Box::new(FunctionSym {
            name: name.to_string(),
            args: args.into(),
            expr: expr.into(),
            ty: None,
        })
    }

    pub(crate) fn get_args(&self) -> Vec<ArgSym> {
        self.args.to_vec()
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

    fn solve_type(&self, scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
        match &self.ty {
            Some(_) => Ok(Box::new(self.clone())),
            None => {
                let sym_expr = self.expr.solve_type(scope)?;
                Ok(Box::new(FunctionSym{
                    name: self.name.clone(),
                    args: self.args.clone(),
                    expr: self.expr.clone(),
                    ty: sym_expr.get_type(),
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
        let args = String::new();
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
            Args::Many(_) => todo!(),
        }
    }
}
