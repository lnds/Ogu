use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;
use crate::symbols::Symbol;
use crate::types::Type;
use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::fmt::Display;

#[derive(Clone)]
pub(crate) struct FunctionSym {
    name: String,
    args: Vec<ArgSym>,
    //expr: ExprSym,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: Args, expr: Expression) -> Box<Self> {
        Box::new(FunctionSym {
            name: name.to_string(),
            args: args.into(),
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
}

impl SymbolWriter for FunctionSym {
    fn write_symbol(&self, fmt: &Box<dyn Formatter>, file: &mut File) -> Result<()> {
        let func_type = fmt.format_type(self.get_type());
        let mut args = String::new();
        let header = fmt.format_func_header(self.get_name(), args, func_type);
        writeln!(file, "{} {{", header)?;
        writeln!(file, "}}")?;
        Ok(())
    }
}

#[derive(Clone)]
pub(crate) struct ArgSym {
    name: String,
    ty: Option<Box<dyn Type>>
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
}

impl<'a> From<Args<'a>> for Vec<ArgSym> {
    fn from(args: Args<'a>) -> Self {
        match args {
            Args::Void => vec![],
            Args::Many(a) => todo!(),
        }
    }
}
