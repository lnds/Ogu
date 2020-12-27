use crate::symbols::exprs::ExprSym;
use crate::types::Type;
use anyhow::{Result, Error};
use crate::parser::ast::expressions::expression::Expression;
use crate::symbols::Symbol;
use crate::codegen::transpilers::{SymbolWriter, Formatter};
use crate::symbols::scopes::Scope;
use std::fs::File;
use std::io::Write;
use crate::backend::errors::OguError;

#[derive(Clone, Debug)]
pub(crate) struct ValueSym {
    name: String,
    expr: Box<ExprSym>,
    ty: Option<Box<dyn Type>>,
}

impl ValueSym {
    pub(crate) fn new(name: &str, expr: Expression) -> Box<Self> {
        let expr : Box<ExprSym> = expr.into();
        let ty : Option<Box<dyn Type>> = expr.get_type();
        Box::new(ValueSym {
            name: name.to_string(),
            expr,
            ty,
        })
    }

}

impl Symbol for ValueSym {
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
        let sym_expr = self.expr.solve_type(scope)?;
        Ok(Box::new(
            ValueSym {
                name: self.name.clone(),
                expr: self.expr.clone(),
                ty: sym_expr.get_type()
            }
        ))
    }
}

impl SymbolWriter for ValueSym {
    fn write_symbol(&self, fmt: &Box<dyn Formatter>, file: &mut File) -> Result<()> {
        let func_type = fmt.format_type(self.get_type().ok_or_else(|| {
            Error::new(OguError::CodeGenError)
                .context(format!("Symbol {:?} has no type", self.get_name()))
        })?);
        let hdr = fmt.format_const_decl_header(&self.get_name(), &func_type);
        write!(file, "{}", hdr)?;
        self.expr.write_symbol(fmt, file)?;
        writeln!(file, ";")?;
        Ok(())
    }
}

