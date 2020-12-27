use crate::codegen::transpilers::{SymbolWriter, Formatter};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::expression::Expression::{
    FuncCallExpr, Identifier, StringLiteral,
};
use crate::symbols::scopes::Scope;
use crate::symbols::{raise_symbol_table_error, Symbol};
use crate::types::basic::BasicType;
use crate::types::Type;
use anyhow::{Error, Result};
use std::ops::Deref;
use std::fs::File;
use std::io::Write;

#[derive(Clone, Debug)]
pub(crate) struct ExprSym {
    expr: ExprSymEnum,
    ty: Option<Box<dyn Type>>,
}

impl ExprSym {
    fn new(expr: ExprSymEnum) -> Box<Self> {
        let ty = ExprSym::type_of(&expr);
        Box::new(ExprSym { expr, ty })
    }

    fn type_of(expr: &ExprSymEnum) -> Option<Box<dyn Type>> {
        match expr {
            ExprSymEnum::Str(_) => Some(Box::new(BasicType::Str)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
enum ExprSymEnum {
    Void,
    Id(String),
    Str(String),
    FuncCall(Box<ExprSym>, Box<ExprSym>),
}

impl ExprSymEnum {

    fn get_name(&self) -> String {
        match self {
            ExprSymEnum::Void => String::new(),
            ExprSymEnum::Id(s) => s.clone(),
            ExprSymEnum::Str(s) => s.clone(),
            ExprSymEnum::FuncCall(e, _) => e.get_name()
        }
    }

    fn write_symbol(&self, fmt: &Box<dyn Formatter>, file: &mut File) -> Result<()> {
        match self {
            ExprSymEnum::Id(id) => write!(file, "{}", id)?,
            ExprSymEnum::FuncCall(e, a) => {
                let f_call = e.expr.format(fmt);
                let f_args = a.expr.format(fmt);
                writeln!(file, "{}", fmt.format_func_call(&f_call, &f_args))?;
            }
            _ => {
                todo!()
            }
        };
        Ok(())
    }

    fn format(&self, fmt: &Box<dyn Formatter>) -> String {
     match self {
         ExprSymEnum::Id(id) => fmt.format_id(id),
         ExprSymEnum::Str(s) => fmt.format_str(s),
         _e => {
             println!("format not implemented for {:?}", _e);
             todo!()
         }
     }
    }
}

impl<'a> From<Expression<'a>> for Box<ExprSym> {
    fn from(expr: Expression<'a>) -> Self {
        match expr {
            FuncCallExpr(id, expr) => ExprSym::new(ExprSymEnum::FuncCall(id.into(), expr.into())),
            Identifier(id) => ExprSym::new(ExprSymEnum::Id(id.to_string())),
            StringLiteral(s) => ExprSym::new(ExprSymEnum::Str(s.to_string())),
            _e => {
                println!("not implemented from for {:?}", _e);
                todo!()
            }
        }
    }
}

impl<'a> From<Box<Expression<'a>>> for Box<ExprSym> {
    fn from(expr: Box<Expression<'a>>) -> Self {
        let ex = expr.deref().clone();
        ex.into()
    }
}

impl Symbol for ExprSym {
    fn get_name(&self) -> String {
        self.expr.get_name()
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        Box::new(self.clone())
    }

    fn solve_type(&self, scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
        match &self.ty {
            Some(ty) => Ok(self.make_copy(ty.clone())),
            None => match self.find_type(scope) {
                Some(ty) => Ok(self.make_copy(ty)),
                None => raise_symbol_table_error(
                    "type not found",
                    self.get_name(),
                    scope.scope_name().to_string(),
                ),
            },
        }
    }
}

impl SymbolWriter for ExprSym {
    fn write_symbol(&self, fmt: &Box<dyn Formatter>, file: &mut File) -> Result<(), Error> {
        self.expr.write_symbol(fmt, file)
    }
}

impl ExprSym {
    fn make_copy(&self, ty: Box<dyn Type>) -> Box<dyn Symbol> {
        Box::new(ExprSym {
            expr: self.expr.clone(),
            ty: Some(ty),
        })
    }

    fn find_type(&self, scope: &dyn Scope) -> Option<Box<dyn Type>> {
        match &self.expr {
            ExprSymEnum::Id(id) => self.resolve(scope, &id),
            ExprSymEnum::FuncCall(e, _) => {
                e.find_type(scope)
            },
            _e => {
                println!("ExprSym::find_type not implemented for {:?}", _e);
                todo!()
            }
        }
    }

    fn resolve(&self, scope: &dyn Scope, name: &str) -> Option<Box<dyn Type>> {
        match scope.resolve(name) {
            None => None,
            Some(sym) => sym.get_type(),
        }
    }


}