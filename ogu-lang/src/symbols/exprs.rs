use crate::codegen::transpilers::SymbolWriter;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::expression::Expression::FuncCallExpr;
use crate::symbols::scopes::Scope;
use crate::symbols::Symbol;
use crate::types::Type;
use anyhow::Result;
use std::ops::Deref;

#[derive(Clone, Debug)]
pub(crate) struct ExprSym {
    expr: ExprSymEnum,
    ty: Option<Box<dyn Type>>,
}

impl ExprSym {
    fn new(expr: ExprSymEnum) -> Box<Self> {
        Box::new(ExprSym { expr, ty: None })
    }
}

#[derive(Clone, Debug)]
enum ExprSymEnum {
    Void,
    FuncCall(Box<ExprSym>, Box<ExprSym>),
}

impl<'a> From<Expression<'a>> for Box<ExprSym> {
    fn from(expr: Expression<'a>) -> Self {
        match expr {
            FuncCallExpr(id,  expr) => {
                ExprSym::new(ExprSymEnum::FuncCall(id.into(), expr.into()))
            }
            _e => {
                println!("not implemented from for {:?}", _e);
                todo!()
            }
        }
    }
}

impl<'a> From<Box<Expression<'a>>> for Box<ExprSym> {
    fn from(expr: Box<Expression<'a>>) -> Self {
        match expr {
            _e => {
                println!("not implemented from for boxed {:?}", _e);
                todo!()
            }
        }
    }
}

impl Symbol for ExprSym {
    fn get_name(&self) -> String {
        format!("{:?}", self)
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        unimplemented!()
    }

    fn get_symbol_writer(&self) -> Box<dyn SymbolWriter> {
        unimplemented!()
    }

    fn solve_type(&self, scope: &dyn Scope) -> Result<Box<dyn Symbol>> {
        unimplemented!()
    }
}
