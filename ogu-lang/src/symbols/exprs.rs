use crate::codegen::transpilers::{Formatter, SymbolWriter};
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::expression::Expression::{
    AddExpr, DivExpr, EqExpr, FloatLiteral, FuncCallExpr, GeExpr, GtExpr, Identifier, IfExpr,
    IntegerLiteral, LeExpr, LetExpr, LtExpr, ModExpr, MulExpr, NeExpr, PowExpr, StringLiteral,
    SubExpr, Unit,
};
use crate::symbols::scopes::Scope;
use crate::symbols::{raise_symbol_table_error, Symbol};
use crate::types::basic::BasicType;
use crate::types::Type;
use anyhow::{Error, Result};
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;
use std::ops::Deref;
use crate::symbols::sym_table::SymbolTable;

#[derive(Clone, Debug)]
pub(crate) struct ExprSym {
    expr: ExprSymEnum,
    ty: Option<Box<dyn Type>>,
}

impl ExprSym {
    fn new(expr: ExprSymEnum) -> Self {
        let ty = ExprSym::type_of(&expr);
        ExprSym { expr, ty }
    }

    fn type_of(expr: &ExprSymEnum) -> Option<Box<dyn Type>> {
        match expr {
            ExprSymEnum::Str(_) => Some(Box::new(BasicType::Str)),
            ExprSymEnum::Int(str) => Some(BasicType::int(str)),
            ExprSymEnum::Float(str) => Some(BasicType::float(str)),
            _ => None,
        }
    }

    pub(crate) fn find_traits(&self, name: &str) -> Vec<String> {
        let mut result: HashSet<String> = HashSet::new();
        match &self.expr {
            ExprSymEnum::If(c, t, e) => {
                result.extend(c.find_traits(name).iter().cloned());
                result.extend(t.find_traits(name).iter().cloned());
                result.extend(e.find_traits(name).iter().cloned());
            }
            ExprSymEnum::Gt(l, r)
            | ExprSymEnum::Ge(l, r)
            | ExprSymEnum::Lt(l, r)
            | ExprSymEnum::Le(l, r) => {
                if l.contains_id(name) || r.contains_id(name) {
                    result.insert("PartialOrd".to_string());
                }
            }
            ExprSymEnum::Eq(l, r) | ExprSymEnum::Ne(l, r) => {
                if l.contains_id(name) || r.contains_id(name) {
                    result.insert("PartialEq".to_string());
                }
            }
            e => {
                println!("!!! e = {:?}", e)
            }
        }
        result.iter().cloned().collect()
    }

    fn contains_id(&self, name: &str) -> bool {
        println!("contains {:?} in {:?}", name, self);
        match &self.expr {
            ExprSymEnum::Id(id) => id == name,
            ExprSymEnum::Gt(l, r) => l.contains_id(name) || r.contains_id(name),
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
enum ExprSymEnum {
    Void,
    Id(String),
    Str(String),
    Int(String),
    Float(String),
    FuncCall(Box<ExprSym>, Vec<ExprSym>),
    Add(Box<ExprSym>, Box<ExprSym>),
    Sub(Box<ExprSym>, Box<ExprSym>),
    Mul(Box<ExprSym>, Box<ExprSym>),
    Div(Box<ExprSym>, Box<ExprSym>),
    Mod(Box<ExprSym>, Box<ExprSym>),
    Pow(Box<ExprSym>, Box<ExprSym>),
    Le(Box<ExprSym>, Box<ExprSym>),
    Lt(Box<ExprSym>, Box<ExprSym>),
    Ge(Box<ExprSym>, Box<ExprSym>),
    Gt(Box<ExprSym>, Box<ExprSym>),
    Eq(Box<ExprSym>, Box<ExprSym>),
    Ne(Box<ExprSym>, Box<ExprSym>),
    Let(Vec<ExprSym>, Box<ExprSym>),
    If(Box<ExprSym>, Box<ExprSym>, Box<ExprSym>),
    Val(String, Box<ExprSym>),
}

impl ExprSymEnum {
    fn get_name(&self) -> String {
        match self {
            ExprSymEnum::Id(s) => s.to_string(),
            ExprSymEnum::FuncCall(e, _) => e.get_name(),
            _ => format!("{:?}", self),
        }
    }

    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<()> {
        match self {
            ExprSymEnum::Id(id) => write!(file, "{}", id)?,
            ExprSymEnum::FuncCall(e, args) => {
                let f_call = e.expr.format(fmt);
                let mut f_args = vec![];
                for a in args.iter() {
                    f_args.push(a.expr.format(fmt));
                }
                let f_args = f_args.join(",");
                writeln!(file, "{}", fmt.format_func_call(&f_call, &f_args))?;
            }
            ExprSymEnum::If(c, t, e) => {
                let c = c.expr.format(fmt);
                let t = t.expr.format(fmt);
                let e = e.expr.format(fmt);
                writeln!(file, "{}", fmt.format_if_expr(&c, &t, &e))?;
            }
            ExprSymEnum::Add(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                writeln!(file, "{} + {}", ls, rs)?;
            }
            ExprSymEnum::Sub(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                writeln!(file, "{} - {}", ls, rs)?;
            }
            ExprSymEnum::Mul(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                writeln!(file, "{} * {}", ls, rs)?;
            }
            ExprSymEnum::Div(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                writeln!(file, "{} / {}", ls, rs)?;
            }
            ExprSymEnum::Mod(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                writeln!(file, "{} % {}", ls, rs)?;
            }
            ExprSymEnum::Int(s) => {
                write!(file, "{}", s)?;
            }
            _s => {
                println!("write_symbol not implemented for {:?}", _s);
                todo!()
            }
        };
        Ok(())
    }

    fn format(&self, fmt: &dyn Formatter) -> String {
        match self {
            ExprSymEnum::Id(id) => fmt.format_id(id),
            ExprSymEnum::Str(s) => fmt.format_str(s),
            ExprSymEnum::Int(s) => fmt.format_int(s),
            ExprSymEnum::Eq(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_eq(&ls, &rs)
            }
            ExprSymEnum::Ne(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_ne(&ls, &rs)
            }
            ExprSymEnum::Gt(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_gt(&ls, &rs)
            }
            ExprSymEnum::Ge(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_ge(&ls, &rs)
            }
            ExprSymEnum::Lt(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_lt(&ls, &rs)
            }
            ExprSymEnum::Le(l, r) => {
                let ls = l.expr.format(fmt);
                let rs = r.expr.format(fmt);
                fmt.format_le(&ls, &rs)
            }
            ExprSymEnum::FuncCall(f, args) => {
                let fs = f.expr.format(fmt);
                let mut f_args = vec![];
                for a in args.iter() {
                    f_args.push(a.expr.format(fmt));
                }
                let f_args = f_args.join(",");
                fmt.format_func_call(&fs, &f_args)
            }
            _e => {
                println!("format not implemented for {:?}", _e);
                todo!()
            }
        }
    }
}

impl<'a> From<Expression<'a>> for Box<ExprSym> {
    fn from(expr: Expression<'a>) -> Self {
        Box::new(expr.into())
    }
}

impl<'a> From<Equation<'a>> for ExprSym {
    fn from(eq: Equation<'a>) -> Self {
        match eq {
            Equation::Value(id, exp) => ExprSym::new(ExprSymEnum::Val(id.to_string(), exp.into())),
            _ => todo!(),
        }
    }
}

impl<'a> From<Expression<'a>> for ExprSym {
    fn from(expr: Expression<'a>) -> Self {
        match expr {
            Unit => ExprSym::new(ExprSymEnum::Void),
            FuncCallExpr(id, args) => ExprSym::new(ExprSymEnum::FuncCall(
                id.into(),
                args.iter().map(|a| a.clone().into()).collect(),
            )),
            Identifier(id) => ExprSym::new(ExprSymEnum::Id(id.to_string())),
            StringLiteral(s) => ExprSym::new(ExprSymEnum::Str(s.to_string())),
            IntegerLiteral(s) => ExprSym::new(ExprSymEnum::Int(s.to_string())),
            FloatLiteral(s) => ExprSym::new(ExprSymEnum::Float(s.to_string())),
            AddExpr(l, r) => ExprSym::new(ExprSymEnum::Add(l.into(), r.into())),
            SubExpr(l, r) => ExprSym::new(ExprSymEnum::Sub(l.into(), r.into())),
            MulExpr(l, r) => ExprSym::new(ExprSymEnum::Mul(l.into(), r.into())),
            DivExpr(l, r) => ExprSym::new(ExprSymEnum::Div(l.into(), r.into())),
            ModExpr(l, r) => ExprSym::new(ExprSymEnum::Mod(l.into(), r.into())),
            PowExpr(l, r) => ExprSym::new(ExprSymEnum::Pow(l.into(), r.into())),
            GeExpr(l, r) => ExprSym::new(ExprSymEnum::Ge(l.into(), r.into())),
            GtExpr(l, r) => ExprSym::new(ExprSymEnum::Gt(l.into(), r.into())),
            LeExpr(l, r) => ExprSym::new(ExprSymEnum::Le(l.into(), r.into())),
            LtExpr(l, r) => ExprSym::new(ExprSymEnum::Lt(l.into(), r.into())),
            EqExpr(l, r) => ExprSym::new(ExprSymEnum::Eq(l.into(), r.into())),
            NeExpr(l, r) => ExprSym::new(ExprSymEnum::Ne(l.into(), r.into())),
            LetExpr(eqv, expr) => {
                let mut eqs = vec![];
                for e in eqv.iter() {
                    eqs.push(e.clone().into());
                }
                ExprSym::new(ExprSymEnum::Let(eqs, expr.into()))
            }
            IfExpr(c, t, e) => ExprSym::new(ExprSymEnum::If(c.into(), t.into(), e.into())),
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

    fn solve_type(&self, scope: &Box<dyn Scope>) -> Result<Box<dyn Symbol>> {
        match &self.ty {
            Some(_) => Ok(Box::new(self.clone())),
            None => match self.find_type(scope.clone()) {
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

impl ExprSym {
    fn make_copy(&self, ty: Box<dyn Type>) -> Box<dyn Symbol> {
        Box::new(ExprSym {
            expr: self.expr.clone(),
            ty: Some(ty),
        })
    }

    fn find_type(&self, scope: Box<dyn Scope>) -> Option<Box<dyn Type>> {
        match &self.expr {
            ExprSymEnum::Void => Some(BasicType::unit()),
            ExprSymEnum::Id(id) => self.resolve_type_from_name(scope, &id),
            ExprSymEnum::FuncCall(e, _) => e.find_type(scope),
            ExprSymEnum::Add(l, r)
            | ExprSymEnum::Sub(l, r)
            | ExprSymEnum::Mul(l, r)
            | ExprSymEnum::Div(l, r)
            | ExprSymEnum::Mod(l, r) => {
                let lt = self.resolve_type_from_sym(scope.clone(), l)?;
                let rt = self.resolve_type_from_sym(scope.clone(), r)?;
                if lt != rt {
                    None
                } else {
                    Some(lt)
                }
            }
            ExprSymEnum::If(_, t, e) => {
                let tt = self.resolve_type_from_sym(scope.clone(), t)?;
                let et = self.resolve_type_from_sym(scope.clone(), e)?;
                if tt != et {
                    None
                } else {
                    Some(tt)
                }
            }
            ExprSymEnum::Let(eqs, expr) => {
                let mut sym_table = SymbolTable::new("let", Some(scope.clone()));
                for eq in eqs.iter() {
                    println!("adding symbol eq = {:#?}", eq);
                    sym_table.define(Box::new(eq.clone()));
                }
                println!("sym table for let = {:#?}", sym_table);
                todo!()
            }

            _e => {
                println!("ExprSym::find_type not implemented for {:?}", _e);
                todo!()
            }
        }
    }

    fn resolve_type_from_name(&self, scope: Box<dyn Scope>, name: &str) -> Option<Box<dyn Type>> {
        match scope.resolve(name) {
            None => None,
            Some(sym) => sym.get_type(),
        }
    }

    fn resolve_type_from_sym(&self, scope: Box<dyn Scope>, sym: &ExprSym) -> Option<Box<dyn Type>> {
        match sym.get_type() {
            Some(ty) => Some(ty.clone()),
            None => {
                let s = scope.resolve(&sym.get_name());
                match s {
                    None => None,
                    Some(s) => s.get_type(),
                }
            }
        }
    }
}

impl SymbolWriter for ExprSym {
    fn write_symbol(&self, fmt: &dyn Formatter, file: &mut File) -> Result<(), Error> {
        self.expr.write_symbol(fmt, file)
    }
}
