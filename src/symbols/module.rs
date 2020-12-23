use crate::backend::Compiler;
use crate::lexer::tokens::Token::{HashCurly, Mod};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::body::{BodyAst, Declaration};
use crate::parser::ast::module::exposing::Exposing;
use crate::parser::ast::module::ModuleAst;
use crate::symbols::scopes::Scope;
use crate::symbols::types::Type;
use crate::symbols::values::{Date, Int};
use crate::symbols::{Symbol, raise_symbol_table_error, SymbolValue};
use anyhow::Result;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use crate::symbols::exprs::{Expr, Ref};

pub(crate) struct Module {
    name: String,
    symbols: HashMap<String, Symbol>,
}

impl Module {
    pub(crate) fn new(module_ast: &ModuleAst, compiler: &Scope) -> Result<Self> {
        println!("new module {}", module_ast.get_module_name());
        let mut module = Module {
            name: module_ast.get_module_name(),
            symbols: HashMap::new()
        };
        for decl in module_ast.body.declarations.iter() {
            Module::match_decl(decl, &mut module, compiler)?;
        }
        Ok(module)
    }


    fn match_decl(decl: &Declaration, module: &mut Module, compiler: &Scope) -> Result<()> {
        match decl {
            Declaration::Value(name, Expression::DateLiteral(date)) => {
                module.define(Date::make(name, date))
            }
            Declaration::Value(name, Expression::IntegerLiteral(int)) => {
                module.define(Int::make(name, int))
            }
            Declaration::Value(name, Expression::AddExpr(left, right)) => {
                println!("{:?} + {:?}", left, right);
                let left = Module::check_expr(left, module, compiler)?;
                let right = Module::check_expr(right, module, compiler)?;
                module.define(Expr::make(name, Expr::Add, left, right))
                //module.define(Int::make(name, int))
            }
            _ => {
                todo!()
            }
        };
        Ok(())
    }

    fn check_expr(expr: &Expression, module: &mut Module, compiler: &Scope) -> Result <SymbolValue> {
        match expr {
            Expression::Identifier(id) => {
                Module::check_existence(id, module, compiler)?;
                Ok(SymbolValue::Ref(id.to_string()))
            }
            Expression::IntegerLiteral(lit) => {
                Ok(SymbolValue::Int(lit.to_string()))
            }
            _ => {
                todo!()
            }
        }
    }

    fn check_existence(name: &str, module: &mut Module, compiler: &Scope) -> Result<()> {
        if module.resolve(name).is_some() {
            Ok(())
        } else if compiler.resolve(name).is_some() {
            Ok(())
        } else {
            raise_symbol_table_error("symbol not found", name.to_string(), module.name.to_string())
        }
    }
}

impl<'a> Scope for Module {
    fn scope_name(&self) -> &str {
        &self.name
    }

    fn define(&mut self, sym: Symbol) -> Option<Symbol> {
        self.symbols.insert(sym.get_name(), sym)
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        self.symbols.get(name).cloned()
    }

    fn dump(&self) {
        println!("Module Scope: {}", self.scope_name());
        println!("Symbols:");
        println!("{:#?}", self.symbols);
    }
}

