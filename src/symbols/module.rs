use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::body::Declaration;
use crate::parser::ast::module::ModuleAst;
use crate::symbols::exprs::{Expr, Func};
use crate::symbols::scopes::Scope;
use crate::symbols::values::{Date, Int, Str};
use crate::symbols::{raise_symbol_table_error, Symbol, SymbolValue};
use anyhow::Result;
use std::collections::HashMap;
use crate::codegen::CodeGenerator;

pub(crate) struct Module {
    name: String,
    symbols: HashMap<String, Symbol>,
}

impl Module {
    pub(crate) fn new(module_ast: &ModuleAst, compiler: &dyn Scope) -> Result<Self> {
        let mut module = Module {
            name: module_ast.get_module_name(),
            symbols: HashMap::new(),
        };
        for decl in module_ast.body.declarations.iter() {
            Module::match_decl(decl, &mut module, compiler)?;
        }
        Ok(module)
    }

    pub(crate) fn get_name(&self) -> String {
        self.name.clone()
    }

    pub(crate) fn get_symbols(&self) -> Vec<Symbol> {
        self.symbols.values().cloned().collect()
    }

    fn match_decl(decl: &Declaration, module: &mut Module, compiler: &dyn Scope) -> Result<()> {
        macro_rules! check_or_define {
            ($t:ty, $name:ident, $v: expr) => {
                if module.resolve($name).is_some() {
                    println!("ya existe");
                } else {
                    module.define(<$t>::make($name, $v));
                };
            };
        }

        macro_rules! check_or_define_lr {
            ($t:ty, $t2: ident, $name:ident, $l: ident, $r: ident) => {
                if module.resolve($name).is_some() {
                    println!("ya existe");
                } else {
                    let left = Module::check_expr($l, module, compiler)?;
                    let right = Module::check_expr($r, module, compiler)?;
                    module.define(<$t>::make($name, <$t>::$t2, left, right));
                }
            };
        }

        macro_rules! check_or_define_fn {
            ($t:ty, $name:ident, $a: expr, $e: expr) => {
                if module.resolve($name).is_some() {
                    println!("ya existe");
                } else {
                    let exp = Module::check_expr($e, module, compiler)?;
                    let args = Module::check_args($a, module, compiler)?;
                    module.define(<$t>::make($name, args, exp));
                }
            };
        }

        match decl {
            Declaration::Value(name, Expression::DateLiteral(date)) => {
                check_or_define!(Date, name, date)
            }
            Declaration::Value(name, Expression::IntegerLiteral(int)) => {
                check_or_define!(Int, name, int)
            }
            Declaration::Value(name, Expression::StringLiteral(str)) => {
                check_or_define!(Str, name, str)
            }
            Declaration::Value(name, Expression::AddExpr(left, right)) => {
                check_or_define_lr!(Expr, Add, name, left, right)
            }
            Declaration::Value(name, Expression::MulExpr(left, right)) => {
                check_or_define_lr!(Expr, Mul, name, left, right)
            }
            Declaration::Function(name, args, expr) => check_or_define_fn!(Func, name, args, expr),
            d => {
                println!("match decl failed at: {:#?}", d);
                todo!();
            }
        };
        Ok(())
    }

    fn check_expr(
        expr: &Expression,
        module: &mut Module,
        compiler: &dyn Scope,
    ) -> Result<SymbolValue> {
        match expr {
            Expression::Identifier(id) => {
                Module::check_existence(id, module, compiler)?;
                Ok(SymbolValue::Ref(id.to_string()))
            }
            Expression::IntegerLiteral(lit) => Ok(SymbolValue::Int(lit.to_string())),
            Expression::StringLiteral(lit) => Ok(SymbolValue::Str(lit.to_string())),
            Expression::FuncCallExpr(fun, arg) => {
                let e1 = Module::check_expr(fun, module, compiler)?;
                let e2 = Module::check_expr(arg, module, compiler)?;
                Ok(SymbolValue::FuncCall(Box::new(e1), Box::new(e2)))
            }
            e => {
                println!("not implementes {:?}", e);
                todo!()
            }
        }
    }

    fn check_args(
        args: &Args,
        module: &mut Module,
        compiler: &dyn Scope,
    ) -> Result<SymbolValue> {
        match args {
            Args::Void => Ok(SymbolValue::Unit),
            Args::Many(args) =>
                Module::check_vec_args(args, module, compiler)
        }
    }


    fn check_vec_args(
        args: &Vec<Arg>,
        module: &mut Module,
        compiler: &dyn Scope,
    ) -> Result<SymbolValue> {
        let mut result = vec![];
        for arg in args.iter() {
            match arg {
                Arg::Simple(id) => result.push(SymbolValue::Ref(id.to_string())),
                Arg::Tuple(a) => result.push(SymbolValue::Tuple(Box::new(Module::check_vec_args(a, module, compiler)?))),
                Arg::Expr(e) => result.push(Module::check_expr(e, module, compiler)?),
            }
        }

        Ok(SymbolValue::Seq(result))
    }

    fn check_existence(name: &str, module: &mut Module, compiler: &dyn Scope) -> Result<()> {
        if module.resolve(name).is_some() {
            Ok(())
        } else {
            if compiler.resolve(name).is_some() {
                Ok(())
            } else {
                raise_symbol_table_error(
                    "symbol not found",
                    name.to_string(),
                    module.name.to_string(),
                )
            }
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

    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        generator.process(self)
    }
}
