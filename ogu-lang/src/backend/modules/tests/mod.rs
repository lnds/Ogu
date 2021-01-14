mod cond_exprs;
mod curry;
mod func_decls;
mod lambdas;
mod letswheres;
mod logical;
mod paren_ops;
mod protos;
mod recursives;
mod simple;
mod val_decls;
mod lists_exprs;

use crate::backend::modules::module::Module;
use crate::backend::scopes::Scope;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use anyhow::Result;

pub(crate) fn make_module(source: &str, sym_table: Box<dyn Scope>) -> Result<Module> {
    let mut lexer = Lexer::from(source);
    let (tokens, strs) = lexer.scan()?;
    let parser = Parser::new(tokens, strs)?;
    let module_ast = ModuleAst::parse(&parser, None, 0)?;
    println!("AST:\n\t{:#?}", module_ast);
    Module::new(module_ast, sym_table)
}
