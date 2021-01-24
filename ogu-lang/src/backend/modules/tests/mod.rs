mod test_cond_exprs;
mod test_curry;
mod test_dict_exprs;
mod test_func_decls;
mod test_lambdas;
mod test_lets_wheres;
mod test_lists_exprs;
mod test_logical;
mod test_paren_ops;
mod test_protos;
mod test_range_exprs;
mod test_recursives;
mod test_simple;
mod test_val_decls;
mod test_loops;
mod test_func_composition;
mod test_eulers;

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
