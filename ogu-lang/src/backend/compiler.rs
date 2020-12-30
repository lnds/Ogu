use crate::lexer::tokens::Lexeme;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use anyhow::Result;
use std::path::PathBuf;
use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::symbols::macro_sym::MacroSym;
use crate::backend::modules::module::Module;


pub fn compile(path: PathBuf, show_tokens: bool, show_ast: bool, dump: bool) -> Result<()> {
    let mut lexer = Lexer::new(&path)?;
    println!("parsing {:?}", &path);
    let (tokens, strs) = lexer.scan()?;
    if show_tokens {
        let syms: Vec<Lexeme> = tokens.iter().map(|t| t.lexeme).collect();
        println!("TOKENS = {:?}", syms);
    }
    let parser = Parser::new(tokens.to_owned(), strs.to_vec())?;
    let module_ast = ModuleAst::parse(&parser, path, 0)?;
    if show_ast {
        println!("AST = {:#?}", module_ast);
    }

    let mut symbol_table: Box<dyn Scope> = SymbolTable::new("_ogu", None);
    symbol_table.define(&MacroSym::new("println!", Some(BasicType::unit())));
    symbol_table.define(&MacroSym::new("print!", Some(BasicType::unit())));

    let scope = Module::new(module_ast, symbol_table)?;
    if dump {}
    Ok(())
}
