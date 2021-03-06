use crate::backend::modules::module::Module;
use crate::backend::modules::symbols::macro_sym::MacroSym;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::modules::types::variadic_type::VariadicType;
use crate::backend::scopes::sym_table::SymbolTable;
use crate::backend::scopes::types::TypeClone;
use crate::backend::scopes::Scope;
use crate::lexer::tokens::Lexeme;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use anyhow::Result;
use std::path::PathBuf;

pub fn compile(path: PathBuf, show_tokens: bool, show_ast: bool, dump: bool) -> Result<Module> {
    let mut lexer = Lexer::new(&path)?;
    let (tokens, strs) = lexer.scan()?;
    if show_tokens {
        let syms: Vec<Lexeme> = tokens.iter().map(|t| t.lexeme).collect();
        println!("TOKENS = {:?}", syms);
    }
    let parser = Parser::new(tokens.to_owned(), strs.to_vec());
    let module_ast = ModuleAst::parse(&parser, Some(path), 0)?;
    if show_ast {
        println!("AST = {:#?}", module_ast);
    }
    let scope = Module::new(module_ast, default_sym_table())?;
    if dump {
        println!("Symbols:");
        println!("{:#?}", scope.get_decls());
    }
    Ok(scope)
}

pub(crate) fn default_sym_table() -> Box<dyn Scope> {
    let mut symbol_table: Box<dyn Scope> = SymbolTable::new("_ogu", None);
    symbol_table.define(MacroSym::new(
        "println!",
        Some(VariadicType::new(BasicType::unit())),
    ));
    symbol_table.define(MacroSym::new(
        "print!",
        Some(VariadicType::new(BasicType::unit())),
    ));
    symbol_table.define(MacroSym::new(
        "error!",
        Some(VariadicType::new(TRAIT_UNKNOWN.clone_box())),
    ));
    symbol_table
}
