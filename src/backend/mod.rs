use crate::backend::banner::akarru;
use crate::backend::params::Params;
use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use crate::symbols::module::Module;
use crate::symbols::scopes::Scope;
use crate::symbols::sym_table::SymbolTable;
use crate::symbols::types::Type;
use crate::symbols::{Macro, Symbol};
use anyhow::Result;
use std::fmt::Debug;
use std::path::PathBuf;
use thiserror::Error;

pub mod banner;
pub mod errors;
pub mod params;

pub(crate) struct Compiler {
    show_tokens: bool,
    show_ast: bool,
    scopes: Vec<Box<dyn Scope>>,
}

impl Compiler {
    pub(crate) fn new(params: &Params) -> Self {
        let mut symbol_table = Box::new(SymbolTable::new("_ogu"));
        symbol_table.define(Macro::make("printf!", Type::Unit, 1));
        symbol_table.define(Macro::make("print!", Type::Unit, 1));
        Compiler {
            show_tokens: params.tokens,
            show_ast: params.print,
            scopes: vec![symbol_table],
        }
    }
}

impl Scope for Compiler {
    fn scope_name(&self) -> &str {
        "_compiler"
    }

    fn define(&mut self, _: Symbol) {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        for s in self.scopes.iter() {
            if let Some(sym) = s.resolve(name) {
                return Some(sym);
            }
        }
        None
    }

    fn dump(&self) {
        for scope in self.scopes.iter() {
            scope.dump();
        }
    }
}

impl Compiler {
    pub(crate) fn run(&mut self, files: Vec<PathBuf>) -> Result<()> {
        for path in files.iter() {
            let scope = self.compile(path.clone())?;
            self.scopes.push(scope);
        }
        Ok(())
    }

    fn compile(&self, path: PathBuf) -> Result<Box<dyn Scope>> {
        let mut lexer = Lexer::new(&path)?;
        println!("parsing {:?}", &path);
        let (tokens, strs) = lexer.scan()?;
        if self.show_tokens {
            let syms: Vec<Token> = tokens.iter().map(|t| t.token).collect();
            println!("TOKENS = {:?}", syms);
        }
        let parser = Parser::new(tokens.to_owned(), strs.to_vec())?;
        let module = ModuleAst::parse(&parser, path, 0)?;
        if self.show_ast {
            println!("AST = {:#?}", module);
        }
        Ok(Box::new(Module::new(&module)?))
    }
}

pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
    }
    let mut backend = Compiler::new(&params);
    backend.run(params.files.to_vec())?;
    backend.dump();
    Ok(())
}
