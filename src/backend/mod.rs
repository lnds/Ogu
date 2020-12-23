use crate::backend::banner::akarru;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use anyhow::Result;
use std::fmt::Debug;
use std::path::PathBuf;
use thiserror::Error;
use crate::parser::ast::module::ModuleAst;
use crate::lexer::token_stream::TokenStream;
use crate::symbols::scopes::Scope;
use crate::symbols::symbols::Symbol;
use crate::symbols::SymbolTable;
use crate::symbols::types::Type;
use crate::backend::params::Params;
use crate::symbols::module::Module;

pub mod banner;
pub mod params;
pub mod errors;


pub(crate) struct Compiler {
    show_tokens: bool,
    show_ast: bool,
    scopes: Vec<Box<dyn Scope>>,
}


impl Compiler {
    pub(crate) fn new(params: &Params) -> Self {
        let mut symbol_table = Box::new(SymbolTable::new("_ogu"));
        symbol_table.define(Symbol::Macro("printf!", Type::Unit, 1));
        symbol_table.define(Symbol::Macro("print!", Type::Unit, 1));
        Compiler {
            show_tokens: params.tokens,
            show_ast: params.print,
            scopes: vec![symbol_table]
        }
    }
}

impl Scope for Compiler {
    fn scope_name(&self) -> &str {
        unimplemented!()
    }

    fn define(&mut self, sym: Symbol) {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        unimplemented!()
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
        Ok(Box::new(Module::new(&module)))
    }

}


pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
    }
    let mut backend = Compiler::new(&params);
    backend.run(params.files.to_vec())
    /*
    if params.banner {
        akarru()?
    }
    let mut loader = Loader::new();
    for path in params.files.iter() {
        let mut lexer = create_lexer(path, params)?;
        let (tokens, large_strings) = lex(&mut lexer, params)?;
        let mut parser = create_parser(tokens, large_strings)?;
        let module = parser.parse(path)?;
        //let module = parse(path, params)?;
            //loader.parse(&module);
    }
    Ok(())

     */
}

/*
fn create_lexer<'a>(path: &'a PathBuf, params: &'a Params) -> Result<Lexer<'a>> {
    println!("parsing {:?}", path);
    Ok(Lexer::new(path)?)
}

fn lex<'a>(lexer: &'a mut Lexer<'a>, params: &Params) -> Result<(TokenStream<'a>, Vec<String>)> {
    let (tokens, large_strings) = lexer.scan()?;
    if params.tokens {
        let syms: Vec<Token> = tokens.iter().map(|t| t.token).collect();
        println!("TOKENS = {:?}", syms);
    }
    Ok((tokens, large_strings))
}

fn create_parser(tokens: TokenStream, large_strings: Vec<String>) -> Result<Parser> {
    Parser::new(tokens, large_strings)
}
*/
/*
fn parse<'a>(path: &'a PathBuf, params: &'a Params) -> Result<ModuleAst<'a>> {
    let mut lexer = Lexer::new(path)?;
    println!("parsing {:?}", path);
    let (tokens, large_strings) = lexer.scan()?;
    if params.tokens {
        let syms: Vec<Token> = tokens.iter().map(|t| t.token).collect();
        println!("TOKENS = {:?}", syms);
    }
    let mut parser = Parser::new(tokens, large_strings)?;
    let module = parser.parse(path)?;
    if params.print {
        println!("AST = {:#?}", module);
    }
    Ok(module)
}

*/