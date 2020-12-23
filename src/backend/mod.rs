use crate::backend::banner::akarru;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use anyhow::Result;
use std::fmt::Debug;
use std::path::PathBuf;
use structopt::StructOpt;
use thiserror::Error;
use crate::symbols::loader::Loader;
use crate::parser::ast::module::ModuleAst;
use crate::lexer::token_stream::TokenStream;

pub mod banner;

#[derive(Error, Debug)]
pub enum OguError {
    #[error("Can't load Figfont")]
    FigfontError(String),
    #[error("Source not found")]
    NotFound(String),
    #[error("Parser error")]
    ParserError(String),
    #[error("Symbol table error")]
    SymbolTableError(String),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

const HELP_GREETINGS: &str = "\n\t\tOLA AMIKO MIO DE MI\n\n";

#[derive(Debug, StructOpt)]
#[structopt(name = "ogu", about = HELP_GREETINGS)]
pub struct Params {
    /// shows akarrú banner
    #[structopt(short, long)]
    banner: bool,

    /// print TOKENS
    #[structopt(short, long)]
    tokens: bool,

    /// print AST
    #[structopt(short, long)]
    print: bool,

    #[structopt(parse(from_os_str), help = "ogu modules...", required = true)]
    files: Vec<PathBuf>,

    #[structopt(
        name = "ARGS",
        required_if("has_args", "true"),
        last = true,
        help = "args for main module"
    )]
    args: Vec<String>,
}

pub(crate) struct Backend<'a> {
    params: &'a Params,
    loader: Loader<'a>,
    modules: Vec<ModuleAst<'a>>
}


impl<'a> Backend<'a> {
    pub(crate) fn new(params: &'a Params) -> Self {
        Backend {
            params,
            loader: Loader::new(),
            modules: vec![],
        }
    }
}

impl<'a> Backend<'a> {

    pub(crate) fn run(&mut self) -> Result<()> {
        if self.params.banner {
            akarru()?;
        }
        for path in self.params.files.iter() {
            self.compile(path);
        }
        Ok(())
    }

    fn compile(&mut self, path: &PathBuf) -> Result<()> {
        let mut lexer = Lexer::new(path)?;
        println!("parsing {:?}", path);
        let (tokens, strs) = lexer.scan()?;
        if self.params.tokens {
            let syms: Vec<Token> = tokens.iter().map(|t| t.token).collect();
            println!("TOKENS = {:?}", syms);
        }
        let mut parser  = Parser::new(tokens.to_owned(), strs.to_vec())?;
        let module = parser.parse(path)?;
        if self.params.print {
            println!("AST = {:#?}", module);
        }
        //self.loader.add(&module);
        Ok(())
    }

}


pub fn run(params: &Params) -> Result<()> {
    let mut backend = Backend::new(params);
    backend.run()
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