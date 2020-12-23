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
use crate::symbols::scopes::Scope;

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

pub(crate) struct Backend {
    show_tokens: bool,
    show_ast: bool,
    loader: Loader,
}


impl Backend {
    pub(crate) fn new(params: &Params) -> Self {
        Backend {
            show_tokens: params.tokens,
            show_ast: params.print,
            loader: Loader::new(),
        }
    }
}

impl Backend {
    pub(crate) fn run(&mut self, files: Vec<PathBuf>) -> Result<()> {
        let mut scope = self.loader.get_scope();
        for path in files.iter() {
            scope = self.compile(path.clone(), scope)?;
        }
        Ok(())
    }

    fn compile(&mut self, path: PathBuf, scope: Box<dyn Scope>) -> Result<(Box<dyn Scope>)> {
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
        Ok(module.resolve_names(scope))
    }

}


pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
    }
    let mut backend = Backend::new(&params);
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