use crate::backend::banner::akarru;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use anyhow::Result;
use std::fmt::Debug;
use std::path::PathBuf;
use structopt::StructOpt;
use thiserror::Error;
use crate::symbols::loader::Loader;

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

pub fn run(params: &Params) -> Result<()> {
    if params.banner {
        akarru()?
    }
    let mut loader = Loader::new();
    for path in params.files.iter() {
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
        loader.add(&module);
        loader.parse(&module);
    }
    Ok(())
}


