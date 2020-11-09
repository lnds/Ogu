use crate::backend::banner::akarru;
use crate::lexer::Lexer;
use std::fmt::Debug;
use std::io::Result as IOResult;
use std::path::PathBuf;
use structopt::StructOpt;

pub mod banner;

const HELP_GREETINGS: &str = "\n\t\tOLA AMIKO MIO DE MI\n\n";

#[derive(Debug, StructOpt)]
#[structopt(name = "ogu", about = HELP_GREETINGS)]
pub struct Params {
    /// shows akarrú banner
    #[structopt(short, long)]
    banner: bool,

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

pub fn run(params: Params) -> IOResult<()> {
    if params.banner {
        akarru()?
    }
    for file in params.files.iter() {
        run_module(file, &params)?
    }
    Ok(())
}

fn run_module(path: &PathBuf, _params: &Params) -> IOResult<()> {
    let mut lexer = Lexer::new(path)?;
    let tokens = lexer.scan()?;
    println!("TOKENS: {:?}", tokens);
    Ok(())
}
