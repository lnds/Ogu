use anyhow::Result;
use structopt::StructOpt;
use crate::backend::params::Params;

pub mod backend;
pub mod lexer;
pub mod parser;
mod symbols;


fn main() -> Result<()> {
    let params = Params::from_args();
    backend::run(params)
}
