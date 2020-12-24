use crate::backend::params::Params;
use anyhow::Result;
use structopt::StructOpt;

pub(crate) mod backend;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod symbols;
pub(crate) mod codegen;

fn main() -> Result<()> {
    let params = Params::from_args();
    backend::run(params)
}
