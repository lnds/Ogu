use std::io::Result as IOResult;
use structopt::StructOpt;

pub mod backend;
pub mod lexer;

use backend::Params;

fn main() -> IOResult<()> {
    let params = Params::from_args();
    backend::run(params)
}
