use std::io::{Result as IOResult};
use structopt::StructOpt;

mod backend;

use backend::Params;


fn main() -> IOResult<()> {
    let params = Params::from_args();
    backend::run(params)
}

