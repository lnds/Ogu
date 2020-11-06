use std::path::PathBuf;
use std::io::Result as IOResult;
use structopt::StructOpt;

const HELP_GREETINGS :&str = "\n\t\tOLA AMIKO MIO DE MI\n\n";

#[derive(Debug, StructOpt)]
#[structopt(name = "ogu", about = HELP_GREETINGS)]
pub struct Params {

    /// shows akarrú banner
    #[structopt(short, long)]
    banner: bool,

    /// print AST
    #[structopt(short, long)]
    print: bool,

    #[structopt(parse(from_os_str), help="ogu modules...")]
    files: Vec<PathBuf>,

    #[structopt(name="ARGS", required_if("has_args", "true"), last=true, help="args for main module")]
    args: Vec<String>,
}

pub fn run(params: Params) -> IOResult<()> {
    println!("run {:?}", params);
    Ok(())
}
