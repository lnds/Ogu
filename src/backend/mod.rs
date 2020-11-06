use std::path::PathBuf;
use std::io::{Result as IOResult, Error, ErrorKind};
use structopt::StructOpt;
use figlet_rs::FIGfont;

const HELP_GREETINGS: &str = "\n\t\tOLA AMIKO MIO DE MI\n\n";
const VERSION: &str = "Ogu compiler version 0.3.0 (Ñeclito)";

#[derive(Debug, StructOpt)]
#[structopt(name = "ogu", about = HELP_GREETINGS)]
pub struct Params {
    /// shows akarrú banner
    #[structopt(short, long)]
    banner: bool,

    /// print AST
    #[structopt(short, long)]
    print: bool,

    #[structopt(parse(from_os_str), help = "ogu modules...")]
    files: Vec<PathBuf>,

    #[structopt(name = "ARGS", required_if("has_args", "true"), last = true, help = "args for main module")]
    args: Vec<String>,
}

pub fn run(params: Params) -> IOResult<()> {
    if params.banner {
        akarru()
    } else {
        println!("run {:?}", params);
        Ok(())
    }
}


fn akarru() -> IOResult<()> {
    let standard_font = FIGfont::standand().map_err(|s| Error::new(ErrorKind::Other, s))?;
    let figure = standard_font.convert("Akarrú").ok_or_else(|| Error::new(ErrorKind::Other, "no banner"))?;
    println!("{}", figure);
    println!("{}", VERSION);
    Ok(())
}