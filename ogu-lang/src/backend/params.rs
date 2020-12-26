use std::path::PathBuf;
use structopt::StructOpt;

const HELP_GREETINGS: &str = "\n\t\tOLA AMIKO MIO DE MI\n\n";

#[derive(Debug, StructOpt)]
#[structopt(name = "ogu", about = HELP_GREETINGS)]
pub struct Params {
    /// shows akarrú banner
    #[structopt(short, long)]
    pub banner: bool,

    #[structopt(short, long)]
    pub clean : bool,

    /// print TOKENS
    #[structopt(short, long)]
    pub tokens: bool,

    /// print AST
    #[structopt(short, long)]
    pub print: bool,

    /// dump symbol table
    #[structopt(short, long)]
    pub dump_symbols: bool,

    /// emit rust code
    #[structopt(short, long)]
    pub rust: bool,


    #[structopt(parse(from_os_str), help = "ogu modules...", required_if("clean", "false"))]
    pub files: Vec<PathBuf>,

    #[structopt(
        name = "ARGS",
        required_if("has_args", "true"),
        last = true,
        help = "args for main module"
    )]
    pub args: Vec<String>,
}
