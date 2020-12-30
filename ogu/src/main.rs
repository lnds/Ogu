use anyhow::Result;
use ogu_lang::backend::banner::akarru;
use ogu_lang::backend::clean_ogu_dir;
use ogu_lang::backend::compiler::compile;
use ogu_lang::backend::params::Params;
use structopt::StructOpt;

fn main() -> Result<()> {
    let params = Params::from_args();
    run(params)
}

pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
    }
    if params.clean {
        clean_ogu_dir()?;
    }
    compile(
        params.file,
        params.tokens,
        params.print,
        params.dump_symbols,
    )?;
    Ok(())
}
