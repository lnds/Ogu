use anyhow::Result;
use ogu_lang::backend::banner::akarru;
use ogu_lang::backend::clean_ogu_dir;
use ogu_lang::backend::compiler::Compiler;
use ogu_lang::backend::params::Params;
use ogu_lang::codegen::transpilers::rust_transpiler::RustTranspiler;
use ogu_lang::codegen::CodeGenerator;
use ogu_lang::symbols::scopes::Scope;
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
    let mut compiler = Compiler::new(&params);
    compiler.run(params.file, params.tokens, params.print,params.dump_symbols)?;
    Ok(())
}
