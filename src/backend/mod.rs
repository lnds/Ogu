use crate::backend::banner::akarru;
use crate::backend::params::Params;
use anyhow::Result;

use crate::backend::compiler::Compiler;
use crate::codegen::CodeGenerator;
use crate::codegen::transpilers::rust_transpiler::RustTranspiler;
use crate::symbols::scopes::Scope;
use std::{env, fs};

pub(crate) mod banner;
pub(crate) mod errors;
pub(crate) mod params;
pub(crate) mod compiler;


pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
    }
    if params.clean {
        clean_ogu_dir()?;
    }
    let mut backend = Compiler::new(&params);
    backend.run(params.files.to_vec())?;
    if params.dump_symbols {
        backend.dump();
    }
    if params.rust {
        let mut rust_transpiler: Box<dyn CodeGenerator> = Box::new(RustTranspiler::new()?);
        backend.gen_code(&mut rust_transpiler)?
    }
    Ok(())
}


fn clean_ogu_dir() -> Result<()>{
    println!("cleaning ogu generated files...");
    let mut base = env::current_dir()?;
    base.push(".ogu");
    fs::remove_dir_all(base)?;
    Ok(())
}