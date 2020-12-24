use std::path::PathBuf;
use std::{env, fs};
use crate::codegen::CodeGenerator;
use anyhow::{Result, Error};
use crate::symbols::module::Module;
use crate::codegen::transpilers::Transpiler;
use std::fs::File;
use crate::backend::errors::OguError::CodeGenError;
use std::io::Write;

pub(crate) struct RustTranspiler {
    outputdir: PathBuf,
    current_file: Option<PathBuf>,
}

impl RustTranspiler {
    pub(crate) fn new() -> Result<Self> {
        Ok(RustTranspiler {
            outputdir: RustTranspiler::det_outputdir()?,
            current_file: None,
        })
    }

    fn get_path(&self, module_name: String) -> Result<PathBuf> {
        let mut result = self.outputdir.clone();
        result.push(module_name);
        result.set_extension("rs");
        Ok(result)
    }

    fn det_outputdir() -> Result<PathBuf> {
        let mut base = env::current_dir()?;
        base.push(".ogu/target/rust");
        if !base.exists() {
            fs::create_dir_all(&base)?;
        }
        Ok(base)
    }
}

impl CodeGenerator for RustTranspiler {
    fn process(&mut self, module: &Module) -> Result<()> {
        let path = self.get_path(module.get_name().to_lowercase())?;
        self.set_output_path(&path)?;
        self.dump(module)
    }
}


impl RustTranspiler {
    fn dump_code(&self, path: &PathBuf, module: &Module) -> Result<()> {
        let mut file = File::create(path)?;
        write!(file, "// ogu generated code from module: {}.ogu\n", module.get_name());
        Ok(())
    }
}

impl Transpiler for RustTranspiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()> {
        self.current_file = Some(path.clone());
        Ok(())
    }

    fn dump(&mut self, module: &Module) -> Result<()> {
        match &self.current_file {
            None => Err(Error::new(CodeGenError(format!("no output file for module: {}", module.get_name())))),
            Some(path) => self.dump_code(path, module)
        }
    }
}
