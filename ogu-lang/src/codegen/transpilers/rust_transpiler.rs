use crate::backend::errors::OguError::CodeGenError;
use crate::codegen::transpilers::{Formatter, Transpiler};
use crate::codegen::CodeGenerator;
use crate::symbols::scopes::Scope;
use crate::types::Type;
use anyhow::{Error, Result};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::{env, fs};
use std::ops::Deref;

pub struct RustTranspiler {
    outputdir: PathBuf,
    current_file: Option<PathBuf>,
    formatter: Box<dyn Formatter>,
}

impl RustTranspiler {
    pub fn new() -> Result<Self> {
        Ok(RustTranspiler {
            outputdir: RustTranspiler::det_outputdir()?,
            current_file: None,
            formatter: RustFormatter::new(),
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
    fn process(&mut self, module: &dyn Scope) -> Result<()> {
        let path = self.get_path(module.scope_name().to_lowercase())?;
        self.set_output_path(&path)?;
        self.dump(module)
    }
}

impl RustTranspiler {
    fn dump_code(&self, path: &PathBuf, module: &dyn Scope) -> Result<()> {
        let mut file = File::create(path)?;
        writeln!(
            file,
            "// ogu generated code from module: {}.ogu",
            module.scope_name()
        )?;
        for sym in module.get_symbols().iter().map(|s| s.get_symbol_writer()) {
            sym.write_symbol(self.formatter.deref(), &mut file)?;
        }
        Ok(())
    }
}

impl Transpiler for RustTranspiler {
    fn set_output_path(&mut self, path: &PathBuf) -> Result<()> {
        self.current_file = Some(path.clone());
        Ok(())
    }

    fn dump(&mut self, module: &dyn Scope) -> Result<()> {
        match &self.current_file {
            None => Err(Error::new(CodeGenError).context(format!(
                "no output file for module: {}",
                module.scope_name()
            ))),
            Some(path) => self.dump_code(path, module),
        }
    }
}

struct RustFormatter {}

impl RustFormatter {
    fn new() -> Box<Self> {
        Box::new(RustFormatter {})
    }
}

impl Formatter for RustFormatter {
    fn format_func_header(&self, name: String, args: String, ty: String) -> String {
        format!("fn {} ({}) -> {}", name, args, ty)
    }

    fn format_type(&self, ty: Box<dyn Type>) -> String {
        ty.get_name()
    }

    fn format_id(&self, id: &str) -> String {
        id.to_string()
    }

    fn format_str(&self, s: &str) -> String {
        format!("\"{}\"", s)
    }

    fn format_int(&self, s: &str) -> String {
        s.to_string()
    }

    fn format_func_call(&self, f: &str, args: &str) -> String{
        format!("{} ({})", f, args)
    }

    fn format_const_decl_header(&self, name: &str, ty: &str) -> String {
        format!("const {} : {} = ", name, ty)
    }
}
