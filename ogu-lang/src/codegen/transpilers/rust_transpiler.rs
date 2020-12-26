use std::path::PathBuf;
use std::{env, fs};
use crate::codegen::CodeGenerator;
use anyhow::{Result, Error};
use crate::symbols::module::Module;
use crate::codegen::transpilers::Transpiler;
use std::fs::File;
use crate::backend::errors::OguError::CodeGenError;
use std::io::Write;
use crate::symbols::{Symbol, SymbolValue};
use crate::symbols::SymbolValue::{FuncDecl, Ref, Str, FuncCall};

pub struct RustTranspiler {
    outputdir: PathBuf,
    current_file: Option<PathBuf>,
}

impl RustTranspiler {
    pub fn new() -> Result<Self> {
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
        writeln!(file, "// ogu generated code from module: {}.ogu", module.get_name())?;
        for sym in module.get_symbols().iter() {
            self.dump_symbol_code(&mut file, sym)?;
        }
        Ok(())
    }

    fn dump_symbol_code(&self, file: &mut File, symbol: &Symbol) -> Result<()>{
        let name = symbol.get_name();
        match symbol.get_value() {
            FuncDecl(args, expr) => {
                write!(file, "fn {}", name)?;
                writeln!(file, "({}) {{", self.dump_args( &args))?;
                writeln!(file, "\t{}", self.dump_expr(&expr))?;
                writeln!(file, "}}")?;
            }
            sym => {
                write!(file, "/* {:#?} */", sym)?;
            },
        }
        Ok(())
    }

    fn dump_args(&self,  val: &SymbolValue) -> String {
        match val {
            SymbolValue::Unit => String::new(),
            s => format!("{:?}", s)
        }
    }

    fn dump_expr(&self, expr: &SymbolValue) -> String {
        match expr {
            Ref(id) => id.to_string(),
            Str(s) => format!("\"{}\"", s.to_string()),
            FuncCall(f, a ) =>
                format!("{} ({})", self.dump_expr(&f), self.dump_expr(&a)),
            s => format!("/* {:?} */", s)
        }
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
