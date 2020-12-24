use crate::backend::banner::akarru;
use crate::backend::params::Params;
use crate::lexer::tokens::Token;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use crate::symbols::module::Module;
use crate::symbols::scopes::Scope;
use crate::symbols::sym_table::SymbolTable;
use crate::symbols::types::Type;
use crate::symbols::{Macro, Symbol};
use anyhow::Result;
use std::path::PathBuf;
use crate::codegen::transpilers::rust_transpiler::RustTranspiler;
use crate::codegen::CodeGenerator;

pub mod banner;
pub mod errors;
pub mod params;

pub(crate) struct Compiler {
    show_tokens: bool,
    show_ast: bool,
    ogu_scope: Box<dyn Scope>,
    scopes: Vec<Box<dyn Scope>>,
}

impl Compiler {
    pub(crate) fn new(params: &Params) -> Self {
        let mut symbol_table = Box::new(SymbolTable::new("_ogu"));
        symbol_table.define(Macro::make("println!", Type::Unit, 1));
        symbol_table.define(Macro::make("print!", Type::Unit, 1));
        Compiler {
            show_tokens: params.tokens,
            show_ast: params.print,
            ogu_scope: symbol_table,
            scopes: vec![],
        }
    }
}

impl Scope for Compiler {
    fn scope_name(&self) -> &str {
        "_compiler"
    }

    fn define(&mut self, _: Symbol) -> Option<Symbol> {
        unimplemented!()
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        self.ogu_scope.resolve(name)
    }

    fn dump(&self) {
        for scope in self.scopes.iter() {
            scope.dump();
        }
    }

    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        for scope in self.scopes.iter() {
            scope.gen_code(generator)?;
        }
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn run(&mut self, files: Vec<PathBuf>) -> Result<()> {
        for path in files.iter() {
            let scope = self.compile(path.clone())?;
            self.scopes.push(scope);
        }
        Ok(())
    }

    fn compile(&self, path: PathBuf) -> Result<Box<dyn Scope>> {
        let mut lexer = Lexer::new(&path)?;
        println!("parsing {:?}", &path);
        let (tokens, strs) = lexer.scan()?;
        if self.show_tokens {
            let syms: Vec<Token> = tokens.iter().map(|t| t.token).collect();
            println!("TOKENS = {:?}", syms);
        }
        let parser = Parser::new(tokens.to_owned(), strs.to_vec())?;
        let module = ModuleAst::parse(&parser, path, 0)?;
        if self.show_ast {
            println!("AST = {:#?}", module);
        }
        Ok(Box::new(Module::new(&module, self)?))
    }

}

pub fn run(params: Params) -> Result<()> {
    if params.banner {
        akarru()?;
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
