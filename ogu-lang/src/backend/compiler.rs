use crate::backend::params::Params;
use crate::codegen::CodeGenerator;
use crate::lexer::tokens::Lexeme;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use crate::symbols::macros::MacroSym;
use crate::symbols::module::Module;
use crate::symbols::scopes::Scope;
use crate::symbols::sym_table::SymbolTable;
use crate::symbols::Symbol;
use crate::types::basic::BasicType;
use anyhow::Result;
use std::path::PathBuf;
use crate::symbols::decls::types::TypeAliasSym;

#[derive(Clone)]
pub struct Compiler {
    show_tokens: bool,
    show_ast: bool,
    ogu_scope: Box<dyn Scope>,
    scopes: Vec<Box<dyn Scope>>,
}

impl Compiler {
    pub fn new(params: &Params) -> Box<Self> {
        let mut symbol_table = Box::new(SymbolTable::new("_ogu"));
        symbol_table.define(MacroSym::new("println!", BasicType::Unit));
        symbol_table.define(MacroSym::new("print!", BasicType::Unit));
        symbol_table.define(TypeAliasSym::new("Int", BasicType::primitive("i64")));
        Box::new(Compiler {
            show_tokens: params.tokens,
            show_ast: params.print,
            ogu_scope: symbol_table,
            scopes: vec![],
        })
    }
}

impl Scope for Compiler {
    fn scope_name(&self) -> &str {
        "_compiler"
    }

    fn define(&mut self, sym: Box<dyn Symbol>) -> Option<Box<dyn Symbol>> {
        self.ogu_scope.define(sym)
    }

    fn resolve(&self, name: &str) -> Option<Box<dyn Symbol>> {
        self.ogu_scope.resolve(name)
    }

    fn gen_code(&self, generator: &mut Box<dyn CodeGenerator>) -> Result<()> {
        for scope in self.scopes.iter() {
            scope.gen_code(generator)?;
        }
        Ok(())
    }

    fn get_symbols(&self) -> Vec<Box<dyn Symbol>> {
        let mut result = vec![];
        result.append(&mut self.ogu_scope.get_symbols());
        for scope in self.scopes.iter() {
            result.append(&mut scope.get_symbols());
        }
        result
    }
}

impl Compiler {
    pub fn run(&mut self, files: Vec<PathBuf>) -> Result<()> {
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
            let syms: Vec<Lexeme> = tokens.iter().map(|t| t.lexeme).collect();
            println!("TOKENS = {:?}", syms);
        }
        let parser = Parser::new(tokens.to_owned(), strs.to_vec())?;
        let module = ModuleAst::parse(&parser, path, 0)?;
        if self.show_ast {
            println!("AST = {:#?}", module);
        }
        let mut module = Box::new(Module::new(&module, Box::new(self.clone()))?);
        module.set_symbols(module.solve_symbols_types()?);
        Ok(module)
    }

    /// dumps symbols
    pub fn dump(&self) {
        println!("Symbols:");
        println!("{:#?}", self.ogu_scope);
        for scope in self.scopes.iter() {
            println!("{:#?}", scope)
        }
    }
}
