use crate::backend::params::Params;
use crate::lexer::tokens::Lexeme;
use crate::lexer::Lexer;
use crate::parser::ast::module::ModuleAst;
use crate::parser::Parser;
use anyhow::Result;
use std::path::PathBuf;
use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::sym_table::SymbolTable;

#[derive(Clone)]
pub struct Compiler {
    ogu_scope: Box<dyn Scope>,
}

impl Compiler {

    pub fn new(params: &Params) -> Self {
        let mut symbol_table = SymbolTable::new("_ogu", None);
        //symbol_table.define(MacroSym::new("println!", BasicType::Unit));
        //symbol_table.define(MacroSym::new("print!", BasicType::Unit));
        //symbol_table.define(TypeAliasSym::new("Int", BasicType::primitive("i64")));
        Compiler {
            ogu_scope: symbol_table,
        }
    }

    pub fn run(&self, path: PathBuf, show_tokens: bool, show_ast: bool, dump: bool) -> Result<()> {
        let mut lexer = Lexer::new(&path)?;
        println!("parsing {:?}", &path);
        let (tokens, strs) = lexer.scan()?;
        if show_tokens {
            let syms: Vec<Lexeme> = tokens.iter().map(|t| t.lexeme).collect();
            println!("TOKENS = {:?}", syms);
        }
        let parser = Parser::new(tokens.to_owned(), strs.to_vec())?;
        let module = ModuleAst::parse(&parser, path, 0)?;
        if show_ast {
            println!("AST = {:#?}", module);
        }
        //let scope = module.into();
        if dump {

        }
        Ok(())
    }

    /*
    /// dumps symbols
    pub fn dump(&self) {
        println!("Symbols:");
        println!("{:#?}", self.ogu_scope);
        for scope in self.scopes.iter() {
            println!("{:#?}", scope)
        }
    }

     */
}
