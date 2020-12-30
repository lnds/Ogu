use crate::lexer::tokens::Lexeme::Module;
use crate::parser::ast::module::ModuleAst;

pub(crate) mod symbols;
pub(crate) mod types;
pub(crate) mod module;

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::ast::module::ModuleAst;
    use crate::parser::Parser;
    use crate::backend::modules::module::Module;
    use crate::backend::scopes::scopes::Scope;
    use crate::backend::compiler::default_sym_table;
    use crate::backend::modules::symbols::values::ValueSym;
    use crate::backend::modules::types::basic_type::BasicType;

    fn test_module(source: &str, sym_table: Box<dyn Scope>) -> Option<Module> {
        let mut lexer = Lexer::from(source);
        let lex = lexer.scan();
        assert!(lex.is_ok());
        let (tokens, strs) = lex.unwrap();
        let parser = Parser::new(tokens, strs);
        assert!(parser.is_ok());
        if let Ok(parser) = parser {
            let module_ast = ModuleAst::parse(&parser, None, 0);
            assert!(module_ast.is_ok());
            let module = Module::new(module_ast.unwrap(), sym_table);
            assert!(module.is_ok());
            Some(module.unwrap())
        } else {
            None
        }

    }

    #[test]
    fn test_0() {
        let module = test_module("a = 1", default_sym_table());
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        for decl in decls.iter() {
            assert!(decl.get_type().unwrap() ==  BasicType::int());
        }
        println!("{:#?}", module);
    }

    #[test]
    fn test_1() {
        let module = test_module("a = 1\
        b = a + 1", default_sym_table());
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        for decl in decls.iter() {
            assert!(decl.get_type().unwrap() ==  BasicType::int());
        }
        println!("{:#?}", module);
    }
}