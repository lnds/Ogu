pub(crate) mod module;
pub(crate) mod symbols;
pub(crate) mod types;

#[cfg(test)]
mod tests {
    use crate::backend::compiler::default_sym_table;
    use crate::backend::modules::module::Module;
    use crate::backend::modules::types::basic_type::BasicType;
    use crate::lexer::Lexer;
    use crate::parser::ast::module::ModuleAst;
    use crate::parser::Parser;
    use indoc::indoc;
    use crate::backend::modules::types::trait_type::TraitType;
    use crate::backend::scopes::Scope;
    use crate::backend::modules::types::func_type::FuncType;

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
    fn test_values() {
        let module = test_module(
            indoc! {"
                a = 1
                b = a * 1
                c = a * b"},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
    }

    #[test]
    fn test_hello() {
        let module = test_module("main () = println! \"hello world\"", default_sym_table());
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        assert_eq!(decls[0].get_type(), FuncType::make_pair( FuncType::make_box_type(BasicType::unit()), FuncType::make_box_type(BasicType::unit())));

    }


    #[test]
    fn test_arithmetic() {
        let module = test_module(indoc!{r#"
            a = 10
            a1 = a + 1
            a2 = a * a1
            a3 = a1 - a2
            a4 = a3 / a2
            a5 = a4 % 10
            a6 = a5 ^ a
            a7 = (a1 + a2) * (a3 - a4) / (a5 % a6) ^ a
            main () = println! "a7 = {}" a7
            "#},
            default_sym_table());
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
        assert_eq!(decls[3].get_type(), Some(BasicType::int()));
        assert_eq!(decls[4].get_type(), Some(BasicType::int()));
        assert_eq!(decls[5].get_type(), Some(BasicType::int()));
        assert_eq!(decls[6].get_type(), Some(BasicType::int()));
        assert_eq!(decls[7].get_type(), Some(BasicType::int()));
        assert_eq!(decls[8].get_type(), FuncType::make_pair( FuncType::make_box_type(BasicType::unit()), FuncType::make_box_type(BasicType::unit())));

    }

    #[test]
    fn test_if_and_do() {
        let module = test_module(indoc!{r#"
        max x y = if x > y then x else y

        --max2 x y = if x >= y then x else y

        -- min x y = if x < y then x else y

        -- min2 x y = if x <= y then x else y

        main () =
           println! "{} {}" (max $ 10 20)  (max2 10 20)
        -- println! "{} {}" (min $ 10 20) (min2 10 20)"#},
            default_sym_table());
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), FuncType::make_pair( FuncType::make_pair_box(FuncType::make_box_type(TraitType::new("PartialOrd")), FuncType::make_box_type(TraitType::new("PartialOrd"))), FuncType::make_box_type(TraitType::new("PartialOrd"))));
        //assert_eq!(decls[1].get_type(), Some(TraitType::new("PartialOrd")));
        //assert_eq!(decls[2].get_type(), Some(TraitType::new("PartialOrd")));
        //assert_eq!(decls[3].get_type(), Some(TraitType::new("PartialOrd")));
        //assert_eq!(decls[5].get_type(), Some(BasicType::unit()));
    }
}
