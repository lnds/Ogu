pub(crate) mod module;
pub(crate) mod symbols;
pub(crate) mod types;

#[cfg(test)]
mod tests {
    use crate::backend::compiler::default_sym_table;
    use crate::backend::modules::module::Module;
    use crate::backend::modules::types::basic_type::BasicType;
    use crate::backend::modules::types::func_type::FuncType;
    use crate::backend::modules::types::trait_type::{TRAIT_EQ, TRAIT_NUM, TRAIT_ORD};
    use crate::backend::scopes::types::TypeClone;
    use crate::backend::scopes::Scope;
    use crate::lexer::Lexer;
    use crate::parser::ast::module::ModuleAst;
    use crate::parser::Parser;
    use indoc::indoc;

    fn test_module(source: &str, sym_table: Box<dyn Scope>) -> Option<Module> {
        let mut lexer = Lexer::from(source);
        let lex = lexer.scan();
        assert!(lex.is_ok());
        let (tokens, strs) = lex.unwrap();
        let parser = Parser::new(tokens, strs);
        assert!(parser.is_ok());
        if let Ok(parser) = parser {
            let module_ast = ModuleAst::parse(&parser, None, 0);
            if !module_ast.is_ok() {
                println!("ERR: {:?}", module_ast);
            }

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
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(None, BasicType::unit())
        );
    }

    #[test]
    fn test_arithmetic() {
        let module = test_module(
            indoc! {r#"
            a = 10
            a1 = a + 1
            a2 = a * a1
            a3 = a1 - a2
            a4 = a3 // a2
            a5 = a3 / a2
            a6 = a4 % 10.0
            a7 = a5 ^ a
            a8 = (a1 + a2) * (a3 - a4) / (a5 % a6) ^ a
            main () = println! "a7 = {}" a7
            "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
        assert_eq!(decls[3].get_type(), Some(BasicType::int()));
        assert_eq!(decls[4].get_type(), Some(BasicType::int()));
        assert_eq!(decls[5].get_type(), Some(BasicType::float()));
        assert_eq!(decls[6].get_type(), Some(BasicType::float()));
        assert_eq!(decls[7].get_type(), Some(BasicType::float()));
        assert_eq!(decls[8].get_type(), Some(BasicType::float()));
        assert_eq!(
            decls[9].get_type(),
            FuncType::new_opt(None, BasicType::unit())
        );
    }

    #[test]
    fn test_let() {
        let module = test_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h =
                let bmi = w / h ^ 2
                in if bmi <= 18.5 then "You're underweight, you emo, you!"
                   elif bmi <= 25.0 then "You're supposedly normal. Pffft, I bet you're ugly!"
                   elif bmi <= 30.0 then "You're fat! Lose some weight, fatty!"
                   else "You're a whale, congratulations!"
            "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int(), BasicType::int()]), BasicType::static_str())
        );
    }

    #[test]
    fn test_where() {
        let module = test_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h =
                if bmi <= 18.5 then "You're underweight, you emo, you!"
                elif bmi <= 25.0 then "You're supposedly normal. Pffft, I bet you're ugly!"
                elif bmi <= 30.0 then "You're fat! Lose some weight, fatty!"
                else "You're a whale, congratulations!"
                where
                     bmi = w / h ^ 2

            "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(decls[0].get_type(),
                   FuncType::new_opt(Some(vec![BasicType::int(), BasicType::int()]), BasicType::static_str())
        );
    }

    
    #[test]
    fn test_funcs_1() {
        let module = test_module(
            indoc! {r#"
        mul x y = x * y
        m = mul"#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), decls[1].get_type());
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TRAIT_NUM.clone_box()
            )
        );
    }

    #[test]
    fn test_funcs_2() {
        let module = test_module(
            indoc! {r#"
        mul x y = x * y

        fmul x y = (mul x y) / 1.0
        "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TRAIT_NUM.clone_box()
            )
        );
        assert_eq!(
            decls[1].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                BasicType::float()
            )
        );
    }

    #[test]
    fn test_vals_1() {
        let module = test_module(
            indoc! {r#"
        a = 10
        b = (a)
        c () = a
        d = (c)
        "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), decls[1].get_type());
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(
            decls[2].get_type(),
            FuncType::new_opt(None, BasicType::int())
        );
        assert_eq!(decls[3].get_type(), Some(BasicType::int()));
    }

    #[test]
    fn test_args_1() {
        let module = test_module(
            indoc! {r#"
        min x y = if x < y then x else y
        b = (min (min 10 20) 20)
        "#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box()
            )
        );
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
    }

    #[test]
    fn test_if_and_do() {
        let module = test_module(
            indoc! {r#"
        max x y = if x > y then x else y

        max2 x y = if x >= y then x else y

        min x y = if x < y then x else y

        min2 x y = if x <= y then x else y

        p20 = max 10 20

        eq x y = if x == y then "equal" else "distinct"

        main () = do
            if (max $ 10 20) == (max2 10 20) then
                println! "{} {}" (max $ 10 20)  (max2 10 20)
            else
                println!("no")
            println! "{} {}" (min $ 10 20) (min2 10 20)"#},
            default_sym_table(),
        );
        assert!(module.is_some());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box()
            )
        );
        assert_eq!(decls[0].get_type(), decls[1].get_type());
        assert_eq!(
            decls[2].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box()
            )
        );
        assert_eq!(decls[2].get_type(), decls[3].get_type());
        assert_eq!(decls[4].get_type(), Some(BasicType::int()));
        assert_eq!(
            decls[5].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_EQ.clone_box(), TRAIT_EQ.clone_box()]),
                BasicType::static_str()
            )
        );
        assert_eq!(
            decls[6].get_type(),
            FuncType::new_opt(None, BasicType::unit())
        );
    }
}
