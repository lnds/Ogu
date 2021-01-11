pub(crate) mod module;
pub(crate) mod symbols;
pub(crate) mod types;

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use indoc::indoc;

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

    fn make_module(source: &str, sym_table: Box<dyn Scope>) -> Result<Module> {
        let mut lexer = Lexer::from(source);
        let (tokens, strs) = lexer.scan()?;
        let parser = Parser::new(tokens, strs)?;
        let module_ast = ModuleAst::parse(&parser, None, 0)?;
        println!("AST:\n\t{:#?}", module_ast);
        Module::new(module_ast, sym_table)
    }

    #[test]
    fn test_no_dups() {
        let module = make_module(
            indoc! {"
                a = 1
                a = a * 1"},
            default_sym_table(),
        );
        println!("{:?}", module);
        assert!(module.is_err());

        let module = make_module(
            indoc! {"
            b = let a = 1, a = 2 in a + 1"},
            default_sym_table(),
        );
        println!("{:?}", module);
        assert!(module.is_err());

        let module = make_module(
            indoc! {"
            b = let (a, a) = (1, 1) in a * a"},
            default_sym_table(),
        );
        println!("{:?}", module);
        assert!(module.is_err());

        let module = make_module(
            indoc! {"
                a = 1
                b = let (a, b) = (a, a) in a * b"},
            default_sym_table(),
        );
        println!("{:?}", module);
        assert!(module.is_ok());
    }

    #[test]
    fn test_values() {
        let module = make_module(
            indoc! {"
                a = 1
                b = a * 1
                c = a * b"},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
    }

    #[test]
    fn test_hello() {
        let module = make_module("main () = println! \"hello world\"", default_sym_table());
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(None, BasicType::unit())
        );
    }

    #[test]
    fn test_arithmetic() {
        let module = make_module(
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
        assert!(module.is_ok());
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
        let module = make_module(
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
        println!("module: {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }

    #[test]
    fn test_let_2() {
        let module = make_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = let a = 10 in let a = a * 1.0 in let a = a // 2 in a
            b = let a = 10 in let a = a * 1.0 in let a = a / 2 in a
            "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::float()));
    }

    #[test]
    fn test_where() {
        let module = make_module(
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
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }

    #[test]
    fn test_func_pattern_1() {
        let module = make_module(
            indoc! {r#"
             triple n = n * 3
             siracusa 1 = 4
             siracusa 2 = 1
             siracusa n | n % 2 == 0 = siracusa (n // 2)
             siracusa n = siracusa ((triple n) + 1) "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
        );
    }

    #[test]
    fn test_func_pattern_2() {
        let module = make_module(
            indoc! {r#"
            ackermann 0 n | n >= 0 = n + 1
            ackermann m 0 | m >= 0 = recur (m - 1) 1
            acckermann m n  = recur (m - 1) (ackermann m (n - 1))"#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::int(),
            )
        );
    }

    #[test]
    fn test_func_pattern_strange_case() {
        let module = make_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h | w / h ^ 2 <= skinny = "You're underweight, you emo, you!"
            where
                skinny = 18.5
            str_imc w h | w / h ^ 2 <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
            where
                normal = 25.0
            str_imc w h | w / h ^ 2 <= fat = "You're fat! Lose some weight, fatty!"
            where
                fat = 30.0
            str_imc w h =  "You're a whale, congratulations!""#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }
    #[test]
    fn test_guards_1() {
        let module = make_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h
                | bmi <= skinny = "You're underweight, you emo, you!"
                | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
                | bmi <= fat = "You're fat! Lose some weight, fatty!"
                | otherwise =  "You're a whale, congratulations!"
                where
                     bmi = w / h ^ 2
                     skinny = 18.5
                     normal = 25.0
                     fat = 30.0
            "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }

    #[test]
    fn test_guards2() {
        let module = make_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h
                | bmi <= skinny = "You're underweight, you emo, you!"
                | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
                | bmi <= fat = "You're fat! Lose some weight, fatty!"
                | otherwise =  "You're a whale, congratulations!"
                where
                     bmi = w / h ^ 2
                     (skinny, normal, fat) = (18.5, 25.0, 30.0)
            "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }

    #[test]
    fn test_cond() {
        let module = make_module(
            indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            str_imc w h =
                cond
                  bmi <= 18.5 -> "You're underweight, you emo, you!"
                  bmi <= 25.0 -> "You're supposedly normal. Pffft, I bet you're ugly!"
                  bmi <= 30.0 -> "You're fat! Lose some weight, fatty!"
                  otherwise -> "You're a whale, congratulations!"
                where
                     bmi = w / h ^ 2
            "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::static_str(),
            )
        );
    }

    #[test]
    fn test_case_1() {
        let module = make_module(
            indoc! {r#"
             siracusa n =
                case n of
                    1 -> 4
                    2 -> 1
                    n | n % 2 == 0 -> siracusa (n // 2)
                    _ -> siracusa (n * 3 + 1)"#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
        );
    }

    #[test]
    fn test_case_2() {
        let module = make_module(
            indoc! {r#"
            ackermann m n =
                case (m,n) of
                  (0, n) -> n + 1
                  (m, 0) -> recur (m - 1) 1
                  _ -> recur (m - 1) (recur  m (n - 1))"#},
            default_sym_table(),
        );

        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::int(),
            )
        );
    }

    #[test]
    fn test_func_expr_arg() {
        let module = make_module(
            indoc! {r#"
             triple 0 = 0
             "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("TEST DECLS = {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
        );
    }

    #[test]
    fn test_funcs_1() {
        let module = make_module(
            indoc! {r#"
        mul x y = x * y
        m = mul
        a = m 5 5
        b = m 10.0 5"#},
            default_sym_table(),
        );
        println!("module: {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), decls[1].get_type());
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TRAIT_NUM.clone_box(),
            )
        );
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
        assert_eq!(decls[3].get_type(), Some(BasicType::float()));
    }

    #[test]
    fn test_funcs_2() {
        let module = make_module(
            indoc! {r#"
        mul x y = x * y

        fmul x y = (mul x y) / 1.0
        "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TRAIT_NUM.clone_box(),
            )
        );
        assert_eq!(
            decls[1].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                BasicType::float(),
            )
        );
    }

    #[test]
    fn test_vals_1() {
        let module = make_module(
            indoc! {r#"
        a = 10
        b = (a)
        c () = a
        d = (c)
        "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
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
    fn test_vals_2() {
        let module = make_module(
            indoc! {r#"
        a = 10
        b = 'b'
        c = 10.0
        d = "str"
        e = ()
        f = #2020-01-03
        g = #/(a|b)*/#
        "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::char()));
        assert_eq!(decls[2].get_type(), Some(BasicType::float()));
        assert_eq!(decls[3].get_type(), Some(BasicType::static_str()));
        assert_eq!(decls[4].get_type(), Some(BasicType::unit()));
        assert_eq!(decls[5].get_type(), Some(BasicType::date()));
        assert_eq!(decls[6].get_type(), Some(BasicType::regexp()));
    }

    #[test]
    fn test_args_1() {
        let module = make_module(
            indoc! {r#"
        min x y = if x < y then x else y
        b = (min (min 10 20) 20)
        "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box(),
            )
        );
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
    }

    #[test]
    fn test_logical() {
        let module = make_module(
            indoc! {r#"
        a = 10
        b = 20
        c = if a == b || a <= b then a else b
        d = a == b
        e = a <= b
        f = a >= b
        de_morgan_1 = (not (e || f)) == ((not e) && (not f))
        de_morgan_2 = (not (e && f)) == ((not e) || (not f))
        "#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(decls[0].get_type(), Some(BasicType::int()));
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::int()));
        assert_eq!(decls[3].get_type(), Some(BasicType::bool()));
        assert_eq!(decls[4].get_type(), Some(BasicType::bool()));
        assert_eq!(decls[5].get_type(), Some(BasicType::bool()));
        assert_eq!(decls[6].get_type(), Some(BasicType::bool()));
        assert_eq!(decls[7].get_type(), Some(BasicType::bool()));
    }

    #[test]
    fn test_if_and_do() {
        let module = make_module(
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
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box(),
            )
        );
        assert_eq!(decls[0].get_type(), decls[1].get_type());
        assert_eq!(
            decls[2].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
                TRAIT_ORD.clone_box(),
            )
        );
        assert_eq!(decls[2].get_type(), decls[3].get_type());
        assert_eq!(decls[4].get_type(), Some(BasicType::int()));
        assert_eq!(
            decls[5].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_EQ.clone_box(), TRAIT_EQ.clone_box()]),
                BasicType::static_str(),
            )
        );
        assert_eq!(
            decls[6].get_type(),
            FuncType::new_opt(None, BasicType::unit())
        );
    }

    #[test]
    fn test_recursive_1() {
        let module = make_module(
            indoc! {r#"
                siracusa n
                | n == 1 = 4
                | n == 2 = 1
                | n % 2 == 0 = siracusa (n // 2)
                | otherwise = siracusa (n * 3 + 1)"#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
        );
    }

    #[test]
    fn test_recursive_2() {
        let module = make_module(
            indoc! {r#"
               ackermann m n
                    | m == 0 = n + 1
                    | n == 0 = recur (m - 1) 1
                    | otherwise  = recur (m - 1) (ackermann m (n - 1))"#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::int(), BasicType::int()]),
                BasicType::int()
            )
        );
    }

    #[test]
    fn test_recursive_3() {
        let module = make_module(
            indoc! {r#"
                factorial 0 = 1
                factorial 1 = 1
                factorial n = n * (recur n - 1)"#},
            default_sym_table(),
        );
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
        );
    }

    #[test]
    fn test_proto_1() {
        let module = make_module(
            indoc! {r#"
                factorial : Int -> Int
                factorial 0 = 1
                factorial 1 = 1
                factorial n = n * (recur n - 1)"#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
        );
    }

    #[test]
    fn test_proto_2() {
        let module = make_module(
            indoc! {r#"
            ackermann : UInt -> UInt -> UInt
            ackermann 0 n = n + 1
            ackermann m 0 = recur (m - 1) 1
            acckermann m n = recur (m - 1) (ackermann m (n - 1))"#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
        let module = module.unwrap();
        let decls = module.get_decls();
        println!("DECLS: {:#?}", decls);
        assert_eq!(
            decls[0].get_type(),
            FuncType::new_opt(
                Some(vec![BasicType::uint(), BasicType::uint()]),
                BasicType::uint()
            )
        );
    }

    #[test]
    fn test_proto_3() {
        let module = make_module(
            indoc! {r#"
                factorial : Int -> Int
                factorial 0 0 = 1
                factorial 1 2 = 1
                factorial n _ = n * (recur n - 1)"#},
            default_sym_table(),
        );
        assert!(module.is_err());
    }

    #[test]
    fn test_lambda_1() {
        let module = make_module(
            indoc! {r#"
            mul = \x y -> x * y
            ten = mul 2 5
            three = mul 1 3.0 "#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
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
        assert_eq!(decls[1].get_type(), Some(BasicType::int()));
        assert_eq!(decls[2].get_type(), Some(BasicType::float()));
    }

    #[test]
    fn test_curry_1() {
        let module = make_module(
            indoc! {r#"
            mul x y = x * y
            double x = mul 2 x
            double' = mul 2
            ten = double' 5
            add x y z = x + y + z
            add2 x y = add 0"#},
            default_sym_table(),
        );
        println!("module = {:?}", module);
        assert!(module.is_ok());
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
            FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())
        );
        assert_eq!(
            decls[2].get_type(),
            FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())
        );
        assert_eq!(decls[3].get_type(), Some(BasicType::int()));
        assert_eq!(
            decls[4].get_type(),
            FuncType::new_opt(
                Some(vec![
                    TRAIT_NUM.clone_box(),
                    TRAIT_NUM.clone_box(),
                    TRAIT_NUM.clone_box()
                ]),
                TRAIT_NUM.clone_box()
            )
        );
        assert_eq!(
            decls[5].get_type(),
            FuncType::new_opt(
                Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TRAIT_NUM.clone_box()
            )
        );
    }
}
