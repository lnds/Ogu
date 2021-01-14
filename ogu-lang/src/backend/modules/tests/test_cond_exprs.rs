use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::{TRAIT_EQ, TRAIT_ORD};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

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
