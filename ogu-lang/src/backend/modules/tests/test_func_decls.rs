use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

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
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        )
    );
    assert_eq!(decls[1].get_type(), decls[0].get_type());
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
fn test_args_1() {
    let module = make_module(
        indoc! {r#"
        min x y = if x < y then x else y
        b = (min 10 20)
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
fn test_args_2() {
    let module = make_module(
        indoc! {r#"
        min (x, y) = if x < y then x else y
        b = min (10, 20)
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
            Some(vec![TupleType::new_box(vec![
                TRAIT_ORD.clone_box(),
                TRAIT_ORD.clone_box()
            ])]),
            TRAIT_ORD.clone_box()
        )
    );
    assert_eq!(decls[1].get_type(), Some(BasicType::int()));
}

#[test]
fn test_args_3() {
    let module = make_module(
        indoc! {r#"
        swap (x, y) = (y, x)
        b = swap (10, 20)
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
            Some(vec![TupleType::new_box(vec![
                TRAIT_UNKNOWN.clone_box(),
                TRAIT_UNKNOWN.clone_box()
            ])]),
            TupleType::new_box(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()])
        )
    );
    assert_eq!(
        decls[1].get_type(),
        Some(TupleType::new_box(vec![BasicType::int(), BasicType::int()]))
    );
}

#[test]
fn test_args_4() {
    let module = make_module(
        indoc! {r#"
        sum (a, b) (c, d) = (a + c, b + d )
        b = sum (10, 10) (20, 20)
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
            Some(vec![
                TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
                TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()])
            ]),
            TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()])
        )
    );
    assert_eq!(
        decls[1].get_type(),
        Some(TupleType::new_box(vec![BasicType::int(), BasicType::int()]))
    );
}


#[test]
fn test_erroneus_args() {
    let module = make_module(
        indoc! {r#"
             double n = n * 2
             twelve = (2, 3) |> double
             "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_err());
}