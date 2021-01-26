use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;

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
            a = let a = 10 in let a = a * 1.0 in let a = a // 2 in a
            b = let a = 10 in let a = a * 1.0 in let a = a / 2 in a
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
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
