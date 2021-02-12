use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::types::TypeClone;
use crate::backend::modules::types::list_type::ListType;

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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![BasicType::int(), BasicType::int()]),
            BasicType::static_str(),
        ))
    );
}

#[test]
fn test_let_cons() {
    let module = make_module(
        indoc! {r#"
            tail x = let (x :: xs) = x in xs
            t = tail [1, 2, 3]
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), ListType::new_list(TRAIT_UNKNOWN.clone_box()))));
    assert_eq!(decls[1].get_type(), Some(ListType::new_list(BasicType::int())));
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![BasicType::int(), BasicType::int()]),
            BasicType::static_str(),
        ))
    );
}
