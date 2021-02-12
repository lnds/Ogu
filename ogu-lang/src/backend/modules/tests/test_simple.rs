use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

#[test]
fn test_hello() {
    let module = make_module("main () = println! \"hello world\"", default_sym_table());
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(None, BasicType::unit()))
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
        Some(FuncType::new_func_type(None, BasicType::unit()))
    );
}

#[test]
fn test_dollar() {
    let module = make_module(
        indoc! {r#"
            sort [] = []
            sort x :: xs = smaller ++ (x :: bigger)
                where
                    smaller = sort [a | a <- xs, a <= x]
                    bigger  = sort [a | a <- xs, a > x]

            a = sort "julie" ++ "moronuki"
            b = sort $ "julie" ++ "moronuki"
            c = sort ("julie" ++ "moronuki")

            head [] = error! "head of empty list"
            head x :: xs = x

            d = head << sort $ "julie"
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_ORD.clone_box())]),
            ListType::new_list(TRAIT_ORD.clone_box())
        ))
    );
    assert_eq!(decls[1].get_type(), Some(BasicType::static_str()));
    assert_eq!(decls[2].get_type(), Some(BasicType::static_str()));
    assert_eq!(decls[3].get_type(), Some(BasicType::static_str()));
    assert_eq!(
        decls[4].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            TRAIT_UNKNOWN.clone_box()
        ))
    );
    assert_eq!(decls[5].get_type(), Some(BasicType::char()));
}
