use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_UNKNOWN};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

#[test]
fn test_lambda_1() {
    let module = make_module(
        indoc! {r#"
            mul = \x y -> x * y
            ten = mul 2 5
            three = mul 1 3.0
            three' = (\x y -> x * y) 2 4"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box()
        ))
    );
    assert_eq!(decls[1].get_type(), Some(BasicType::int()));
    assert_eq!(decls[2].get_type(), Some(BasicType::float()));
    assert_eq!(decls[3].get_type(), Some(BasicType::int())); // should be int!
}

#[test]
fn test_y_combinator() {
    let module = make_module(
        indoc! {r#"
            y = \f -> (\x -> lazy f (x x)) (\x -> lazy f  (x x))

            factorial _ 0 = 1
            factorial f n = n * f (n - 1)

            fact = y factorial <| 10


            "#},
        default_sym_table(),
    );

    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_UNKNOWN.clone_box()]),
            TRAIT_UNKNOWN.clone_box()
        ))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()),
                BasicType::int()
            ]),
            BasicType::int()
        ))
    );
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));
}
