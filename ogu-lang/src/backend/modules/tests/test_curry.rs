use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

#[test]
fn test_curry_1() {
    let module = make_module(
        indoc! {r#"
            mul x y = x * y
            double x = mul 2 x
            double' = mul 2
            ten = double' 5
            add x y z = x + y + z
            add2 x y = add 0
            tenf = double' 5"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box()
        )
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::int())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::int())
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
            BasicType::int()
        )
    );
    assert_eq!(decls[6].get_type(), Some(BasicType::int()));
}
