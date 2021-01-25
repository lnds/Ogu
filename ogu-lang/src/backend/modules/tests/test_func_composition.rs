use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_UNKNOWN};
use crate::backend::scopes::types::TypeClone;
use crate::backend::modules::types::list_type::ListType;

#[test]
fn test_func_composition_1() {
    let module = make_module(
        indoc! {r#"
             triple n = n * 3
             double n = n * 2
             sixtuple = triple >> double
             six = sixtuple 1
             six' = (triple >> double) 1
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
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[3].get_type(),
        Some(BasicType::int())
    );
}


#[test]
fn test_func_composition_2() {
    let module = make_module(
        indoc! {r#"
             triple n = n * 3
             double n = n * 2
             sixtuple = triple << double
             six = sixtuple 1
             six' = (triple << double) 1
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
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[3].get_type(),
        Some(BasicType::int())
    );
}


#[test]
fn test_pipes_1() {
    let module = make_module(
        indoc! {r#"
             mul (a, b) = a * b
             twelve = (2, 3) |> mul
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
        FuncType::new_opt(Some(vec![TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()])]), TRAIT_NUM.clone_box())
    );
    assert_eq!(
        decls[1].get_type(),
        Some(BasicType::int())
    );
}

#[test]
fn test_pipes_2() {
    let module = make_module(
        indoc! {r#"
             mul (a, b) = a * b
             double n = n * 2.0
             twelve = (2, 3) |> mul |> double
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
        FuncType::new_opt(Some(vec![TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()])]), TRAIT_NUM.clone_box())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::float()]), BasicType::float())
    );
    assert_eq!(
        decls[2].get_type(),
        Some(BasicType::float())
    );
}


#[test]
fn test_pipes_3() {
    let module = make_module(
        indoc! {r#"
             mul (a, b) = a * b
             double n = n * 2
             twelve = (2, 3) |> mul |> double

             union xs ys = xs ++ ys
             to_zero xs = 0

             zero = union [1, 2, 3] [2, 3, 4] |> to_zero

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
        FuncType::new_opt(Some(vec![TupleType::new_box(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()])]), TRAIT_NUM.clone_box())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));

    assert_eq!(
        decls[3].get_type(),
        FuncType::new_opt(Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box()), ListType::new_list(TRAIT_UNKNOWN.clone_box())]), ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );

    assert_eq!(
        decls[4].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::int())
    );

    assert_eq!(decls[5].get_type(), Some(BasicType::int()));
}