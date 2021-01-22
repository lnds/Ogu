use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

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
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int(),)
    );
    assert_eq!(
        decls[3].get_type(),
        Some(BasicType::int())
    );
}
