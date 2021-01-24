use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_UNKNOWN, TRAIT_NUM};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;


#[test]
fn test_euler_1() {
    let module = make_module(
        indoc! {r#"
            -- Resuelve problema 1 del proyecto Euler
            union [] [] = []
            union xs [] = xs
            union [] ys = ys
            union (x :: xs) ys = x :: union xs ys

            sum [] = 0N
            sum x :: xs = x + sum xs

            result = union [3, 6 .. 999] [5, 10 .. 999] |> sum"#},
        default_sym_table(),
    );
    //println!("module = {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box()), ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(
            Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
           TRAIT_NUM.clone_box())
    );
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));
}
