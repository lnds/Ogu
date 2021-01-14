use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;

#[test]
fn test_proto_1() {
    let module = make_module(
        indoc! {r#"
                factorial : Int -> Int
                factorial 0 = 1
                factorial 1 = 1
                factorial n = n * (recur n - 1)"#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
}

#[test]
fn test_proto_2() {
    let module = make_module(
        indoc! {r#"
            ackermann : UInt -> UInt -> UInt
            ackermann 0 n = n + 1
            ackermann m 0 = recur (m - 1) 1
            acckermann m n = recur (m - 1) (ackermann m (n - 1))"#},
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
            Some(vec![BasicType::uint(), BasicType::uint()]),
            BasicType::uint()
        )
    );
}

#[test]
fn test_proto_3() {
    let module = make_module(
        indoc! {r#"
                factorial : Int -> Int
                factorial 0 0 = 1
                factorial 1 2 = 1
                factorial n _ = n * (recur n - 1)"#},
        default_sym_table(),
    );
    assert!(module.is_err());
}
