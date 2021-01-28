use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;

#[test]
fn test_recursive_1() {
    let module = make_module(
        indoc! {r#"
                siracusa n
                | n == 1 = 4
                | n == 2 = 1
                | n % 2 == 0 = siracusa (n // 2)
                | otherwise = siracusa (n * 3 + 1)"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
}

#[test]
fn test_recursive_2() {
    let module = make_module(
        indoc! {r#"
               ackermann m n
                    | m == 0 = n + 1
                    | n == 0 = recur (m - 1) 1
                    | otherwise  = recur (m - 1) (ackermann m (n - 1))"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            BasicType::int()
        )
    );
}

#[test]
fn test_recursive_3() {
    let module = make_module(
        indoc! {r#"
                factorial 0 = 1
                factorial 1 = 1
                factorial n = n * (recur n - 1)"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
}
