use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;


#[test]
fn test_simple_loop() {
    let module = make_module(
        indoc! {r#"
          s = for i = 0, sum = 0 loop
                  if i == 10 then sum
                  else repeat i + 1, sum + i
          "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));

}

#[test]
fn test_rev_loop() {
    let module = make_module(
        indoc! {r#"
          zero? n = n == 0
          rev num =
            for reversed = 0, n = num
            loop
              if zero? n then reversed
              else repeat (reversed * 10 + n % 10), (n // 10)

          palindrome? n = n == rev n
          "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::bool())));
    assert_eq!(decls[1].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int())));
    assert_eq!(decls[2].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::bool())));

}


#[test]
fn test_let_in_loop() {
    let module = make_module(
        indoc! {r#"
          calc n =
            for i = 1, out = 0 loop
                if i == n then out
                else repeat let i' = inc i, let out = i' * 2
          "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int())));

}


