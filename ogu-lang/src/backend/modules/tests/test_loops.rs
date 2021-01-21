use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;
use crate::backend::modules::types::tuple_type::TupleType;

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

}
