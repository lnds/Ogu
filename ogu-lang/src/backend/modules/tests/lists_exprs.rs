use crate::backend::modules::tests::make_module;
use crate::backend::compiler::default_sym_table;
use indoc::indoc;
use crate::backend::modules::types::list_type::ListType;

#[test]
fn test_simple_list() {
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            empty = []
            a = [1]
            b = ['a', 'b']
            c = ["hello", "world"]
            d = [1, 2, 3]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("TEST DECLS = {:#?}", decls);
    assert_eq!(decls[0].get_type(), Some(ListType::new_empty()))
}

#[test]
fn test_invalid_lists() {
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = [1, 'a']
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = [1, 2.0]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = ["hello", 'a']
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = ["hello", 3]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            -- taken from http://learnyouahaskell.com/syntax-in-functions#pattern-matching
            a = [1, 2, 3N]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
}