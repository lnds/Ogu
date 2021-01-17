use crate::backend::modules::tests::make_module;
use crate::backend::compiler::default_sym_table;
use indoc::indoc;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::types::TypeClone;

#[test]
fn test_simple_list() {
    let module = make_module(
        indoc! {r#"
            empty = []
            a = [1]
            b = ['a', 'b']
            c = ["hello", "world"]
            d = [1.0, 2.0, 3.0]
            e = [1N, 2N, 3N]
            f = [0 == 0, 0 != 1, 0 < 1, true, false]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("TEST DECLS = {:#?}", decls);
    assert_eq!(decls[0].get_type(), Some(ListType::new_empty()));
    assert_eq!(decls[1].get_type(), Some(ListType::new_list(BasicType::int())));
    assert_eq!(decls[2].get_type(), Some(ListType::new_list(BasicType::char())));
    assert_eq!(decls[3].get_type(), Some(ListType::new_list(BasicType::static_str())));
    assert_eq!(decls[4].get_type(), Some(ListType::new_list(BasicType::float())));
    assert_eq!(decls[5].get_type(), Some(ListType::new_list(TRAIT_NUM.clone_box())));
    assert_eq!(decls[6].get_type(), Some(ListType::new_list(BasicType::bool())));
}

#[test]
fn test_invalid_lists() {
    let module = make_module(
        indoc! {r#"
            a = [1, 'a']
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2.0]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = ["hello", 'a']
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = ["hello", 3]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3N]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
}

#[test]
fn test_list_ops() {
    let module = make_module(
        indoc! {r#"
            empty = []
            a = [1]
            b = a ++ empty
            c = 'a' :: []
            d = "hello" :: "world" :: []
            e = 1 :: [2]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("TEST DECLS = {:#?}", decls);
}


#[test]
fn test_invalid_list_ops() {
    let module = make_module(
        indoc! {r#"
            a = [1, 'a']
            b = []
            c = a + b
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2.0]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = "hello" :: 'a'
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = "hello" :: [3]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3]
            b = 'b' :: a
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3]
            b = [1.0, 2.0]
            c = a ++ b
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
}