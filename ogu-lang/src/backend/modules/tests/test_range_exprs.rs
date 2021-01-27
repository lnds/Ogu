use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

#[test]
fn test_simple_range() {
    let module = make_module(
        indoc! {r#"
            a = [1..10]
            b = ['a'..'z']
            c = [1, 2..]
            e = [1N, 3N..]
            f = [1N..10N]
            g = 1
            h = 10
            i = [g .. h]
            j = [1, 3..21]
            k = [1.1, 1.2..10.0]
            l = 0
            m = 100N
            n = [0..m]
            o = 0N
            p = [o..m]
            q = [o..100.0]
            r = [o..100]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::char()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(ListType::new_list(TRAIT_NUM.clone_box()))
    );
    assert_eq!(
        decls[4].get_type(),
        Some(ListType::new_list(TRAIT_NUM.clone_box()))
    );
    assert_eq!(decls[5].get_type(), Some(BasicType::int()));
    assert_eq!(decls[6].get_type(), Some(BasicType::int()));
    assert_eq!(
        decls[7].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[8].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[9].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
    assert_eq!(decls[10].get_type(), Some(BasicType::int()));
    assert_eq!(decls[11].get_type(), Some(TRAIT_NUM.clone_box()));
    assert_eq!(
        decls[12].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(decls[13].get_type(), Some(TRAIT_NUM.clone_box()));
    assert_eq!(
        decls[14].get_type(),
        Some(ListType::new_list(TRAIT_NUM.clone_box()))
    );
    assert_eq!(
        decls[15].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
    assert_eq!(
        decls[16].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
}

#[test]
fn test_invalid_ranges() {
    let module = make_module(
        indoc! {r#"
            a = ["a" .. "z"]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = "1"
            b = "10"
            c = [a .. b]
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
            a = [1, 2.0..]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {r#"
            a = [1, 2 .. 3.0]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {r#"
            a = [1, 2.0 .. 3]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {r#"
            a = [1.0, 2 .. 3.0]
            "#},
        default_sym_table(),
    );
    println!("module: {:?}", module);
    assert!(module.is_err());
}
