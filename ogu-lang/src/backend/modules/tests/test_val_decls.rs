use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use indoc::indoc;

#[test]
fn test_vals_1() {
    let module = make_module(
        indoc! {r#"
        a = 10
        b = (a)
        c () = a
        d = (c)
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(decls[0].get_type(), decls[1].get_type());
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(None, BasicType::int())
    );
    assert_eq!(decls[3].get_type(), Some(BasicType::int()));
}

#[test]
fn test_vals_2() {
    let module = make_module(
        indoc! {r#"
        a = 10
        b = 'b'
        c = 10.0
        d = "str"
        e = ()
        f = #2020-01-03
        g = #/(a|b)*/#
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));
    assert_eq!(decls[1].get_type(), Some(BasicType::char()));
    assert_eq!(decls[2].get_type(), Some(BasicType::float()));
    assert_eq!(decls[3].get_type(), Some(BasicType::static_str()));
    assert_eq!(decls[4].get_type(), Some(BasicType::unit()));
    assert_eq!(decls[5].get_type(), Some(BasicType::date()));
    assert_eq!(decls[6].get_type(), Some(BasicType::regexp()));
}

#[test]
fn test_values() {
    let module = make_module(
        indoc! {"
                a = 1
                b = a * 1
                c = a * b"},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));
    assert_eq!(decls[1].get_type(), Some(BasicType::int()));
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));
}

#[test]
fn test_no_dups() {
    let module = make_module(
        indoc! {"
                a = 1
                a = a * 1"},
        default_sym_table(),
    );
    println!("{:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {"
        b = let a = 1, a = 2 in a + 1"},
        default_sym_table(),
    );
    println!("{:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {"
        b = let (a, a) = (1, 1) in a * a"},
        default_sym_table(),
    );
    println!("{:?}", module);
    assert!(module.is_err());

    let module = make_module(
        indoc! {"
                a = 1
                b = let (a, b) = (a, a) in a * b"},
        default_sym_table(),
    );
    println!("{:?}", module);
    assert!(module.is_ok());
}
