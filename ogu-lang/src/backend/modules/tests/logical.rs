use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use indoc::indoc;

#[test]
fn test_logical() {
    let module = make_module(
        indoc! {r#"
        a = 10
        b = 20
        c = if a == b || a <= b then a else b
        d = a == b
        e = a <= b
        f = a >= b
        de_morgan_1 = (not (e || f)) == ((not e) && (not f))
        de_morgan_2 = (not (e && f)) == ((not e) || (not f))
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));
    assert_eq!(decls[1].get_type(), Some(BasicType::int()));
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));
    assert_eq!(decls[3].get_type(), Some(BasicType::bool()));
    assert_eq!(decls[4].get_type(), Some(BasicType::bool()));
    assert_eq!(decls[5].get_type(), Some(BasicType::bool()));
    assert_eq!(decls[6].get_type(), Some(BasicType::bool()));
    assert_eq!(decls[7].get_type(), Some(BasicType::bool()));
}
