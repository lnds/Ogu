use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::dict_type::DictType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

#[test]
fn test_simple_dict() {
    let module = make_module(
        indoc! {r#"
            empty = #{}
            a = #{'a': 1, 'b': 2}
            b = #{"a": 1, "b": 2}
            c = #{1: 1.0, 2: 2.0}
            d = 10
            e = "10"
            f = #{d: e, 20: "20"}
            g = #{10N: e, 20N: "20"}
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(DictType::new_empty()));
    assert_eq!(
        decls[1].get_type(),
        Some(DictType::new_dict(BasicType::char(), BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(DictType::new_dict(
            BasicType::static_str(),
            BasicType::int()
        ))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(DictType::new_dict(BasicType::int(), BasicType::float()))
    );
    assert_eq!(decls[4].get_type(), Some(BasicType::int()));
    assert_eq!(decls[5].get_type(), Some(BasicType::static_str()));
    assert_eq!(
        decls[6].get_type(),
        Some(DictType::new_dict(
            BasicType::int(),
            BasicType::static_str()
        ))
    );
    assert_eq!(
        decls[7].get_type(),
        Some(DictType::new_dict(
            TRAIT_NUM.clone_box(),
            BasicType::static_str()
        ))
    );
}

#[test]
fn test_invalid_dicts() {
    let module = make_module(
        indoc! {r#"
            a = {1: 'a'}
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = #{1 => 2.0}
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = #{"hello": 'a', "world": 10}
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = #{"hello": 3, "hello": 4.0}
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = #{1, 2, 3N}
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
}
