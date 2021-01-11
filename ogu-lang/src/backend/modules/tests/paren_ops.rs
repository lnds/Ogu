use crate::backend::compiler::default_sym_table;
use crate::backend::modules::module::Module;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::TRAIT_NUM;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use anyhow::Result;
use indoc::indoc;

#[test]
fn test_add() {
    let module = make_module(
        indoc! {r#"
        sum x y = (+ x y)
        sum' = (+)
        inc = (+ 1)

        a = 10
        b = inc a
        c = sum (sum' a 0.0) b
        d = sum (sum' a 0) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_sub() {
    let module = make_module(
        indoc! {r#"
        sub x y = (- x y)
        sub' = (-)
        dec = (- 1)

        a = 10
        b = dec a
        c = sub (sub' a 0.0) b
        d = sub (sub' a 0) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_mul() {
    let module = make_module(
        indoc! {r#"
        mul x y = (* x y)
        mul' = (*)
        same = (* 1)
        a = 10
        b = same a
        c = mul (mul' a 1.0) b
        d = mul (mul' a 1) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_div() {
    let module = make_module(
        indoc! {r#"
        div x y = (/ x y)
        div' = (/)
        same = (/ 1N)
        a = 10
        b = same a
        c = div (div' a 1.0) b
        d = div (div' a 1) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::float()),
            Some(BasicType::float()),
        ],
    );
}

#[test]
fn test_mod() {
    let module = make_module(
        indoc! {r#"
        mod x y = (% x y)
        mod' = (%)
        same = (% 1)
        a = 10
        b = same a
        c = mod (mod' a 1.0) b
        d = mod (mod' a 1) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_pow() {
    let module = make_module(
        indoc! {r#"
        pow x y = (^ x y)
        pow' = (^)
        same = (^ 1)
        a = 10
        b = same a
        c = pow (pow' a 1.0) b
        d = pow (pow' a 1) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_intdiv() {
    let module = make_module(
        indoc! {r#"
        div x y = (// x y)
        div' = (//)
        same = (// 1N)
        a = 10
        b = same a
        c = div (div' a 1.0) b
        d = div (div' a 1) b
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::int()),
        ],
    );
}

fn validate_decls(module: Result<Module>, t: Vec<Option<Box<dyn Type>>>) {
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box()
        )
    );
    assert_eq!(decls[0].get_type(), decls[1].get_type());
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())
    );
    assert_eq!(decls[3].get_type(), t[0]);
    assert_eq!(decls[4].get_type(), t[1]);
    assert_eq!(decls[5].get_type(), t[2]);
    assert_eq!(decls[6].get_type(), t[3]);
}
