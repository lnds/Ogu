use crate::backend::compiler::default_sym_table;
use crate::backend::modules::module::Module;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::trait_type::{TRAIT_EQ, TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::scopes::types::{Type, TypeClone};
use anyhow::Result;
use indoc::indoc;
use crate::backend::modules::types::list_type::ListType;

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

        e = (+) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
            Some(BasicType::int()),
        ],
    );
}


#[test]
fn test_cons() {
    let module = make_module(
        indoc! {r#"
        cons x xs = (:: x xs)
        cons' = (::)
        cons_1 = (:: 1)

        a = [10]
        b = cons_1 a
        c = cons 1 (cons' 0  b)
        d = cons 1 (cons' 0 a)

        e = (::) 1 [2]
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(Some(vec![TRAIT_UNKNOWN.clone_box(), ListType::new_list(TRAIT_UNKNOWN.clone_box())]), ListType::new_list(TRAIT_UNKNOWN.clone_box())),
        FuncType::new_opt(Some(vec![ListType::new_list(BasicType::int())]), ListType::new_list(BasicType::int())),
        vec![
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
        ],
    );
}

#[test]
fn test_concat() {
    let module = make_module(
        indoc! {r#"
        concat x xs = (++ x xs)
        concat' = (++)
        concat_1 = (++ [1])

        a = [1]
        b = concat_1 a
        c = concat [1] (concat' [0]  b)
        d = concat [1] (concat' [0] a)

        e = (++) [1] [2]
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box()), ListType::new_list(TRAIT_UNKNOWN.clone_box())]), ListType::new_list(TRAIT_UNKNOWN.clone_box())),
        FuncType::new_opt(Some(vec![ListType::new_list(BasicType::int())]), ListType::new_list(BasicType::int())),
        vec![
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
            Some(ListType::new_list(BasicType::int())),
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

        e = (-) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
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

        e = (*) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
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

        e = (/) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::float()),
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

        e = (%) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
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

        e = (^) 2 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::float()),
            Some(BasicType::int()),
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

        e = (//) 1.0 2.0
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::int()),
            Some(BasicType::int()),
        ],
    );
}

#[test]
fn test_eq() {
    let module = make_module(
        indoc! {r#"
        eq x y = (== x y)
        eq' = (==)
        unit = (== 1)
        a = 10
        b = unit a
        c = eq (eq' a 1.0) b
        d = eq (eq' a 1) b

        e = (==) true true
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_EQ.clone_box(), TRAIT_EQ.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_EQ.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_ne() {
    let module = make_module(
        indoc! {r#"
        ne x y = (!= x y)
        ne' = (!=)
        unit = (!= 1)
        a = 10
        b = unit a
        c = ne (ne' a 1.0) b
        d = ne (ne' a 1) b

        e = (!=) 1 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_EQ.clone_box(), TRAIT_EQ.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_EQ.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_gt() {
    let module = make_module(
        indoc! {r#"
        gt x y = (> x y)
        gt' = (>)
        pos = (> 0)
        a = 10
        b = pos a
        c = gt (gt' a 0.0) b
        d = gt (gt' a 0) b

        e = (>) 1 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_ORD.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_ge() {
    let module = make_module(
        indoc! {r#"
        ge x y = (>= x y)
        ge' = (>=)
        pos = (>= 0)
        a = 10
        b = pos a
        c = ge (ge' a 0.0) b
        d = ge (ge' a 0) b

        e = (>=) 1 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_ORD.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_lt() {
    let module = make_module(
        indoc! {r#"
        lt x y = (< x y)
        lt' = (<)
        neg = (< 0)
        a = 10
        b = neg a
        c = lt (lt' a 0.0) b
        d = lt (lt' a 0) b

        e = (<) 1 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_ORD.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_le() {
    let module = make_module(
        indoc! {r#"
        le x y = (<= x y)
        le' = (<=)
        neg = (<= 0)
        a = 10
        b = neg a
        c = le (le' a 0.0) b
        d = le (le' a 0) b

        e = (<=) 1 2
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![TRAIT_ORD.clone_box()]), BasicType::bool()),
        vec![
            Some(BasicType::int()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_or() {
    let module = make_module(
        indoc! {r#"
        or x y = (|| x y)
        or' = (||)
        neg = (|| (0 == 1))
        a = (0 == 0)
        b = neg a
        c = or (or' a (0 == 1)) b
        d = or (or' a (0 == 0)) b

        e = (||) true false
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![BasicType::bool(), BasicType::bool()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![BasicType::bool()]), BasicType::bool()),
        vec![
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_and() {
    let module = make_module(
        indoc! {r#"
        and x y = (&& x y)
        and' = (&&)
        neg = (&& (0 == 1))
        a = (0 == 0)
        b = neg a
        c = and (and' a (0 == 1)) b
        d = and (and' a (0 == 0)) b

        e = (&&) true true
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(
            Some(vec![BasicType::bool(), BasicType::bool()]),
            BasicType::bool(),
        ),
        FuncType::new_opt(Some(vec![BasicType::bool()]), BasicType::bool()),
        vec![
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}

#[test]
fn test_not() {
    let module = make_module(
        indoc! {r#"
        not x = (! x)
        not' = (!)
        neg = (!)
        a = (0 == 0)
        b = neg a
        c = not (not' b)
        d = not (not' a)

        e = (!) false
        "#},
        default_sym_table(),
    );
    println!("module = {:?}", module);
    assert!(module.is_ok());
    validate_decls(
        module,
        FuncType::new_opt(Some(vec![BasicType::bool()]), BasicType::bool()),
        FuncType::new_opt(Some(vec![BasicType::bool()]), BasicType::bool()),
        vec![
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
            Some(BasicType::bool()),
        ],
    );
}


fn validate_decls(
    module: Result<Module>,
    ft1: Option<Box<dyn Type>>,
    ft2: Option<Box<dyn Type>>,
    t: Vec<Option<Box<dyn Type>>>,
) {
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("DECLS: {:#?}", decls);
    assert_eq!(decls[0].get_type(), ft1);
    assert_eq!(decls[0].get_type(), decls[1].get_type());
    assert_eq!(decls[2].get_type(), ft2);
    assert_eq!(decls[3].get_type(), t[0]);
    assert_eq!(decls[4].get_type(), t[1]);
    assert_eq!(decls[5].get_type(), t[2]);
    assert_eq!(decls[6].get_type(), t[3]);
    assert_eq!(decls[7].get_type(), t[4]);
}
