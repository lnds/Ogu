use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;

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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::char()))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(ListType::new_list(BasicType::static_str()))
    );
    assert_eq!(
        decls[4].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
    assert_eq!(
        decls[5].get_type(),
        Some(ListType::new_list(TRAIT_NUM.clone_box()))
    );
    assert_eq!(
        decls[6].get_type(),
        Some(ListType::new_list(BasicType::bool()))
    );
}

#[test]
fn test_invalid_lists() {
    let module = make_module(
        indoc! {r#"
            a = [1, 'a']
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2.0]
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = ["hello", 'a']
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = ["hello", 3]
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3.0]
            "#},
        default_sym_table(),
    );
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(ListType::new_list(BasicType::char()))
    );
    assert_eq!(
        decls[4].get_type(),
        Some(ListType::new_list(BasicType::static_str()))
    );
    assert_eq!(
        decls[5].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
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
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2.0]
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = "hello" :: 'a'
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = "hello" :: [3]
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3]
            b = 'b' :: a
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());

    let module = make_module(
        indoc! {r#"
            a = [1, 2, 3]
            b = [1.0, 2.0]
            c = a ++ b
            "#},
        default_sym_table(),
    );
    assert!(module.is_err());
}

#[test]
fn test_str_are_lists() {
    let module = make_module(
        indoc! {r#"
            hello = "hello"
            world = "world"
            hello_world = hello ++ " " ++ world
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
}

#[test]
fn test_lists_are_eq() {
    let module = make_module(
        indoc! {r#"
            a = "hello" != "world"
            b = [1, 2, 3]
            c = [3, 2, 1]
            d = b != c
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::bool()));
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(decls[3].get_type(), Some(BasicType::bool()));
}

#[test]
fn test_lists_are_ord() {
    let module = make_module(
        indoc! {r#"
            a = "hello" < "world"
            b = [1, 2, 3]
            c = [3, 2, 1]
            d = b > c
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::bool()));
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(decls[3].get_type(), Some(BasicType::bool()));
}

#[test]
fn test_list_comprehension_1() {
    let module = make_module(
        indoc! {r#"
        l = [2, 7, 23, 12, 1, 44, 89, 5, 32, 23, 10, 105, 13]
        b = [a | a <- l, a < 50]
        c = [a * 1.0 | a <- l, a >= 50]
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
}

#[test]
fn test_list_comprehension_2() {
    let module = make_module(
        indoc! {r#"
        l = [2, 7, 23, 12, 1, 44, 89, 5, 32, 23, 10, 105, 13]
        b = [a | a <- l, a < 50]
        c = let smaller = [a * 1.0 | a <- l, a < 50],
                bigger = [a * 1.0 | a <- l, a >= 50]
            in smaller ++ (50.0 :: bigger)
        f n l = n :: l
        d = f 50 l
        g l1 l2 = l1 ++ l2
        h = g [20.0, 10.0] [3.0, 5.0]
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                TRAIT_UNKNOWN.clone_box(),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box())
        ))
    );
    assert_eq!(
        decls[4].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[5].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                ListType::new_list(TRAIT_UNKNOWN.clone_box()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box())
        ))
    );
    assert_eq!(
        decls[6].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
}

#[test]
fn test_list_comprehension_3() {
    let module = make_module(
        indoc! {r#"
        l = [2, 7, 23, 12, 1, 44, 89, 5, 32, 23, 10, 105, 13]
        l2 = [(x, x) | x <- l]
        b = [r | a <- l, a < 50, let r = a * 1.0]
        c = [(x, y, z) | x <- l, y <- l, x < y, let z = 1.0 * (x + y)]
        d = [t | x <- l, y <- l, x < y, let t = (x * 1.0, y * 1N, (x + y))]
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(TupleType::new_box(vec![
            BasicType::int(),
            BasicType::int()
        ])))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(BasicType::float()))
    );
    assert_eq!(
        decls[3].get_type(),
        Some(ListType::new_list(TupleType::new_box(vec![
            BasicType::int(),
            BasicType::int(),
            BasicType::float()
        ])))
    );
    assert_eq!(
        decls[4].get_type(),
        Some(ListType::new_list(TupleType::new_box(vec![
            BasicType::float(),
            BasicType::int(),
            BasicType::int()
        ])))
    );
}

#[test]
fn test_list_func1() {
    let module = make_module(
        indoc! {r#"
            sum [] = 0N
            sum (head :: tail) = head + sum tail
            s = sum [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
            TRAIT_NUM.clone_box()
        ))
    );

    assert_eq!(decls[1].get_type(), Some(BasicType::int()));
}

#[test]
fn test_list_func2() {
    let module = make_module(
        indoc! {r#"
        quicksort [] = []
        quicksort (x :: xs) = smaller ++ (x :: bigger)
          where
            smaller = quicksort [a | a <- xs, a <= x]
            bigger  = quicksort [a | a <- xs, a > x]

        s = quicksort [2, 7, 23, 12, 1, 44, 89, 5, 32, 23, 10, 105, 13]
        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_ORD.clone_box())]),
            ListType::new_list(TRAIT_ORD.clone_box())
        ))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
}

#[test]
fn test_list_fib() {
    let module = make_module(
        indoc! {r#"
        fib-seq = fib 0 1
            where fib a b = lazy a :: fib b (a + b)

        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
}

#[test]
fn test_prime_factors_1() {
    let module = make_module(
        indoc! {r#"
        prime-factors f n
            | n == 1 = lazy []
            | n % f == 0 = lazy f :: prime-factors f (n / f)
            | otherwise = recur (f + 1) n

        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            ListType::new_list(BasicType::int())
        )
    );
}

#[test]
fn test_prime_factors_2() {
    let module = make_module(
        indoc! {r#"
        factor-of? f n = (n % f) == 0

        prime-factors f n
            | n == 1 = lazy []
            | factor-of? f n = lazy f :: prime-factors f (n // f)
            | otherwise = recur (f + 1) n

        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            BasicType::bool()
        )
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            ListType::new_list(BasicType::int())
        )
    );
}

#[test]
fn test_prime_factors_3() {
    let module = make_module(
        indoc! {r#"
        zero? x = x == 0

        factor-of? f n = zero? (n % f)

        prime-factors a b
            | b == 1 = lazy []
            | factor-of? a b = lazy a :: prime-factors a (b // a)
            | otherwise = recur (a + 1) b

        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::bool())
    );

    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            BasicType::bool()
        )
    );

    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            ListType::new_list(BasicType::int())
        )
    );
}

#[test]
fn test_prime_factors_4() {
    let module = make_module(
        indoc! {r#"
        zero? x = x == 0N

        factor-of? f n = zero? (n % f)

        prime-factors a b
            | b == 1 = lazy []
            | factor-of? a b = lazy a :: prime-factors a (b // a)
            | otherwise = recur (a + 1) b

        "#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool())
    );

    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            BasicType::bool()
        )
    );

    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(
            Some(vec![BasicType::int(), BasicType::int()]),
            ListType::new_list(TRAIT_NUM.clone_box())
        )
    );
}

#[test]
fn test_map() {
    let module = make_module(
        indoc! {r#"
        -- map : (a -> a) -> [a] -> [a]
        map f [] = []
        map f (x :: xs) = (f x) :: (map f xs)

        -- pow : Num -> Num
        pow x = x * x

        -- squares -> (Num -> Num) -> [Num] -> [Num
        squares   =  map pow [0..10]
        squares'  =  map (\x -> x * x) [0..10]
        squares'' =  [0..10] |> map \x -> x * x

        "#},
        default_sym_table(),
    );

    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box()
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box())
        )
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())
    );
    assert_eq!(
        decls[2].get_type(),
        Some(ListType::new_list(TRAIT_NUM.clone_box()))
    );
    assert_eq!(decls[2].get_type(), decls[3].get_type());
    assert_eq!(decls[3].get_type(), decls[4].get_type());
}
