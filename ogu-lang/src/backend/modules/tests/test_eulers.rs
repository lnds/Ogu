use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_UNKNOWN, TRAIT_NUM, TRAIT_ORD};
use crate::backend::scopes::types::TypeClone;
use indoc::indoc;


#[test]
fn test_euler_1() {
    let module = make_module(
        indoc! {r#"
            -- Resuelve problema 1 del proyecto Euler
            union [] [] = []
            union xs [] = xs
            union [] ys = ys
            union (x :: xs) ys = x :: union xs ys

            sum [] = 0N
            sum x :: xs = x + sum xs

            result = union [3, 6 .. 999] [5, 10 .. 999] |> sum"#},
        default_sym_table(),
    );
    //println!("module = {:?}", module);
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box()), ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(
            Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
            TRAIT_NUM.clone_box())
    );
    assert_eq!(decls[2].get_type(), Some(BasicType::int()));
}

#[test]
fn test_euler_2() {
    let module = make_module(
        indoc! {r#"
           fib-seq = fib 0 1
                where fib a b = lazy a :: fib b (a + b)

           even? x = x % 2 == 0

           take-while f [] = []
           take-while f (x :: xs) = if f x then x :: (take-while f xs) else []

           filter f [] = []
           filter f (x :: xs) = if f x then x :: filter f xs else filter f xs

           sum [] = 0
           sum (x :: xs) = x + sum xs

           result = fib-seq |> take-while (\x -> x < 4000000) |> filter even? |> sum"#},
        default_sym_table(),
    );
    if module.is_err() {
        println!("module = {:?}", module);
    }
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::bool())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                                    ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
                          ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );

    assert_eq!(
        decls[3].get_type(),
        FuncType::new_opt(Some(vec![FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                                    ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
                          ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );

    assert_eq!(
        decls[4].get_type(),
        FuncType::new_opt(Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
                          BasicType::int())
    );

    assert_eq!(decls[5].get_type(), Some(BasicType::int()));
}

#[test]
fn test_euler_3() {
    let module = make_module(
        indoc! {r#"
        zero? n = n == 0N

        last [] = error! "last of empty list"
        last [x] = x
        last (x :: xs) = last xs

        factor-of? f n = zero? (n % f)

        prime-factors f n
            | n == 1 = lazy []
            | factor-of? f n = lazy f :: prime-factors f (n // f)
            | otherwise = recur (f + 1) n

        result = prime-factors 2 600851475143 |> last"#},
        default_sym_table(),
    );
    if module.is_err() {
        println!("module = {:?}", module);
    }
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]), TRAIT_UNKNOWN.clone_box())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]), BasicType::bool())
    );


    assert_eq!(
        decls[3].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int(), BasicType::int()]),
                         ListType::new_list(TRAIT_NUM.clone_box()))
    );

    assert_eq!(
        decls[4].get_type(),
        Some(BasicType::int())
    );

}


#[test]
fn test_euler_4() {
    let module = make_module(
        indoc! {r#"
        zero? n = n == 0N

        rev num =
            for reversed = 0, n = num
            loop
                if zero? n then reversed
                else repeat (reversed * 10 + n % 10), (n // 10)

        palindrome? n = n == rev n

        filter f [] = []
        filter f (h :: t) = if f h then h :: filter f t else filter f t

        max [] = error! "no max for empty list"
        max [x] = x
        max [a, b] = if a > b then a else b
        max (a :: b :: tail) = if a > b then max (a :: tail) else max (b :: tail)



        result = [x * y | x <- [100..999], y <- [100..999]] |> filter palindrome?  |> max"#},
        default_sym_table(),
    );
    if module.is_err() {
        println!("module = {:?}", module);
    }
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::int())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]), BasicType::bool())
    );


    assert_eq!(
        decls[3].get_type(),
        FuncType::new_opt(Some(vec![FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                                    ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
                          ListType::new_list(TRAIT_UNKNOWN.clone_box()))
    );

    assert_eq!(
        decls[4].get_type(),
        FuncType::new_opt(Some(vec![ListType::new_list(TRAIT_ORD.clone_box())]),
                          TRAIT_ORD.clone_box())
    );
    assert_eq!(
        decls[5].get_type(),
        Some(BasicType::int())
    );

}

#[test]
fn test_euler_5() {
    let module = make_module(
        indoc! {r#"
        zero? n = n == 0N

        gcd x y =
            if zero? y then x
            else gcd y (x % y)

        lcm a b
           | zero? a = 0
           | zero? b = 0
           | otherwise = b * (a // (gcd a b))

        reduce f seed [] = error! "reduce undefined for empty list"
        reduce f seed [x] = f seed x
        reduce f seed (x :: xs) = reduce f (f seed x) xs

        divisors n = reduce lcm 1 [2 .. n]

        result = divisors 20"#},
        default_sym_table(),
    );
    if module.is_err() {
        println!("module = {:?}", module);
    }
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    //println!("TEST DECLS = {:#?}", decls);
    assert_eq!(
        decls[0].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool())
    );
    assert_eq!(
        decls[1].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())
    );
    assert_eq!(
        decls[2].get_type(),
        FuncType::new_opt(Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]), BasicType::int())
    );


    assert_eq!(
        decls[3].get_type(),
        FuncType::new_opt(Some(vec![FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()]), TRAIT_UNKNOWN.clone_box()),
                                         TRAIT_UNKNOWN.clone_box(), ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
                          TRAIT_UNKNOWN.clone_box())
    );


    assert_eq!(
        decls[4].get_type(),
        FuncType::new_opt(Some(vec![BasicType::int()]),
                          BasicType::int())
    );
    assert_eq!(
        decls[5].get_type(),
        Some(BasicType::int())
    );

}