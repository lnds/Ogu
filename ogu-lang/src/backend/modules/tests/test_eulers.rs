use crate::backend::compiler::default_sym_table;
use crate::backend::modules::tests::make_module;
use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::func_type::FuncType;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::trait_type::{TRAIT_NUM, TRAIT_ORD, TRAIT_UNKNOWN};
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                ListType::new_list(TRAIT_UNKNOWN.clone_box()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
            TRAIT_NUM.clone_box(),
        ))
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::bool()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );

    assert_eq!(
        decls[4].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_NUM.clone_box())]),
            BasicType::int(),
        ))
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            TRAIT_UNKNOWN.clone_box(),
        ))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            BasicType::bool(),
        ))
    );

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![BasicType::int(), BasicType::int()]),
            ListType::new_list(TRAIT_NUM.clone_box()),
        ))
    );

    assert_eq!(decls[4].get_type(), Some(BasicType::int()));
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::bool()))
    );

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );

    assert_eq!(
        decls[4].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_ORD.clone_box())]),
            TRAIT_ORD.clone_box(),
        ))
    );
    assert_eq!(decls[5].get_type(), Some(BasicType::int()));
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
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            BasicType::int(),
        ))
    );

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                TRAIT_UNKNOWN.clone_box(),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            TRAIT_UNKNOWN.clone_box(),
        ))
    );

    assert_eq!(
        decls[4].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()))
    );
    assert_eq!(decls[5].get_type(), Some(BasicType::int()));
}

#[test]
fn test_euler_6() {
    let module = make_module(
        indoc! {r#"
        zero? n = n == 0N -- 0

        gcd x y = -- 1
            if zero? y then x
            else gcd y (x % y)

        lcm a b -- 2
           | zero? a = 0
           | zero? b = 0
           | otherwise = b * (a // (gcd a b))

        reduce _ [] = error! "reduce undefined for empty list" -- 3
        reduce _ [x] =  x
        reduce f [x, y] =  f x y
        reduce f (x :: y :: xs) = reduce f  ((f x y) :: xs)

        map f [] = [] -- 4
        map f (x :: xs) = (f x) :: (map f xs)

        sum-n n = (n * (n + 1)) // 2 -- 5

        square-sum n = let s = sum-n n in s * s -- 6

        add a b = a + b -- 7

        sum-n-square n = reduce add  <| map (\x -> x * x) [1..n] -- 8

        dif-squares n = (square-sum n) - (sum-n-square n) -- 9

        result = dif-squares 100 -- 10"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(Some(vec![TRAIT_NUM.clone_box()]), BasicType::bool()))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            BasicType::int(),
        ))
    );

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            TRAIT_UNKNOWN.clone_box(),
        ))
    );

    assert_eq!(
        decls[4].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );

    assert_eq!(
        decls[5].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()))
    );

    assert_eq!(
        decls[6].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()))
    );

    assert_eq!(
        decls[7].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        ))
    );

    assert_eq!(
        decls[8].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), TRAIT_NUM.clone_box()))
    );

    assert_eq!(
        decls[9].get_type(),
        Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int()))
    );

    assert_eq!(decls[10].get_type(), Some(BasicType::int()));
}

#[test]
fn test_euler_7() {
    let module = make_module(
        indoc! {r#"
        filter _ [] = []
        filter f (x :: xs) =
              if f x then x :: filter f xs
                     else filter f xs

        length [] = 0
        length x :: xs = 1 + length xs

        primes = lazy 2 :: filter ( (== 1) << length << prime-factors )  [3,5..]


        prime-factors n = factor n primes
          where
            factor n (p :: ps)
                | p * p > n   = [n]
                | n % p == 0  = p :: factor (n // p) (p :: ps)
                | otherwise   = factor n ps

        result = primes @ 10000"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(
        decls[0].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]), BasicType::int())));

    assert_eq!(decls[2].get_type(), Some(ListType::new_list(BasicType::int())));

    assert_eq!(
        decls[3].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), ListType::new_list(BasicType::int()))));

    assert_eq!(decls[4].get_type(), Some(BasicType::int()));
}


#[test]
fn test_euler_8() {
    let module = make_module(
        indoc! {r#"
        num-1000 = """73167176531330624919225119674426574742355349194934
        96983520312774506326239578318016984801869478851843
        85861560789112949495459501737958331952853208805511
        12540698747158523863050715693290963295227443043557
        66896648950445244523161731856403098711121722383113
        62229893423380308135336276614282806444486645238749
        30358907296290491560440772390713810515859307960866
        70172427121883998797908792274921901699720888093776
        65727333001053367881220235421809751254540594752243
        52584907711670556013604839586446706324415722155397
        53697817977846174064955149290862569321978468622482
        83972241375657056057490261407972968652414535100474
        82166370484403199890008895243450658541227588666881
        16427171479924442928230863465674813919123162824586
        17866458359124566529476545682848912883142607690042
        24219022671055626321111109370544217506941658960408
        07198403850962455444362981230987879927244284909188
        84580156166097919133875499200524063689912560717606
        05886116467109405077541002256983155200055935729725
        71636269561882670428252483600823257530420752963450"""

        filter f [] = []
        filter f (x :: xs) = if f x then x :: filter f xs else filter f xs

        map f [] = []
        map f (x :: xs) = (f x) :: (map f xs)

        remove-nl x = x

        int x = x

        mul x y = x * y

        reduce _ [] = error! "reduce undefined for empty list" -- 3
        reduce _ [x] =  x
        reduce f [x, y] =  f x y
        reduce f (x :: y :: xs) = reduce f  ((f x y) :: xs)

        take _ [] = []
        take 0 _  = []
        take 1 [] = []
        take 1 (x :: xs) = [x]
        take n (x :: xs) = [x] ++ (take (n - 1) xs)

        tail [] = []
        tail (x :: xs) = xs

        nums = num-1000 |> remove-nl |> map (\n -> ((int n) - (int '0')) ) |> filter (\n -> n >= 0)

        calc-13 num-seq = reduce mul (take 13 num-seq)

        s13 () = seqs-of-13 nums
          where seqs-of-13 n = lazy (calc-13 n) :: (seqs-of-13 $ tail n)

        max x y = if x > y then x else y

        result = s13 |> take 1000 |> reduce max"#},
        default_sym_table(),
    );

    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::static_str()));
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );
    assert_eq!(decls[3].get_type(), Some(FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), TRAIT_UNKNOWN.clone_box())));
    assert_eq!(decls[4].get_type(), Some(FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), TRAIT_UNKNOWN.clone_box())));

    assert_eq!(
        decls[5].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]),
            TRAIT_NUM.clone_box(),
        )));
    assert_eq!(
        decls[6].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            TRAIT_UNKNOWN.clone_box(),
        ))
    );

    assert_eq!(
        decls[7].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![BasicType::int(), ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        )));

    assert_eq!(
        decls[8].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        )));
    assert_eq!(
        decls[9].get_type(),
        Some(ListType::new_list(BasicType::int()))
    );

    assert_eq!(
        decls[10].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]),  // MMMM
            TRAIT_NUM.clone_box())));

    assert_eq!(
        decls[11].get_type(),
        Some(FuncType::new_func_type(
            None,
            ListType::new_list(BasicType::int()))));

    assert_eq!(
        decls[12].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![TRAIT_ORD.clone_box(), TRAIT_ORD.clone_box()]),
            TRAIT_ORD.clone_box(),
        )));

    assert_eq!(
        decls[13].get_type(),
        Some(TRAIT_ORD.clone_box())); // MMMM
}


#[test]
fn test_euler_9() {
    let module = make_module(
        indoc! {r#"
        result = let ((a, b, c)) = [ (a, b, c) | a <- [1..249], b <- [251..499], c <- [1..499], (a + b + c == 1000) && ((a * a + b * b) == c * c) ] in a * b * c"#},
        default_sym_table(),
    );
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    assert_eq!(decls[0].get_type(), Some(BasicType::int()));// Some(TRAIT_NUM.clone_box())); /// TODO: CHECK THIS SHOULD BE INT
}


#[test]
fn test_euler_10() {
    let module = make_module(
        indoc! {r#"
        primes = lazy 2 :: filter (prime-factors >> length >> (== 1)) [3,5..]

        filter f [] = []
        filter f (x :: xs) = if f x then x :: filter f xs else filter f xs

        length [] = 0
        length x :: xs = 1 + length xs

        reduce _ [] = error! "reduce undefined for empty list" -- 3
        reduce _ [x] =  x
        reduce f [x, y] =  f x y
        reduce f (x :: y :: xs) = reduce f  ((f x y) :: xs)

        prime-factors n = factor n primes
          where
            factor n (p::ps)
              | p * p > n = [n]
              | n % p == 0 = p :: factor (n // p) (p :: ps)
              | otherwise = factor n ps

        dec x = x - 1

        add = (+)

        result = reduce add (primes (dec 2000000))"#},
        default_sym_table(),
    );
    if module.is_err() {
        println!("module = {:?}", module);
    }
    assert!(module.is_ok());
    let module = module.unwrap();
    let decls = module.get_decls();
    println!("{:#?}", decls);
    assert_eq!(decls[0].get_type(), Some(ListType::new_list(BasicType::int())));
    assert_eq!(
        decls[1].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(Some(vec![TRAIT_UNKNOWN.clone_box()]), BasicType::bool()),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            ListType::new_list(TRAIT_UNKNOWN.clone_box()),
        ))
    );
    assert_eq!(
        decls[2].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![ListType::new_list(TRAIT_UNKNOWN.clone_box())]), BasicType::int())));

    assert_eq!(
        decls[3].get_type(),
        Some(FuncType::new_func_type(
            Some(vec![
                FuncType::new_func_type(
                    Some(vec![TRAIT_UNKNOWN.clone_box(), TRAIT_UNKNOWN.clone_box()]),
                    TRAIT_UNKNOWN.clone_box(),
                ),
                ListType::new_list(TRAIT_UNKNOWN.clone_box())
            ]),
            TRAIT_UNKNOWN.clone_box(),
        ))
    );

    assert_eq!(
        decls[4].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), ListType::new_list(BasicType::int()))));

    assert_eq!(decls[5].get_type(), Some(FuncType::new_func_type(Some(vec![BasicType::int()]), BasicType::int())));
    assert_eq!(decls[6].get_type(), Some(FuncType::new_func_type(Some(vec![TRAIT_NUM.clone_box(), TRAIT_NUM.clone_box()]), TRAIT_NUM.clone_box())));

    assert_eq!(decls[7].get_type(), Some(BasicType::int()));


}

