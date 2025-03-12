module ComplexExamples where

import WhileLang.WhileEncoder
    ( Stmt(..),
      BExp(..),
      AExp(..),
      Program,
      Label )
import Variability.VarTypes (Prop, mkBDDVar)

---------------------------------------------------------------
-- 1. Deep Loop Computation (deep_loop)
---------------------------------------------------------------
propDeepLoop :: Prop
propDeepLoop = mkBDDVar "DEEP_LOOP"

deep_loop_s01 = Assignment "i" (Const 0) 1  -- i := 0

deep_loop_test = (LTExp (Variable "i") (Const 100000), 2)  -- while i < 100000

deep_loop_s02 = Assignment "x" (Add (Variable "x") (Variable "i")) 3 -- x := x + i

deep_loop_s03 = Assignment "i" (Add (Variable "i") (Const 1)) 4  -- i := i + 1

deep_loop_body = Seq deep_loop_s02 deep_loop_s03

deep_loop_variant = Variant propDeepLoop deep_loop_body (Skip 5)

deep_loop = Seq deep_loop_s01 (While deep_loop_test deep_loop_variant)

-- Evolution: Deep Loop with Extra Computation
deep_loop_v2 = Seq deep_loop (Assignment "y" (Mult (Variable "x") (Const 2)) 6)  -- New computation based on previous x

---------------------------------------------------------------
-- 2. Nested Loop with Variability (nested_variability)
---------------------------------------------------------------
propNestedLoop :: Prop
propNestedLoop = mkBDDVar "NESTED_LOOP"

nested_s01 = Assignment "i" (Const 0) 10  -- i := 0
nested_s02 = Assignment "j" (Const 0) 11  -- j := 0

nested_test_outer = (LTExp (Variable "i") (Const 100), 12) -- while i < 100
nested_test_inner = (LTExp (Variable "j") (Const 50), 13)  -- while j < 50

nested_inner_s = Assignment "x" (Add (Variable "x") (Variable "j")) 14  -- x := x + j
nested_inner = While nested_test_inner (Variant propNestedLoop nested_inner_s (Skip 15))

nested_outer_s = Seq nested_inner (Assignment "i" (Add (Variable "i") (Const 1)) 16)
nested_outer = While nested_test_outer nested_outer_s

nested_variability = Seq nested_s01 (Seq nested_s02 nested_outer)

-- Evolution: Increase Inner Loop Bound
nested_variability_v2 = Seq nested_s01 (Seq nested_s02 (While (LTExp (Variable "j") (Const 100), 17) nested_outer))

---------------------------------------------------------------
-- 3. Interprocedural Simulation (interprocedural_sim)
---------------------------------------------------------------
propInterproc :: Prop
propInterproc = mkBDDVar "INTERPROC"

interproc_s01 = Assignment "x" (Const 1) 20  -- x := 1
interproc_s02 = Assignment "y" (Const 2) 21  -- y := 2

interproc_if_test = (GTExp (Variable "y") (Variable "x"), 22)
interproc_if_true = Assignment "z" (Mult (Variable "x") (Variable "y")) 23  -- z := x * y
interproc_if_false = Assignment "z" (Add (Variable "x") (Variable "y")) 24  -- z := x + y

interproc_if = IfThenElse interproc_if_test (Variant propInterproc interproc_if_true interproc_if_false) (Skip 25)

interprocedural_sim = Seq interproc_s01 (Seq interproc_s02 interproc_if)

-- Evolution: Add New Conditional Branch
interprocedural_sim_v2 = Seq interprocedural_sim (IfThenElse (GTExp (Variable "z") (Const 10), 26) (Assignment "w" (Const 1) 27) (Assignment "w" (Const 0) 28))

---------------------------------------------------------------
-- 4. Recursion Simulation (factorial_rec_sim)
---------------------------------------------------------------
propRecSim :: Prop
propRecSim = mkBDDVar "RECURSION_SIM"

rec_sim_s01 = Assignment "n" (Const 5) 30  -- n := 5
rec_sim_s02 = Assignment "result" (Const 1) 31  -- result := 1

rec_sim_test = (GTExp (Variable "n") (Const 1), 32)  -- while n > 1
rec_sim_s03 = Assignment "result" (Mult (Variable "result") (Variable "n")) 33  -- result := result * n
rec_sim_s04 = Assignment "n" (Sub (Variable "n") (Const 1)) 34  -- n := n - 1

rec_sim_body = Seq rec_sim_s03 rec_sim_s04
rec_sim_variant = Variant propRecSim rec_sim_body (Skip 35)

factorial_rec_sim = Seq rec_sim_s01 (Seq rec_sim_s02 (While rec_sim_test rec_sim_variant))

-- Evolution: Extend Iteration with Power Computation
factorial_rec_sim_v2 = Seq factorial_rec_sim (Assignment "power" (Mult (Variable "result") (Variable "n")) 36)

---------------------------------------------------------------
-- 5. Arithmetic-Heavy Program (arithmetic_heavy)
---------------------------------------------------------------
propArithHeavy :: Prop
propArithHeavy = mkBDDVar "ARITH_HEAVY"

arith_s01 = Assignment "a" (Const 42) 40  -- a := 42
arith_s02 = Assignment "b" (Const 99) 41  -- b := 99

arith_s03 = Assignment "c" (Mult (Variable "a") (Variable "b")) 42  -- c := a * b
arith_s04 = Assignment "d" (Div (Variable "c") (Const 3)) 43  -- d := c / 3
arith_s05 = Assignment "e" (Add (Variable "d") (Const 7)) 44  -- e := d + 7
arith_s06 = Assignment "f" (Sub (Variable "e") (Const 2)) 45  -- f := e - 2

arith_variant = Variant propArithHeavy arith_s06 (Skip 46)

arithmetic_heavy = Seq arith_s01 (Seq arith_s02 (Seq arith_s03 (Seq arith_s04 (Seq arith_s05 arith_variant))))

-- Evolution: Add Another Arithmetic Operation
arithmetic_heavy_v2 = Seq arithmetic_heavy (Assignment "g" (Add (Variable "f") (Const 10)) 47)

-- New Presence Conditions
propX1, propX2, propX3, propX4, propX5 :: Prop
propX1 = mkBDDVar "X1"
propX2 = mkBDDVar "X2"
propX3 = mkBDDVar "X3"
propX4 = mkBDDVar "X4"
propX5 = mkBDDVar "X5"

---------------------------------------------------------------
-- Example 6: Variational Initialization (init_variability)
---------------------------------------------------------------
init_var_s1 = Variant propX1 (Assignment "a" (Const 1) 1) (Assignment "a" (Const 10) 2)
init_var_s2 = Variant propX2 (Assignment "b" (Const 2) 3) (Assignment "b" (Const 20) 4)
init_var_s3 = Variant propX3 (Assignment "c" (Add (Variable "a") (Variable "b")) 5) (Assignment "c" (Sub (Variable "a") (Variable "b")) 6)
init_variability :: Program
init_variability = Seq init_var_s1 (Seq init_var_s2 init_var_s3)

-- Evolution: Add computation based on c
init_variability_v2 = Seq init_variability (Assignment "d" (Mult (Variable "c") (Const 3)) 7)

---------------------------------------------------------------
-- Example 7: Loop with Multiple Variants (loop_multi_variant)
---------------------------------------------------------------
loop_mv_s1 = Assignment "i" (Const 0) 10
loop_mv_s2 = Assignment "sum" (Const 0) 11
loop_mv_test = (LTExp (Variable "i") (Const 10), 12)
loop_mv_s_body_core = Assignment "sum" (Add (Variable "sum") (Variable "i")) 13
loop_mv_s_variant1 = Variant propX1 loop_mv_s_body_core (Skip 14)
loop_mv_s_variant2 = Variant propX2 loop_mv_s_variant1 (Skip 15)
loop_mv_s_variant3 = Variant propX3 loop_mv_s_variant2 (Skip 16)
loop_mv_inc = Assignment "i" (Add (Variable "i") (Const 1)) 17
loop_mv_body = Seq loop_mv_s_variant3 loop_mv_inc
loop_multi_variant :: Program
loop_multi_variant = Seq loop_mv_s1 (Seq loop_mv_s2 (While loop_mv_test loop_mv_body))

-- Evolution: Add final check after loop
loop_multi_variant_v2 = Seq loop_multi_variant (Assignment "result" (Mult (Variable "sum") (Const 2)) 18)

---------------------------------------------------------------
-- Example 8: Deeply Nested Variants (deep_nested_variants)
---------------------------------------------------------------
dnv_s1 = Variant propX1 
            (Variant propX2 
                (Variant propX3 (Assignment "x" (Const 1) 20)
                                 (Assignment "x" (Const 2) 21))
                (Variant propX4 (Assignment "x" (Const 3) 22)
                                 (Assignment "x" (Const 4) 23)))
            (Variant propX5 (Assignment "x" (Const 5) 24)
                            (Assignment "x" (Const 6) 25))
deep_nested_variants :: Program
deep_nested_variants = dnv_s1

-- Evolution: Add computation on x
deep_nested_variants_v2 = Seq deep_nested_variants (Assignment "y" (Add (Variable "x") (Const 100)) 26)

---------------------------------------------------------------
-- Fibonacci
---------------------------------------------------------------
fibonacci :: Program

fibonacci_s01 = Assignment "a" (Const 0) 11 -- a := 0 (Fib(0))
fibonacci_s02 = Assignment "b" (Const 1) 12 -- b := 1 (Fib(1))
fibonacci_s03 = Assignment "i" (Const 0) 13 -- i := 0 (loop counter)
fibonacci_s04 = Assignment "r" (Const 0) 14 -- r := 0 (result placeholder)

fibonacci_whileTest = (LTExp (Variable "i") (Variable "x"), 15) -- while i < x do
fibonacci_while_s01 = Assignment "r" (Variable "a") 16 -- r := a (store Fib(i))
fibonacci_while_s02 = Assignment "a" (Variable "b") 17 -- a := b (shift values)
fibonacci_while_s03 = Assignment "b" (Add (Variable "r") (Variable "b")) 18 -- b := r + b (Fib(i) + Fib(i+1))
fibonacci_while_s04 = Assignment "i" (Add (Variable "i") (Const 1)) 19  -- i := i + 1
fibonacci_while_s = Seq fibonacci_while_s01 (Seq fibonacci_while_s02 (Seq fibonacci_while_s03 fibonacci_while_s04))

fibonacci_while = While fibonacci_whileTest fibonacci_while_s

fibonacci = Seq fibonacci_s01 (Seq fibonacci_s02 (Seq fibonacci_s03 (Seq fibonacci_s04 fibonacci_while)))

---------------------------------------------------------------
-- Factorial
---------------------------------------------------------------
factorial :: Program

factorial_whileTest :: (BExp, Label)

factorial_s01 = Assignment "a" (Variable "x") 21 -- a := x (auxiliary)
factorial_s02 = Assignment "r" (Const 1) 22 -- r := 1 (result placeholder)

factorial_whileTest = (GTExp (Variable "a") (Const 1), 33) -- while a > 1 do
factorial_whileS1 = Assignment "r" (Mult (Variable "r") (Variable "a")) 34 -- fat(x) = x * a
factorial_whileS2 = Assignment "a" (Sub (Variable "a") (Const 1)) 35 -- a = a - 1
factorial_while = While factorial_whileTest (Seq factorial_whileS1 factorial_whileS2)

factorial = Seq factorial_s01 (Seq factorial_s02 factorial_while)

---------------------------------------------------------------
-- Combining Factorial, Fibonacci and Variability
---------------------------------------------------------------
combining_fib_fat_with_variability :: Program

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

combining_fib_fat_s01 = Assignment "x" (Const 5) 1 -- x := 5 (combining_fib_fat(x))
combining_fib_fat_s02 = Assignment "z" (Const 0) 2 -- z := 0 (result placeholder)

combining_fib_fat_at_s1 = factorial
combining_fib_fat_at_s2 = Assignment "z" (Variable "r") 3
combining_fib_fat_at_bt = Assignment "z" (Mult (Variable "z") (Variable "2")) 4
combining_fib_fat_at_bf = Assignment "z" (Div (Variable "z") (Variable "2")) 5

combining_fib_fat_at_b = Seq combining_fib_fat_at_s1 (Seq combining_fib_fat_at_s2 (Variant propB combining_fib_fat_at_bt combining_fib_fat_at_bf))

combining_fib_fat_af_s1 = fibonacci
combining_fib_fat_af_s2 = Assignment "z" (Variable "r") 6
combining_fib_fat_af_bt = Assignment "z" (Add (Variable "z") (Variable "1")) 7
combining_fib_fat_af_bf = Assignment "z" (Sub (Variable "z") (Variable "1")) 8

combining_fib_fat_af_b = Seq combining_fib_fat_af_s1 (Seq combining_fib_fat_af_s2 (Variant propB combining_fib_fat_af_bt combining_fib_fat_af_bf))

combining_fib_fat_s03_a_b = Variant propA combining_fib_fat_at_b combining_fib_fat_af_b

combining_fib_fat_with_variability = Seq combining_fib_fat_s01 (Seq combining_fib_fat_s02 combining_fib_fat_s03_a_b)

---------------------------------------------------------------
-- Combining Factorial, Fibonacci and Variability / atbt
---------------------------------------------------------------
combining_fib_fat_atbt :: Program

combining_fib_fat_s03_atbt = Seq combining_fib_fat_at_s1 (Seq combining_fib_fat_at_s2 combining_fib_fat_at_bt)

combining_fib_fat_atbt = Seq combining_fib_fat_s01 (Seq combining_fib_fat_s02 combining_fib_fat_s03_atbt)

---------------------------------------------------------------
-- Combining Factorial, Fibonacci and Variability / atbf
---------------------------------------------------------------
combining_fib_fat_atbf :: Program

combining_fib_fat_s03_atbf = Seq combining_fib_fat_at_s1 (Seq combining_fib_fat_at_s2 combining_fib_fat_at_bf)

combining_fib_fat_atbf = Seq combining_fib_fat_s01 (Seq combining_fib_fat_s02 combining_fib_fat_s03_atbf)

---------------------------------------------------------------
-- Combining Factorial, Fibonacci and Variability / afbt
---------------------------------------------------------------
combining_fib_fat_afbt :: Program

combining_fib_fat_s03_afbt = Seq combining_fib_fat_af_s1 (Seq combining_fib_fat_af_s2 combining_fib_fat_af_bt)

combining_fib_fat_afbt = Seq combining_fib_fat_s01 (Seq combining_fib_fat_s02 combining_fib_fat_s03_afbt)

---------------------------------------------------------------
-- Combining Factorial, Fibonacci and Variability / afbf
---------------------------------------------------------------
combining_fib_fat_afbf :: Program

combining_fib_fat_s03_afbf = Seq combining_fib_fat_af_s1 (Seq combining_fib_fat_af_s2 combining_fib_fat_af_bf)

combining_fib_fat_afbf = Seq combining_fib_fat_s01 (Seq combining_fib_fat_s02 combining_fib_fat_s03_afbf)
