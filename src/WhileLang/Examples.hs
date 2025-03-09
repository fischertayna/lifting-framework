module WhileLang.Examples where
    
import WhileLang.WhileEncoder
    ( Stmt(..),
      BExp(..),
      AExp(..),
      Program,
      Label )
import Variability.VarTypes(Prop, VarValor(..), Var (Var), mkBDDVar, propA, propB, atbt, atbf, afbt, afbf)

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
