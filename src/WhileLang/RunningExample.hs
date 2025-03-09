module WhileLang.RunningExample where
    
import WhileLang.WhileEncoder
    ( Stmt(..),
      BExp(..),
      AExp(..),
      Program,
      Label )
import Variability.VarTypes(Prop, VarValor(..), Var (Var), mkBDDVar)

---------------------------------------------------------------
-- Running Example
---------------------------------------------------------------
running_example_variability :: Program

propAltInit :: Prop
propAltInit = mkBDDVar "ALT_INIT"

propFastFib :: Prop
propFastFib = mkBDDVar "FAST_FIB"

re_s01 = Assignment "x" (Const 5) 1 -- [x := 5]1
re_s01_v2 = Assignment "x" (Const 6) 1 -- [x := 6]1

re_v1_if = Assignment "a" (Const 1) 2 -- [a := 1]2 
re_v1_else = Assignment "a" (Const 0) 3 -- [a := 0]3
re_v1 = Variant propAltInit re_v1_if re_v1_else -- #IFDEF ALT_INIT

re_s02 = Assignment "b" (Const 1) 4 -- [b := 1]4
re_s03 = Assignment "i" (Const 0) 5 -- [i := 0]5
re_s04 = Assignment "r" (Const 0) 6 -- [r := 0]6

re_while_test =  (LTExp (Variable "i") (Variable "x"), 7) -- [i < x]7

re_while_v2_if =  Assignment "r" (Variable "b") 8 -- [r := b]8

re_while_v2_else_s01 =  Assignment "r" (Variable "a") 9 -- [r := a]9
re_while_v2_else_s02 =  Assignment "a" (Variable "b") 10 -- [a := b]10
re_while_v2_else = Seq re_while_v2_else_s01 re_while_v2_else_s02

re_while_v2 = Variant propFastFib re_while_v2_if re_while_v2_else  -- #IFDEF FAST_FIB

re_while_s01 =  Assignment "b" (Add (Variable "r") (Variable "b")) 11 -- [b := r + b]11
re_while_s02 =  Assignment "i" (Add (Variable "i") (Const 1)) 12 -- [i := i + 1]12

running_example_variability = Seq re_s01 (Seq re_v1 (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2 (Seq re_while_s01 re_while_s02)))))))

running_example_variability_v2 = Seq re_s01_v2 (Seq re_v1 (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2 (Seq re_while_s01 re_while_s02)))))))
----------------------------------------------------------------
-- Running Example ALT_INIT false FAST_FIB false
---------------------------------------------------------------
running_example_variability_alt_f_fast_f :: Program

running_example_variability_alt_f_fast_f = Seq re_s01 (Seq re_v1_else (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2_else (Seq re_while_s01 re_while_s02)))))))

----------------------------------------------------------------
-- Running Example ALT_INIT true FAST_FIB false
---------------------------------------------------------------
running_example_variability_alt_t_fast_f :: Program

running_example_variability_alt_t_fast_f = Seq re_s01 (Seq re_v1_if (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2_else (Seq re_while_s01 re_while_s02)))))))

----------------------------------------------------------------
-- Running Example ALT_INIT false FAST_FIB true
---------------------------------------------------------------
running_example_variability_alt_f_fast_t :: Program

running_example_variability_alt_f_fast_t = Seq re_s01 (Seq re_v1_else (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2_if (Seq re_while_s01 re_while_s02)))))))

----------------------------------------------------------------
-- Running Example ALT_INIT true FAST_FIB true
---------------------------------------------------------------
running_example_variability_alt_t_fast_t :: Program

running_example_variability_alt_t_fast_t = Seq re_s01 (Seq re_v1_if (Seq re_s02 (Seq re_s03 (Seq re_s04 (While re_while_test (Seq re_while_v2_if (Seq re_while_s01 re_while_s02)))))))





