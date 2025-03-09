module WhileLang.ComplexExamples where

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