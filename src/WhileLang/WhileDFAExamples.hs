module WhileLang.WhileDFAExamples where
import WhileLang.WhileEncoder
    ( Stmt(..),
      BExp(..),
      AExp(..),
      Program,
      Label )
import Variability.VarTypes(Prop, VarValor(..), Var (Var), propA, propB, atbt, atbf, afbt, afbf)

rdS01, rdS02, rdWhileS1, rdWhileS2, rdS03 :: Stmt
rdWhileTest :: (BExp, Label)

---------------------------------------------------------------
-- Examples taken from PPA
---------------------------------------------------------------

---------------------------------------------------------------
-- Reaching Definitions
---------------------------------------------------------------
rdS01 = Assignment "x" (Const 5) 1
rdS02 = Assignment "y" (Const 1) 2
rdWhileTest = (GTExp (Variable "x") (Const 1), 3)
rdWhileS1 = Assignment "y" (Mult (Variable "x") (Variable "y")) 4
rdWhileS2 = Assignment "x" (Sub (Variable "x") (Const 1)) 5
rdS03 = While rdWhileTest (Seq rdWhileS1 rdWhileS2)

rdExample :: Program
rdExample = Seq rdS01 (Seq rdS02 rdS03)

---------------------------------------------------------------
-- Live Variables
---------------------------------------------------------------
lvS01, lvS02, lvS03, lvS04, lvThenS1, lvThenS2, lvS05 :: Stmt
lvIfTest :: (BExp, Label)

lvS01 = Assignment "x" (Const 2) 1
lvS02 = Assignment "y" (Const 4) 2
lvS03 = Assignment "x" (Const 1) 3

lvIfTest = (GTExp (Variable "y") (Variable "x"), 4)
lvThenS1 = Assignment "z" (Variable "y") 5
lvThenS2 = Assignment "z" (Mult (Variable "y") (Variable "y")) 6
lvS04 = IfThenElse lvIfTest lvThenS1 lvThenS2

lvS05 = Assignment "x" (Variable "z") 7

lvExample :: Program
lvExample = Seq lvS01 (Seq lvS02 (Seq lvS03 (Seq lvS04 lvS05)))

---------------------------------------------------------------
-- Available Expressions
---------------------------------------------------------------
aeS01, aeS02, aeS03, aeWhileS1, aeWhileS2 :: Stmt
aeWhileTest :: (BExp, Label)

aeS01 = Assignment "x" (Add (Variable "a") (Variable "b")) 1
aeS02 = Assignment "y" (Mult (Variable "a") (Variable "b")) 2

aeWhileTest = (GTExp (Variable "y") (Add (Variable "a") (Variable "b")), 3)
aeWhileS1 = Assignment "a" (Add (Variable "a") (Const 1)) 4
aeWhileS2 = Assignment "x" (Add (Variable "a") (Variable "b")) 5
aeS03 = While aeWhileTest (Seq aeWhileS1 aeWhileS2)

aeExample :: Program
aeExample = Seq aeS01 (Seq aeS02 aeS03)

---------------------------------------------------------------
-- Very Busy Expressions
---------------------------------------------------------------
vbS01, vbS02, vbS03, vbS04, vbS05, vbThenS1, vbThenS2 :: Stmt
vbIfTest :: (BExp, Label)

vbS02 = Assignment "x" (Sub (Variable "b") (Variable "a")) 2
vbS03 = Assignment "y" (Sub (Variable "a") (Variable "b")) 3

vbS04 = Assignment "y" (Sub (Variable "b") (Variable "a")) 4
vbS05 = Assignment "x" (Sub (Variable "a") (Variable "b")) 5

vbIfTest = (GTExp (Variable "a") (Variable "b"), 1)
vbThenS1 = Seq vbS02 vbS03
vbThenS2 = Seq vbS04 vbS05
vbS01 =  IfThenElse vbIfTest vbThenS1 vbThenS2

vbExample :: Program
vbExample = vbS01

---------------------------------------------------------------
-- Power
---------------------------------------------------------------
powerS01, powerS02, powerWhileS1, powerWhileS2 :: Stmt
powerWhileTest :: (BExp, Label)

powerS01 = Assignment "z" (Const 1) 1

powerWhileTest = (GTExp (Variable "x") (Const 0), 2)
powerWhileS1 = Assignment "z" (Mult (Variable "z") (Variable "y")) 3
powerWhileS2 = Assignment "x" (Sub (Variable "x") (Const 1)) 4
powerS02 = While powerWhileTest (Seq powerWhileS1 powerWhileS2)

power :: Program
power = Seq powerS01 powerS02

---------------------------------------------------------------
-- Example with Variability
---------------------------------------------------------------
ex2S01, ex2S02_3, ex2S04 :: Stmt

ex2S01 = Assignment "x" (Const 1) 1
ex2S02_3 = Variant propA (Assignment "y" (Const 1) 2) (Assignment "y" (Const 4) 3)
ex2S04 = Assignment "z" (Add (Variable "x") (Variable "y")) 4

ex2While = Seq ex2S01 (Seq ex2S02_3 ex2S04)