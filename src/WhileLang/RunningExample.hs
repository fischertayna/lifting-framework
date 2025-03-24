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
propUnsafeDiv :: Prop
propUnsafeDiv = mkBDDVar "UNSAFE_DIV"

s1 = Assignment "x" (Const 10) 1
s2 = Assignment "y" (Const 2) 2
s3 = Variant propUnsafeDiv
        (Assignment "y" (Const 0) 3)    -- Variant A (unsafe): overwrites y := 0
        (Assignment "x" (Const 0) 4)    -- Variant B (safe): overwrites x := 0
s4 = Assignment "z" (Div (Variable "x") (Variable "y")) 5

divisionByZeroExample :: Program
divisionByZeroExample = Seq s1 (Seq s2 (Seq s3 s4))

---------------------------------------------------------------
-- Running Example - Presentation
---------------------------------------------------------------

propSanitize :: Prop
propSanitize = mkBDDVar "SANITIZE"

loadPassword :: Stmt
loadPassword = Assignment "pwd" (Variable "input") 1

sanitizeOrNot :: Stmt
sanitizeOrNot = Variant propSanitize
  (Assignment "pwd" (Variable "sanitized_input") 2) 
  (Assignment "pwd" (Mult (Variable "pwd") (Variable "pwd")) 3)                                          

usePassword :: Stmt
usePassword = Assignment "result" (Variable "pwd") 4

passwordSanitizationExample :: Program
passwordSanitizationExample = Seq loadPassword (Seq sanitizeOrNot usePassword)