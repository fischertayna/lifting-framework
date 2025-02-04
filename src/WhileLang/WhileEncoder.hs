module WhileLang.WhileEncoder where

import Variability.VarTypes

type Id = String 
type Label = Integer
type Program = Stmt
type TestExp = (BExp, Label)

-- AST Representation of Arithmetic Expressions
data AExp = Variable Id            -- variables
          | Const Integer     -- constants
          | Add AExp AExp     
          | Sub AExp AExp
          | Mult AExp AExp
          | Div AExp AExp 
 deriving(Eq, Ord)

instance Show AExp where
    show (Variable var) = show var
    show (Const int) = show int
    show (Add a1 a2) = show a1 <> "+" <> show a2
    show (Sub a1 a2) = show a1 <> "-" <> show a2
    show (Mult a1 a2) = show a1 <> "*" <> show a2
    show (Div a1 a2) = show a1 <> "/" <> show a2

-- AST Representation of Boolean Expressions
data BExp = CTrue             -- True constant
          | CFalse            -- False constant
          | Not BExp
          | And BExp BExp
          | Or  BExp BExp
          | EQExp AExp AExp
          | GTExp AExp AExp
          | LTExp AExp AExp
 deriving(Eq, Ord)  

instance Show BExp where
    show (CTrue) = "true"
    show (CFalse) = "false"
    show (Not b) = "¬" <> show b
    show (And b1 b2) = show b1 <> "&" <> show b2
    show (Or b1 b2) = show b1 <> "||" <> show b2
    show (EQExp a1 a2) = show a1 <> "==" <> show a2
    show (GTExp a1 a2) = show a1 <> ">" <> show a2
    show (LTExp a1 a2) = show a1 <> "<" <> show a2

-- AST Representation of Statements
data Stmt = Assignment Id AExp Label    -- Assignment
          | Skip Label
          | Seq Stmt Stmt
          | IfThenElse TestExp Stmt Stmt
          | While TestExp Stmt
          | Variant PresenceCondition Stmt Stmt -- Handles #IFDEF
 deriving(Eq, Ord)

instance Show Stmt where
    show (Assignment var a l) = "[" <> show var <> ":=" <> show a <> "] " <> show l
    show (Skip l) = "[skip] " <> show l
    show (Seq s1 s2) = show s1 <> "; " <> show s2
    show (IfThenElse test s1 s2) = "if " <> show test <> " then " <> show s1 <> " else " <> show s2
    show (While (test, l) s) = "while " <> "[" <> show test <> "] " <> show l <> " do " <> show s
    show (Variant pc s1 s2) = "#IFDEF " <> show pc <> " \n " <> show s1 <> "\n #ELSE \n" <> show s2 <> " #ENDIF "

-- Encoder to convert AST into VarValor
encodeAExp :: AExp -> VarValor
encodeAExp (Variable v) = VarPair (VarString (Var [("VAR", ttPC)]), VarString (Var [(v, ttPC)]))
encodeAExp (Const i) = VarPair (VarString (Var [("CONST", ttPC)]), VarString (Var [(show i, ttPC)]))
encodeAExp (Add e1 e2) = VarPair (VarString (Var [("ADD", ttPC)]), VarPair (encodeAExp e1, encodeAExp e2))
encodeAExp (Sub e1 e2) = VarPair (VarString (Var [("SUB", ttPC)]), VarPair (encodeAExp e1, encodeAExp e2))
encodeAExp (Mult e1 e2) = VarPair (VarString (Var [("MULT", ttPC)]), VarPair (encodeAExp e1, encodeAExp e2))
encodeAExp (Div e1 e2) = VarPair (VarString (Var [("DIV", ttPC)]), VarPair (encodeAExp e1, encodeAExp e2))

encodeBExp :: BExp -> VarValor
encodeBExp CTrue = VarString (Var [("TRUE", ttPC)])
encodeBExp CFalse = VarString (Var [("FALSE", ttPC)])
encodeBExp (Not b) = VarPair (VarString (Var [("NOT", ttPC)]), encodeBExp b)
encodeBExp (And b1 b2) = VarPair (VarString (Var [("AND", ttPC)]), VarPair (encodeBExp b1, encodeBExp b2))
encodeBExp (Or b1 b2) = VarPair (VarString (Var [("OR", ttPC)]), VarPair (encodeBExp b1, encodeBExp b2))
encodeBExp (EQExp a1 a2) = VarPair (VarString (Var [("EQ", ttPC)]), VarPair (encodeAExp a1, encodeAExp a2))
encodeBExp (GTExp a1 a2) = VarPair (VarString (Var [("GT", ttPC)]), VarPair (encodeAExp a1, encodeAExp a2))
encodeBExp (LTExp a1 a2) = VarPair (VarString (Var [("LT", ttPC)]), VarPair (encodeAExp a1, encodeAExp a2))

encodeStmt :: Stmt -> VarValor
encodeStmt (Assignment v e l) = 
    VarPair (VarString (Var [("ASGN", ttPC)]),
             VarPair (VarString (Var [(show l, ttPC)]),
                      VarPair (VarString (Var [(v, ttPC)]), encodeAExp e)))
encodeStmt (Seq s1 s2) = VarPair (VarString (Var [("SEQ", ttPC)]), VarPair (encodeStmt s1, encodeStmt s2))
encodeStmt (IfThenElse (cond, l) s1 s2) = 
    VarPair (VarString (Var [("IF", ttPC)]),
             VarPair (VarPair (encodeBExp cond, VarString (Var [(show l, ttPC)])),
                      VarPair (encodeStmt s1, encodeStmt s2)))
encodeStmt (While (cond, l) body) = 
    VarPair (VarString (Var [("WHILE", ttPC)]),
             VarPair (VarPair (encodeBExp cond, VarString (Var [(show l, ttPC)])), encodeStmt body))
encodeStmt (Skip l) = VarString (Var [("SKIP", ttPC)])
encodeStmt (Variant pc s1 s2) = 
    VarPair (encodeStmt s1 |||| pc, encodeStmt s2 |||| notBDD pc)
