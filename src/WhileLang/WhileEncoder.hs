module WhileLang.WhileEncoder where

import Variability.VarTypes

import Debug.Trace (trace)
import Base.Types (Valor(..))

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

data StmtPC = AssignmentPC Id AExp Label PresenceCondition
            | SkipPC Label PresenceCondition
            | SeqPC StmtPC StmtPC
            | IfThenElsePC TestExp StmtPC StmtPC PresenceCondition
            | WhilePC TestExp StmtPC PresenceCondition
 deriving (Eq, Ord, Show)

-- Encoder to convert AST into VarValor
encodeAExpToVarValor :: AExp -> PresenceCondition -> VarValor
encodeAExpToVarValor (Variable v) pc = VarPair (VarString (Var [("VAR", pc)]), VarString (Var [(v, pc)]))
encodeAExpToVarValor (Const i) pc = VarPair (VarString (Var [("CONST", pc)]), VarString (Var [(show i, pc)]))
encodeAExpToVarValor (Add e1 e2) pc = VarPair (VarString (Var [("ADD", pc)]), VarPair (encodeAExpToVarValor e1 pc, encodeAExpToVarValor e2 pc))
encodeAExpToVarValor (Sub e1 e2) pc = VarPair (VarString (Var [("SUB", pc)]), VarPair (encodeAExpToVarValor e1 pc, encodeAExpToVarValor e2 pc))
encodeAExpToVarValor (Mult e1 e2) pc = VarPair (VarString (Var [("MULT", pc)]), VarPair (encodeAExpToVarValor e1 pc, encodeAExpToVarValor e2 pc))
encodeAExpToVarValor (Div e1 e2) pc = VarPair (VarString (Var [("DIV", pc)]), VarPair (encodeAExpToVarValor e1 pc, encodeAExpToVarValor e2 pc))

encodeBExpToVarValor :: BExp -> PresenceCondition -> VarValor
encodeBExpToVarValor CTrue pc = VarString (Var [("TRUE", pc)])
encodeBExpToVarValor CFalse pc = VarString (Var [("FALSE", pc)])
encodeBExpToVarValor (Not b) pc = VarPair (VarString (Var [("NOT", pc)]), encodeBExpToVarValor b pc)
encodeBExpToVarValor (And b1 b2) pc = VarPair (VarString (Var [("AND", pc)]), VarPair (encodeBExpToVarValor b1 pc, encodeBExpToVarValor b2 pc))
encodeBExpToVarValor (Or b1 b2) pc = VarPair (VarString (Var [("OR", pc)]), VarPair (encodeBExpToVarValor b1 pc, encodeBExpToVarValor b2 pc))
encodeBExpToVarValor (EQExp a1 a2) pc = VarPair (VarString (Var [("EQ", pc)]), VarPair (encodeAExpToVarValor a1 pc, encodeAExpToVarValor a2 pc))
encodeBExpToVarValor (GTExp a1 a2) pc = VarPair (VarString (Var [("GT", pc)]), VarPair (encodeAExpToVarValor a1 pc, encodeAExpToVarValor a2 pc))
encodeBExpToVarValor (LTExp a1 a2) pc = VarPair (VarString (Var [("LT", pc)]), VarPair (encodeAExpToVarValor a1 pc, encodeAExpToVarValor a2 pc))

stmtToStmtPC :: PresenceCondition -> Stmt -> StmtPC
stmtToStmtPC pc (Assignment v e l) = AssignmentPC v e l pc
stmtToStmtPC pc (Skip l) = SkipPC l pc
stmtToStmtPC pc (Seq s1 s2) = SeqPC (stmtToStmtPC pc s1) (stmtToStmtPC pc s2)
stmtToStmtPC pc (IfThenElse test s1 s2) = 
    IfThenElsePC test (stmtToStmtPC pc s1) (stmtToStmtPC pc s2) pc
stmtToStmtPC pc (While test body) = WhilePC test (stmtToStmtPC pc body) pc
stmtToStmtPC pc (Variant pc' s1 s2) =
    SeqPC (stmtToStmtPC (andBDD pc pc') s1) (stmtToStmtPC (andBDD pc (notBDD pc')) s2)

isCompletePC :: PresenceCondition -> Bool
isCompletePC pc = pc == ttPC || pc == ffPC

presencePairs :: String ->  String -> PresenceCondition -> [(String, PresenceCondition)]
presencePairs tag alternativeTag pc
    | isCompletePC pc = [(tag, pc)]
    | otherwise       = [(tag, pc), (alternativeTag, notBDD pc)]

presencePairsStmt :: String -> PresenceCondition -> [(String, PresenceCondition)]
presencePairsStmt tag pc = presencePairs tag "SKIP" pc

presencePairsLabel :: String -> PresenceCondition -> [(String, PresenceCondition)]
presencePairsLabel tag pc = presencePairs tag ("-" ++ tag) pc

encodeStmt :: Stmt -> VarValor
encodeStmt stmt = encodeStmtPC' (stmtToStmtPC ttPC stmt)
    -- let stmtPC = stmtToStmtPC ttPC stmt
    --     tracedStmtPC = trace ("\n stmtToStmtPC result: " ++ substitute (show stmtPC)) stmtPC
    --     result = encodeStmtPC' tracedStmtPC
    -- in trace ("\n encodeStmt final result: " ++ substitute (show result)) result
  where
    encodeStmtPC' :: StmtPC -> VarValor
    encodeStmtPC' (AssignmentPC v e l pc) = 
        VarPair (VarString (Var (presencePairsStmt "ASGN" pc)), 
                 VarPair (VarString (Var (presencePairsLabel (show l) pc)),
                          VarPair (VarString (Var [(v, pc)]), encodeAExpToVarValor e pc)))

    encodeStmtPC' (SkipPC l pc) = VarString (Var [("SKIP", pc)])

    encodeStmtPC' (SeqPC s1 s2) = 
        VarPair (VarString (Var (presencePairsStmt "SEQ" ttPC)), 
                 VarPair (encodeStmtPC' s1, encodeStmtPC' s2))

    encodeStmtPC' (IfThenElsePC (cond, l) s1 s2 pc) =
        VarPair (VarString (Var (presencePairsStmt "IF" pc)),
                 VarPair (VarString (Var (presencePairsLabel (show l) pc)),
                          VarPair (encodeBExpToVarValor cond pc,
                                   VarPair (encodeStmtPC' s1, encodeStmtPC' s2))))

    encodeStmtPC' (WhilePC (cond, l) body pc) =
        VarPair (VarString (Var (presencePairsStmt "WHILE" pc)),
                 VarPair (VarString (Var (presencePairsLabel (show l) pc)),
                          VarPair (encodeBExpToVarValor cond pc, encodeStmtPC' body)))

--- Encoder to Valor

encodeAExpToValor :: AExp -> Valor
encodeAExpToValor (Variable v) = ValorPair (ValorStr "VAR", ValorStr v)
encodeAExpToValor (Const i) = ValorPair (ValorStr "CONST", ValorStr (show i))
encodeAExpToValor (Add e1 e2) = ValorPair (ValorStr "ADD", ValorPair (encodeAExpToValor e1, encodeAExpToValor e2))
encodeAExpToValor (Sub e1 e2) = ValorPair (ValorStr "SUB", ValorPair (encodeAExpToValor e1, encodeAExpToValor e2))
encodeAExpToValor (Mult e1 e2) = ValorPair (ValorStr "MULT", ValorPair (encodeAExpToValor e1, encodeAExpToValor e2))
encodeAExpToValor (Div e1 e2) = ValorPair (ValorStr "DIV", ValorPair (encodeAExpToValor e1, encodeAExpToValor e2))

encodeBExpToValor :: BExp -> Valor
encodeBExpToValor CTrue = ValorStr "TRUE"
encodeBExpToValor CFalse = ValorStr "FALSE"
encodeBExpToValor (Not b) = ValorPair (ValorStr "NOT", encodeBExpToValor b)
encodeBExpToValor (And b1 b2) = ValorPair (ValorStr "AND", ValorPair (encodeBExpToValor b1, encodeBExpToValor b2))
encodeBExpToValor (Or b1 b2) = ValorPair (ValorStr "OR", ValorPair (encodeBExpToValor b1, encodeBExpToValor b2))
encodeBExpToValor (EQExp a1 a2) = ValorPair (ValorStr "EQ", ValorPair (encodeAExpToValor a1, encodeAExpToValor a2))
encodeBExpToValor (GTExp a1 a2) = ValorPair (ValorStr "GT", ValorPair (encodeAExpToValor a1, encodeAExpToValor a2))
encodeBExpToValor (LTExp a1 a2) = ValorPair (ValorStr "LT", ValorPair (encodeAExpToValor a1, encodeAExpToValor a2))

encodeStmtToValor :: Stmt -> Valor
encodeStmtToValor (Assignment v e l) = 
        ValorPair (ValorStr "ASGN", 
                 ValorPair (ValorStr (show l),
                          ValorPair (ValorStr v, encodeAExpToValor e)))
encodeStmtToValor (Skip l) = ValorStr "SKIP"
encodeStmtToValor (Seq s1 s2) = 
        ValorPair (ValorStr "SEQ", 
                 ValorPair (encodeStmtToValor s1, encodeStmtToValor s2))
encodeStmtToValor (IfThenElse (cond, l) s1 s2) =
        ValorPair (ValorStr "IF",
                 ValorPair (ValorStr (show l),
                          ValorPair (encodeBExpToValor cond,
                                   ValorPair (encodeStmtToValor s1, encodeStmtToValor s2))))
encodeStmtToValor (While (cond, l) body) =
        ValorPair (ValorStr "WHILE",
                 ValorPair (ValorStr (show l),
                          ValorPair (encodeBExpToValor cond, encodeStmtToValor body)))