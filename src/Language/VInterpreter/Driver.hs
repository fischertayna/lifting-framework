module Language.VInterpreter.Driver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)
import Language.VInterpreter.Interpreter
import Variability.VarTypes (PresenceCondition, Var (Var), ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import System.IO.Unsafe

main :: IO ()
main = do
  interact calc
  putStrLn ""

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

atbt :: Prop
atbt = propA /\ propB

atbf :: Prop
atbf = propA /\ notBDD propB

afbt :: Prop
afbt = notBDD propA /\ propB

afbf :: Prop
afbf = notBDD propA /\ notBDD propB

inputInt :: VarValor
inputInt = VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)])

inputBool :: VarValor
inputBool = VarBool (Var [(True, atbt), ( False, afbt),  (False, atbf), (False, afbf)])

inputString :: VarValor
inputString = VarString (Var [("abc", atbt), ( "def", afbt),  ("ghi", atbf), ("jkl", afbf)])

listMod :: Maybe Int
listMod = Just 2

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s)
   in show $ evalPV p listMod inputInt
