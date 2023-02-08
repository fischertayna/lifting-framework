module Language.VInterpreter.Driver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)
import Language.VInterpreter.Interpreter (VarInteger, evalPV)
import Variability.VarTypes (PresenceCondition, Var (Var), ttPC, ffPC)

main :: IO ()
main = do
  interact calc
  putStrLn ""

pc1 :: PresenceCondition
pc1 = ttPC

pc2 :: PresenceCondition
pc2 = ffPC

inputParam :: VarInteger
inputParam = Var [(6, pc1), (3, pc2)]

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s)
   in show $ evalPV p inputParam
