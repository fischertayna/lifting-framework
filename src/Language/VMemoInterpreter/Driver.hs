module Language.VMemoInterpreter.Driver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)

import Language.VMemoInterpreter.Interpreter
import Memoization.Core.State ((<.>), State (runState))
import Variability.VarTypes (PresenceCondition, VarValor(..), Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import Memoization.Core.Memory (KeyValueArray, FuncKey(..))

main :: IO ()
main = do
  interact calc
  putStrLn ""

pcA :: Prop
pcA = mkBDDVar "A"

varInt, varList :: VarValor
varInt = VarInteger (Var [(2, pcA), (3, notBDD pcA)])
varList = VarList [
                      VarInteger (Var [(8, pcA), ( 5, notBDD pcA)]),
                      VarInteger (Var [(2, pcA), ( 1, notBDD pcA)]),
                      VarInteger (Var [(3, pcA), ( 2, notBDD pcA)])
                    ]

initialState :: KeyValueArray FuncKey VarValor
initialState = []

memoizedNames :: [String]
memoizedNames = ["fib"]

executeProg :: [String] -> String -> VarValor -> (VarValor, Mem)
executeProg memoizedFunctionNames prog input =
  let Ok p = pProgram (myLexer prog)
  in (evalP p memoizedFunctionNames input) initialState

calc :: String -> String
calc s =
  let r = executeProg memoizedNames s varList
  in show $ r