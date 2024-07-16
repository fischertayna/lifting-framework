module Language.DeepMemo.Driver where


import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.DeepMemo.Interpreter

import Language.Frontend.ErrM
import Memoization.Core.State ((<.>), State (runState))
import Variability.VarTypes (PresenceCondition, VarValor(..), Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import Memoization.Core.Memory (KeyValueArray)

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

initialState :: KeyValueArray [VarValor] VarValor
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

executeProg :: String -> String -> VarValor -> (VarValor, Mem)
executeProg memoizedFunctionName prog input =
  let Ok p = pProgram (myLexer prog)
  in runState (evalP p memoizedFunctionName <.> return input) initialState

calc :: String -> String
calc s =
  let r = executeProg memoizedFunctionName s varList
  in show $ r