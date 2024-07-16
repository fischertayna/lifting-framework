module Language.MInterpreter.Driver where


import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.MInterpreter.Interpreter

import Language.Frontend.ErrM
import Memoization.Core.State ((<.>), State (runState))
import Memoization.Core.Memory (KeyValueArray)

main :: IO ()
main = do
  interact calc
  putStrLn ""

inputL:: Valor
inputL = ValorList [ValorInt 10, ValorInt 12, ValorInt 13]

initialState :: KeyValueArray [Valor] Valor
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

executeProg :: String -> String -> Valor -> (Valor, Mem)
executeProg memoizedFunctionName prog input =
  let Ok p = pProgram (myLexer prog)
  in runState (evalP p memoizedFunctionName <.> return input) initialState

calc :: String -> String
calc s =
  let r = executeProg memoizedFunctionName s inputL
  in show $ r
