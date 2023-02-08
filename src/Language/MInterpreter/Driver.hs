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

input :: Integer
input = 6

initialState :: KeyValueArray [Integer] Integer
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

calc :: String -> String
calc s =
  let Ok p = pProgram  (myLexer s)
  in show $ runState (evalP p memoizedFunctionName <.> return input) initialState
