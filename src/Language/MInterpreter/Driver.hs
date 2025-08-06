module Language.MInterpreter.Driver where

import Language.Frontend.LexLanguage
import Language.Frontend.ErrM
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage

import Language.MInterpreter.Interpreter
import Base.Types (Valor(..))
import Memoization.Core.State ((<.>), State (runState))
import Memoization.Core.Memory (KeyValueArray, FuncKey(..))

main :: IO ()
main = do
  interact calc
  putStrLn ""

inputI :: Valor
inputI = ValorInt 10

inputL:: Valor
inputL = ValorList [ValorInt 10, ValorInt 12, ValorInt 13]

initialState :: KeyValueArray FuncKey Valor
initialState = []

memoizedNames :: [String]
memoizedNames = ["fib"]

executeProg :: [String] -> Mem -> String -> Valor -> (Valor, Mem)
executeProg memoizedFunctionNames initialState prog input =
  let Ok p = pProgram (myLexer prog)
  in (evalP p memoizedFunctionNames input) initialState

calc :: String -> String
calc s =
  let r = executeProg memoizedNames initialState s inputI
  in show $ r
