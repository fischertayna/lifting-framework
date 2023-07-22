module Language.Typechecker.Driver where

import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.Typechecker.Typechecker
-- import Language.Interpreter.Interpreter
import Language.Frontend.ErrM

main :: IO ()
main = do
  interact calc
  putStrLn ""

input :: Valor
input = ValorInt 10

calc :: String -> String
calc s = 
  let Ok p = pProgram  (myLexer s) 
  in show (typeCheckP p input)
