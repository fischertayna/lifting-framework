module Language.Interpreter.Driver where

import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.Interpreter.Interpreter

import Language.Frontend.ErrM
import Valor (Valor(..))

main :: IO ()
main = do
  interact calc
  putStrLn ""

input :: Valor
input = ValorInt 10

executeProg :: String -> Valor -> Valor
executeProg prog input =
  let Ok p = pProgram (myLexer prog)
  in (evalP p input)

calc :: String -> String
calc s =
  let r = executeProg s input
  in show r
