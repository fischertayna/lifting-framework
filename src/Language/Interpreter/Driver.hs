import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.Interpreter.Interpreter

import Language.Frontend.ErrM

main :: IO ()
main = do
  interact calc
  putStrLn ""

input :: Integer
input = 6 

calc :: String -> String
calc s =
  let Ok p = pProgram  (myLexer s)
  in show (evalP p input)
