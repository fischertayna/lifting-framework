import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.Interpreter.Interpreter

import Language.Frontend.ErrM

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok p = pProgram  (myLexer s)
  in show (evalP p)
