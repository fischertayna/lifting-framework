import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.Frontend.AbsLanguage
import Language.MInterpreter.Interpreter

import Language.Frontend.ErrM
import Memoization.Core.State ((<.>), State (runState))

main :: IO ()
main = do
  interact calc
  putStrLn ""

input :: Integer
input = 6

initialState :: [Integer]
initialState = []

calc :: String -> String
calc s =
  let Ok p = pProgram  (myLexer s)
  in show $ runState (evalP p <.> return input) initialState
