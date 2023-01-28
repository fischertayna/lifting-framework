import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.VInterpreter.Interpreter

main :: IO ()
main = do
  interact calc
  putStrLn ""

pc1 :: PresenceCondition
pc1 = True

pc2 :: PresenceCondition
pc2 = False

inputParam :: VarInt
inputParam = Var [(6, pc1), (3,pc2)]

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s)
   in show $ evalPV p inputParam
