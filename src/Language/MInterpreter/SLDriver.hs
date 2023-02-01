import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.MInterpreter.Interpreter
import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State (runState), (<.>))
import Variability.VarTypes (Var (Var), apply, truePC)

main :: IO ()
main = do
  interact calc
  putStrLn ""

applyMapMAndZip :: (a -> State m b) -> Var a -> State m (Var b)
applyMapMAndZip f a =
  let fV = Var [(f, truePC)]
   in let Var vals = apply fV a
       in let pcs = map snd vals
           in do
                (\ls -> Var (zip ls pcs)) <$> mapM fst vals

shallowLift :: State m (a -> State m b) -> Var a -> State m (Var b)
shallowLift fM v = do
  f <- fM
  applyMapMAndZip f v

input :: Var Integer
-- As we are NOT checking the presence condition invariants (yet), setting all the
-- presence conditions to 'True' will help us to see 
-- some sort of Variational computation in action (even if somewhat limited).
input = Var [(10,True), (8, True), (7,True), (40,True)]

initialState :: KeyValueArray [Integer] Integer
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s); programComputation = shallowLift (evalP p memoizedFunctionName) input
   in show $ runState programComputation initialState
