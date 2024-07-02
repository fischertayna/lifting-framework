module Language.MInterpreter.SLDriver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.MInterpreter.Interpreter
import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State (runState), (<.>))
import Variability.VarTypes (Prop, Var (Var), apply, mkBDDVar, notBDD, ttPC, (/\), (\/), (|||))

main :: IO ()
main = do
  interact calc
  putStrLn ""

applyMapMAndZip :: (a -> State m b) -> State m (Var a -> State m (Var b))
applyMapMAndZip f =
  return
    ( \va -> do
        let fV = Var [(f, ttPC)]
            Var vals = apply fV va
            pcs = map snd vals
         in (\ls -> Var (zip ls pcs)) <$> mapM fst vals
    )

shallowLift :: State m (a -> State m b) -> State m (Var a -> State m (Var b))
shallowLift fM = do
  f <- fM
  applyMapMAndZip f

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

atbt :: Prop
atbt = propA /\ propB

atbf :: Prop
atbf = propA /\ notBDD propB

afbt :: Prop
afbt = notBDD propA /\ propB

afbf :: Prop
afbf = notBDD propA /\ notBDD propB

input :: Var Valor
input = (Var [(ValorInt 8, atbt), (ValorInt 5, afbf), (ValorInt 1, atbf), (ValorInt 0, afbt)])

initialState :: KeyValueArray [Valor] Valor
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s); programComputationM = shallowLift (evalP p memoizedFunctionName) <.> return input
   in show $ (runState programComputationM initialState)
