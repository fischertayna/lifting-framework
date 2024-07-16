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

pA :: Prop
pA = mkBDDVar "A"

input :: Var Valor
input = (Var [(ValorInt 8, pA), (ValorInt 5, notBDD pA)])

initialState :: KeyValueArray [Valor] Valor
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

executeProg :: String -> String -> Var Valor -> (Var Valor, Mem)
executeProg memoizedFunctionName prog input =
  let Ok p = pProgram (myLexer prog); programComputationM = shallowLift (evalP p memoizedFunctionName) <.> return input
  in runState programComputationM initialState

calc :: String -> String
calc s =
  let r = executeProg memoizedFunctionName s input
   in show $ r
