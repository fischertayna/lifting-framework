module Language.MInterpreter.SLDriver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage
import Language.Frontend.ParLanguage
import Language.MInterpreter.Interpreter
import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State (runState), (<.>))
import Variability.VarTypes (Prop, Var (Var), apply, mkBDDVar, notBDD, ttPC, (/\))

main :: IO ()
main = do
  interact calc
  putStrLn ""

applyMapMAndZip :: (a -> State m b) -> Var a -> State m (Var b)
applyMapMAndZip f a =
  let fV = Var [(f, ttPC)]
   in let Var vals = apply fV a
       in let pcs = map snd vals
           in do
                (\ls -> Var (zip ls pcs)) <$> mapM fst vals

shallowLift :: State m (a -> State m b) -> Var a -> State m (Var b)
shallowLift fM v = do
  f <- fM
  applyMapMAndZip f v

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

input :: Var Integer
input = Var [(5, atbt), (6, atbf), (7, afbt), (8, afbf)]

multByTwoIfPropA :: Var (Integer -> Integer)
multByTwoIfPropA = Var [((* 2), propA), (id, notBDD propA) ]

input' :: Var Integer
input' = apply multByTwoIfPropA input

initialState :: KeyValueArray [Integer] Integer
initialState = []

memoizedFunctionName :: String
memoizedFunctionName = "fib"

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s); programComputation = shallowLift (evalP p memoizedFunctionName) input'
   in show $ runState programComputation initialState
