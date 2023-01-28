module Memoization.Examples.Expr where

import Memoization.Core.Memory (KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State (runState), (<.>))

data Expr
  = Lit Int
  | Add Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

evalM :: Expr -> State (KeyValueArray Expr Int) Int
evalM (Lit i) = return i
evalM (Add e1 e2) = return (\x -> return (\y -> return (x + y))) <.> evalM e1 <.> evalM e2

memoizedEval :: Expr -> State (KeyValueArray Expr Int) Int
memoizedEval (Lit i) = return i
memoizedEval e@(Add e1 e2) =
  retrieveOrRun
    e
    ( \_ ->
        return (\x -> return (\y -> return (x + y))) <.> memoizedEval e1 <.> memoizedEval e2
    )
