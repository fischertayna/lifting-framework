{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Language.MInterpreter.MFat where

import Debug.Trace
import Memoization.Core.State (State (runState), (<.>))

liftOp :: (a -> b -> c) -> State m (a -> State m (b -> State m c))
liftOp op = return (\a -> return (\b -> return (op a b)))

liftedEq :: State m (Integer -> State m (Integer -> State m Bool))
liftedEq = liftOp (==)

liftedMult :: State m (Integer -> State m (Integer -> State m Integer))
liftedMult = liftOp (*)

sumM :: State m (Integer -> State m Integer)
sumM =
  return
    ( \n ->
        trace
          (show n)
            ifM
            (liftOp (==) <.> return n <.> return 0)
            (return 0)
            (liftOp (+) <.> return n <.> (sumM <.> (liftOp (-) <.> return n <.> return 1)))
    )

ifM :: State m Bool -> State m Integer -> State m Integer -> State m Integer
ifM cond p1 p2 = do
  c <- cond
  if c then p1 else p2
-- Remember that the 'do' notation is just syntatic sugar to:
-- ifM cond p1 p2 = cond >>= (\c -> if c then p1 else p2)
