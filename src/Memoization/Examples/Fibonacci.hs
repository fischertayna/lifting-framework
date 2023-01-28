{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Memoization.Examples.Fibonacci (fib, memoizedFib) where

import Memoization.Examples.Map (cmap, cmapM)
import Memoization.Core.Memory (KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State (runState), (<.>))

fib :: Int -> Int
fib n = 1 + sum ((\f -> cmap f [0 .. n -2]) fib)

rangeMinus2M :: Int -> State m [Int]
rangeMinus2M n =
  return (\r -> return [0 .. r])
    <.> ( return (\x -> return (\y -> return (x - y)))
            <.> return n
            <.> return 2
        )

fibM :: Int -> State (KeyValueArray Int Int) Int
fibM n =
  return (\x -> return (1 + x))
    <.> ( return (\xs -> return (sum xs))
            <.> (return (\f' -> cmapM <.> return f' <.> rangeMinus2M n) <.> return fibM)
        )

memoizedFib :: Int -> State (KeyValueArray Int Int) Int
memoizedFib n =
  retrieveOrRun
    n
    ( \_ ->
        return (\x -> return (1 + x))
          <.> ( return (\xs -> return (sum xs))
                  <.> (return (\f' -> cmapM <.> return f' <.> rangeMinus2M n) <.> return memoizedFib)
              )
    )
