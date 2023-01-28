{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Memoization.Examples.Stack (Stack, pop, push) where

import Memoization.Core.State (State (State, runState))

type Stack = [Int]

pop :: State [a] a
pop = State (\(x : xs) -> (x, xs))

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)
