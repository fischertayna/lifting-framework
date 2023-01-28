module Memoization.Examples.Map (cmap, cmapM, plusOne, plusOneM, rangeM) where

import Memoization.Core.State (State (State), runState, (<.>))

cmap :: (a -> b) -> [a] -> [b]
cmap _ [] = []
cmap f (x : xs) = f x : map f xs

cmapM :: State m ((a -> State m b) -> State m ([a] -> State m [b]))
cmapM = return (return . cmapM')

cmapM' :: (a -> State m b) -> [a] -> State m [b]
cmapM' f' [] = return []
cmapM' f' (x : xs) = consM <.> (return f' <.> return x) <.> (cmapM <.> return f' <.> return xs)

consM :: State m (a -> State m ([a] -> State m [a]))
consM =
  return
    ( \x ->
        return
          ( \xs ->
              return (x : xs)
          )
    )

plusOne :: Int -> Int
plusOne x = x + 1

plusOneM :: State m (Int -> State m Int)
plusOneM = return (\x -> return (x + 1))

rangeM :: Int -> State m [Int]
rangeM n =
  return (\r -> return [0 .. r])
    <.> return n
