{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE FlexibleContexts #-}
module Memoization.Examples.Knapsack where

import Memoization.Core.Memory (KeyValueArray, retrieveOrRun, KeyMemory)
import Memoization.Core.State (State (runState), (<.>))

wmap :: Int -> Int
wmap n = [13, 3, 4, 1, 2, 6, 8, 10, 7, 5, 3, 2, 9] !! (n - 1)

vmap :: Int -> Int
vmap n = [27, 12, 6, 8, 12, 7, 4, 20, 13, 15, 9, 5, 7] !! (n - 1)

ife :: Bool -> Int -> Int -> Int
ife cond a b = if cond then a else b

vmapM ::
  State
    m
    (Int -> State m Int)
vmapM = return (\n -> return (vmap n))

wmapM ::
  State
    m
    (Int -> State m Int)
wmapM = return (\n -> return (wmap n))

minusM ::
  State
    m
    ( Int ->
      State
        m
        (Int -> State m Int)
    )
minusM = return (\a -> return (\b -> return ((-) a b)))

oneM :: State m Int
oneM = return 1

plusM ::
  State
    m
    ( Int ->
      State
        m
        (Int -> State m Int)
    )
plusM = return (\a -> return (\b -> return ((+) a b)))

ifeM ::
  State
    m
    ( Bool ->
      State
        m
        ( Int ->
          State
            m
            (Int -> State m Int)
        )
    )
ifeM =
  return
    ( \cond ->
        return
          ( \a ->
              return
                ( \b -> if cond then return a else return b
                )
          )
    )

ltM ::
  State
    m
    ( Int ->
      State
        m
        ( Int ->
          State m Bool
        )
    )
ltM = return (\a -> return (\b -> return ((<) a b)))

maxM ::
  State
    m
    ( Int ->
      State
        m
        ( Int ->
          State m Int
        )
    )
maxM = return (\a -> return (\b -> return (max a b)))

knapsack :: Int -> Int -> Int
knapsack n w = case n of
  0 -> 0
  n ->
    ife
      (w < wmap n)
      (knapsack (n - 1) w)
      (max (knapsack (n - 1) w) (vmap n + knapsack (n - 1) (w - wmap n)))

knapsackM ::
  State
    m
    ( Int ->
      State
        m
        (Int -> State m Int)
    )
knapsackM =
  return
    ( \n ->
        return
          ( \w ->
              case n of
                0 -> return 0
                n ->
                  ifeM
                    <.> (ltM <.> return w <.> (wmapM <.> return n))
                    <.> (knapsackM <.> (minusM <.> return n <.> oneM) <.> return w)
                    <.> ( maxM
                            <.> (knapsackM <.> (minusM <.> return n <.> oneM) <.> return w)
                            <.> (plusM <.> (vmapM <.> return n) <.> (knapsackM <.> (minusM <.> return n <.> oneM) <.> (minusM <.> return w <.> (wmapM <.> return n))))
                        )
          )
    )

knapsackMem :: (KeyMemory (Int,Int) Int m) =>
  State
    m
    ( Int ->
      State
        (KeyValueArray (Int, Int) Int)
        (Int -> State (KeyValueArray (Int, Int) Int) Int)
    )
knapsackMem = return(\n -> return (\w -> retrieveOrRun (n, w) (\_ -> case n of
                      0 -> return 0
                      n ->
                        ifeM
                          <.> (ltM <.> return w <.> (wmapM <.> return n))
                          <.> (knapsackMem <.> (minusM <.> return n <.> oneM) <.> return w)
                          <.> ( maxM
                                  <.> (knapsackMem <.> (minusM <.> return n <.> oneM) <.> return w)
                                  <.> (plusM <.> (vmapM <.> return n) <.> (knapsackMem <.> (minusM <.> return n <.> oneM) <.> (minusM <.> return w <.> (wmapM <.> return n))))
                              )

                )))
          
  
nItens :: Int
nItens = 13

capacity :: Int
capacity = 80

nonMonadifiedKnapsack :: Int
nonMonadifiedKnapsack = knapsack nItens capacity

monadifiedKnapsack :: (Int, [Int])
monadifiedKnapsack = runState (knapsackM <.> return nItens <.> return capacity) []

memoizedKnapsack :: (Int, KeyValueArray (Int, Int) Int)
memoizedKnapsack = runState (knapsackMem <.> return nItens <.> return capacity) []