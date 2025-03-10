{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use >>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Memoization.Core.Memory where

import Memoization.Core.State (State (State, runState))
import Debug.Trace

class KeyMemory k v m where
  mlookup :: k -> State m (Maybe v)
  mupdate :: k -> v -> State m ()

type KeyValueArray k v = [(k, (v, Int))]

instance (Eq k, Show k, Show v) => KeyMemory k v (KeyValueArray k v) where
  mlookup :: k -> State (KeyValueArray k v) (Maybe v)
  mlookup a = State (\s ->
    case lookup a s of
      Just (v, count) ->
        let updated = map (\(k', (v', c)) -> if k' == a then (k', (v', c + 1)) else (k', (v', c))) s
         in (Just v, updated)
      Nothing -> (Nothing, s)
    )
  mupdate :: k -> v -> State (KeyValueArray k v) ()
  mupdate a v = State (\s ->
    let newS = case lookup a s of
          Just (_, count) -> (a, (v, count)) : filter (\(k', _) -> k' /= a) s  -- preserve count
          Nothing         -> (a, (v, 0)) : s                                    -- new entry
     in ((), newS))

retrieveOrRun :: (KeyMemory k v m, Show v, Show k) => String -> k -> (() -> State m v) -> State m v
retrieveOrRun name x t = do
  mlookup x >>= \case
    Just v -> return v
    Nothing -> do
      v <- t ()
      mupdate x v
      return v

-- | Resets all lookup counters in the memory to 0
resetCounters :: State (KeyValueArray k v) ()
resetCounters = State (\s -> ((), map (\(k, (v, _)) -> (k, (v, 0))) s))

-- | Returns all keys and their current lookup counters
showCounters :: State (KeyValueArray k v) [(k, Int)]
showCounters = State (\s -> (map (\(k, (_, c)) -> (k, c)) s, s))

-- | Returns the sum of all lookup counters
sumCounters :: State (KeyValueArray k v) Int
sumCounters = State (\s -> (sum (map (\(_, (_, c)) -> c) s), s))

data FuncKey = FuncKey
  { funcName :: String   -- Name of the function
  , funcArgsHash :: Int  -- Hash of the function arguments
  } deriving (Eq, Show, Read)