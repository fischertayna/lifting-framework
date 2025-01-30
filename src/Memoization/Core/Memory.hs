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

type KeyValueArray k v = [(k, v)]

instance (Eq k, Show k, Show v) => KeyMemory k v (KeyValueArray k v) where
  mlookup :: k -> State (KeyValueArray k v) (Maybe v)
  mlookup a = State (\s -> (lookup a s, s))
  mupdate :: k -> v -> State (KeyValueArray k v) ()
  mupdate a v = State (\s -> ((), (a, v) : s))

retrieveOrRun :: (KeyMemory k v m, Show v, Show k) => String -> k -> (() -> State m v) -> State m v
retrieveOrRun name x t = do
  -- trace ("\n\n retrieveOrRun " ++ show name ++ ": Checking for key: " ++ show x) $ return ()
  mlookup x >>= \case
    Just v -> do
      -- trace ("\n retrieveOrRun " ++ show name ++ ": Cache hit for key: " ++ show x ++ "\n with value: " ++ show v) $ return ()
      return v
    Nothing -> do
      -- trace ("\n retrieveOrRun " ++ show name ++ ": Cache miss for key: " ++ show x) $ return ()
      v <- t ()
      -- trace ("\n retrieveOrRun " ++ show name ++ ": Computed value: " ++ show v ++ " for key: " ++ show x) $ return ()
      mupdate x v
      -- trace ("\n retrieveOrRun " ++ show name ++ ": Updated cache with key: " ++ show x ++ " and value: " ++ show v) $ return ()
      return v
