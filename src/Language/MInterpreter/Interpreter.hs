{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Language.MInterpreter.Interpreter (evalP) where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State, (<.>))
import Prelude hiding (lookup)
import Debug.Trace

type Context k v = [(k, v)]

type RContext m = (VContext m, FContext, Ident)

type VContext m = Context Ident (State m Integer)

type FContext = Context Ident Function

type Mem =  (KeyValueArray [Integer] Integer)

evalP :: Program -> String -> State Mem (Integer -> State Mem Integer)
evalP (Prog fs) memoizedFunctionName =
  return
    ( \input ->
        let initialFContext = updatecF ([(Ident "n", return input)], [], Ident memoizedFunctionName) fs
         in let context = initialFContext
             in eval context <.> return (Call (Ident "main") [EVar (Ident "n")])
    )

liftOperator :: (Integer -> Integer -> Integer) -> State m (Integer -> State m (Integer -> State m Integer))
liftOperator op = return (\a -> return (\b -> return (a `op` b)))

liftedIf :: State m Integer -> State m Integer -> State m Integer -> State m Integer
liftedIf cond p1 p2 = do
  c <- cond
  if c /= 0 then p1 else p2

eval :: RContext Mem -> State Mem (Exp -> State Mem Integer)
eval context@(vcontext, fcontext, memoizedFunctionName) =
  return
    ( \x -> case x of
        (EAdd exp0 exp1) -> liftOperator (+) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (ESub exp0 exp1) -> liftOperator (-) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EMul exp0 exp1) -> liftOperator (*) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EDiv exp0 exp1) -> liftOperator div <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        EInt n -> return n
        EVar vId -> fromJust (lookup vcontext vId)
        EIf cond expi expe -> liftedIf evalCond evalExpi evalExpe
          where
            evalCond = eval context <.> return cond
            evalExpi = eval context <.> return expi
            evalExpe = eval context <.> return expe
        Call fId pExps ->
          if fId == memoizedFunctionName
            then memoizedCall context fId pExps
            else
              ( let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
                 in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
                     in eval (paramBindings, fcontext, memoizedFunctionName) <.> return fExp
              )
    )

memoizedCall :: RContext Mem -> Ident -> [Exp] -> State Mem Integer
memoizedCall context@(vcontext, fcontext, memoizedFunctionName) fId pExps =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
   in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
       in let paramListM = mapM snd paramBindings
           in do
                paramList <- paramListM
                retrieveOrRun paramList (\_ -> eval (paramBindings, fcontext, memoizedFunctionName) <.> return fExp)

lookup :: Eq k => Context k v -> k -> Maybe v
lookup [] _ = Nothing
lookup ((i, v) : cs) s
  | i == s = Just v
  | otherwise = lookup cs s

update :: Eq k => Context k v -> k -> v -> Context k v
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

updatecF :: RContext Mem -> [Function] -> RContext Mem
updatecF c [] = c
updatecF (vcontext, fcontext, memoizedFunctionName) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext, memoizedFunctionName) fs
  where
    newFContext = update fcontext fId f
