{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Language.MInterpreter.Interpreter where

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

data Valor
    = ValorInt
        { i :: Integer
        }
    | ValorBool
        { b :: Bool
        }
    | ValorStr
        { s :: String
        }
    | ValorList
        { l :: [Valor]
        }
    | ValorPair
        { p :: (Valor, Valor)
        }
    deriving (Show, Eq)

type Context k v = [(k, v)]

type RContext m = (VContext m, FContext, Ident)

type VContext m = Context Ident (State m Valor)

type FContext = Context Ident Function

type Mem =  (KeyValueArray [Valor] Valor)

evalP :: Program -> String -> State Mem (Valor -> State Mem Valor)
evalP (Prog fs) memoizedFunctionName =
  return
    ( \input ->
        let initialFContext = updatecF ([(Ident "n", return input)], [], Ident memoizedFunctionName) fs
         in let context = initialFContext
             in eval context <.> return (Call (Ident "main") [EVar (Ident "n")])
    )

liftOperator :: (Integer -> Integer -> Integer) -> State m (Valor -> State m (Valor -> State m Valor))
liftOperator op = return (\(ValorInt a) -> return (\(ValorInt b) -> return (ValorInt (a `op` b))))


liftedIf :: State m Valor -> State m Valor -> State m Valor -> State m Valor
liftedIf cond p1 p2 = do
  c <- cond
  case c of
    ValorInt 0 -> p2
    _ -> p1

eval :: RContext Mem -> State Mem (Exp -> State Mem Valor)
eval context@(vcontext, fcontext, memoizedFunctionName) =
  return
    ( \x -> case x of
        (EAdd exp0 exp1) -> liftOperator (+) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (ESub exp0 exp1) -> liftOperator (-) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EMul exp0 exp1) -> liftOperator (*) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EDiv exp0 exp1) -> liftOperator div <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        EInt n -> return (ValorInt n)
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

memoizedCall :: RContext Mem -> Ident -> [Exp] -> State Mem Valor
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
