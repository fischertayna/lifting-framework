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
    deriving (Show, Eq, Ord)

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

liftIntOperator :: (Integer -> Integer -> Integer) -> State m (Valor -> State m (Valor -> State m Valor))
liftIntOperator op = return (\(ValorInt a) -> return (\(ValorInt b) -> return (ValorInt (a `op` b))))

liftStringOperator :: (String -> String -> String) -> State m (Valor -> State m (Valor -> State m Valor))
liftStringOperator op = return (\(ValorStr a) -> return (\(ValorStr b) -> return (ValorStr (a `op` b))))

liftBoolBinaryOperator :: (Bool -> Bool -> Bool) -> State m (Valor -> State m (Valor -> State m Valor))
liftBoolBinaryOperator op = return (\(ValorBool a) -> return (\(ValorBool b) -> return (ValorBool (a `op` b))))

liftBoolUnaryOperator :: (Bool -> Bool) -> State m (Valor -> State m Valor)
liftBoolUnaryOperator op = return (\(ValorBool b) -> return (ValorBool (op b)))

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
        (ECon exp0 exp1)  -> liftStringOperator (++) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EAdd exp0 exp1) -> liftIntOperator (+) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (ESub exp0 exp1) -> liftIntOperator (-) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EMul exp0 exp1) -> liftIntOperator (*) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EDiv exp0 exp1) -> liftIntOperator div <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EOr exp0 exp1)   -> liftBoolBinaryOperator (||) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (EAnd exp0 exp1)  -> liftBoolBinaryOperator (&&) <.> (eval context <.> return exp0) <.> (eval context <.> return exp1)
        (ENot exp1)        -> liftBoolUnaryOperator not <.> (eval context <.> return exp1)
        EInt n -> return (ValorInt n)
        EStr s          -> return (ValorStr s)
        ETrue           -> return (ValorBool True)
        EFalse          -> return (ValorBool False)
        EVar vId -> fromJust (lookup vcontext vId)
        EList exps -> do
          vals <- mapM (\e -> eval context <.> return e) exps
          return $ ValorList vals
        EPair p1 p2 -> do
          v1 <- eval context <.> return p1
          v2 <- eval context <.> return p2
          return $ ValorPair (v1, v2)
        EIf cond expi expe -> liftedIf evalCond evalExpi evalExpe
          where
            evalCond = eval context <.> return cond
            evalExpi = eval context <.> return expi
            evalExpe = eval context <.> return expe
        Call fId pExps -> case fId of
          Ident "head" -> do
            vals <- (mapM (\e -> eval context <.> return e) pExps)
            case vals of
              [ValorList l] -> return (head l)
              _ -> error "Invalid argument to head"
          Ident "tail" -> do
            vals <- (mapM (\e -> eval context <.> return e) pExps)
            case vals of
              [ValorList l] -> return $ ValorList (tail l)
              _ -> error "Invalid argument to tail"
          Ident "isNil" -> do
            vals <- (mapM (\e -> eval context <.> return e) pExps)
            case vals of
              [ValorList l] -> return $ ValorInt (boolToInt (null l))
              _ -> error "Invalid argument to isNil"
          Ident "fst" -> do
            vals <- (mapM (\e -> eval context <.> return e) pExps)
            case vals of
              [ValorPair (f, s)] -> return f
              _ -> error "Invalid argument to fst"
          Ident "snd" -> do
            vals <- (mapM (\e -> eval context <.> return e) pExps)
            case vals of
              [ValorPair (f, s)] -> return s
              _ -> error "Invalid argument to snd"
          _ ->
            if fId == memoizedFunctionName
              then memoizedCall context fId pExps
              else
                ( let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
                  in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
                      in eval (paramBindings, fcontext, memoizedFunctionName) <.> return fExp
                )
    )

boolToInt :: Bool -> Integer
boolToInt b
  | not b = 0
  | otherwise = 1

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
