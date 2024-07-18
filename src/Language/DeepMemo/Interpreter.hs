{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Language.DeepMemo.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State, (<.>))
import Variability.VarTypes
  ( PresenceCondition,
    VarValor(..),
    Var (..),
    ffPC,
    sat,
    ttPC,
    valList,
    (+++),
    (++++), (||||),
    (|||), (/\), (\/),
  )
import Prelude hiding (lookup)
import Debug.Trace
import Language.MInterpreter.Interpreter (Valor(ValorList, ValorInt))

type Context k v = [(k, v)]

type RContext m = (VContext m, FContext, Ident)

type VContext m = Context Ident (State m VarValor)

type FContext = Context Ident Function

type Mem =  (KeyValueArray [VarValor] VarValor)

evalP :: Program -> String -> State Mem (VarValor -> State Mem VarValor)
evalP (Prog fs) memoizedFunctionName =
  return
    ( \input ->
        let initialFContext = updatecF ([(Ident "n", return input)], [], Ident memoizedFunctionName) fs
         in let context = initialFContext
             in eval context <.> return (Call (Ident "main") [EVar (Ident "n")])
    )

applyBinaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext Mem -> Exp -> Exp -> (a -> a -> a) -> State Mem VarValor
applyBinaryOperator cons f context exp0 exp1 op = do
  e0 <- (eval context <.> return exp0)
  e1 <- (eval context <.> return exp1)
  return $ cons (Var
    [ (v1 `op` v2, pc1 /\ pc2)
      | (v1, pc1) <- valList (f e0),
        (v2, pc2) <- valList (f e1),
        sat (pc1 /\ pc2)
    ])

applyUnaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext Mem -> Exp -> (a -> a) -> State Mem VarValor
applyUnaryOperator cons f context exp op = do
  e <- (eval context <.> return exp)
  return $ cons (Var
    [ (op v, pc1)
      | (v, pc1) <- valList (f e)
    ])

eval :: RContext Mem -> State Mem (Exp -> State Mem VarValor)
eval context@(vcontext, fcontext, memoizedFunctionName) =
  return
    ( \x -> case x of
        (ECon exp0 exp1) -> applyBinaryOperator VarString str context exp0 exp1 (++)
        (EAdd exp0 exp1) -> applyBinaryOperator VarInteger int context exp0 exp1 (+)
        (ESub exp0 exp1) -> applyBinaryOperator VarInteger int context exp0 exp1 (-)
        (EMul exp0 exp1) -> applyBinaryOperator VarInteger int context exp0 exp1 (*)
        (EDiv exp0 exp1) -> applyBinaryOperator VarInteger int context exp0 exp1 div
        (EOr exp0 exp1)  -> applyBinaryOperator VarBool bool context exp0 exp1 (||)
        (EAnd exp0 exp1) -> applyBinaryOperator VarBool bool context exp0 exp1 (&&)
        (ENot exp1)        -> applyUnaryOperator VarBool bool context exp1 not
        EInt n -> return (VarInteger (Var [(n, ttPC)]))
        EStr s          -> return (VarString (Var [(s, ttPC)]))
        ETrue           -> return (VarBool (Var [(True, ttPC)]))
        EFalse          -> return (VarBool (Var [(False, ttPC)]))
        EVar vId -> fromJust (lookup vcontext vId)
        EList exps -> do
          vals <- mapM (\e -> eval context <.> return e) exps
          return $ VarList vals
        EPair p1 p2 -> do
          v1 <- eval context <.> return p1
          v2 <- eval context <.> return p2
          return $ VarPair (v1, v2)
        EIf cond eT eE -> do
          e <- eval context <.> return cond
          let (pct, pcf) = partition e
          if pct == ttPC
            then eval context <.> return eT

            else
              if pct == ffPC
                then eval context <.> return eE

                else applyRestrict context pct pcf eT eE
        Call fId pExps -> case fId of
          Ident "head" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarList l] -> return (head l)
              _ -> error "Invalid argument to head"
          Ident "tail" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarList l] -> return $ VarList (tail l)
              _ -> error "Invalid argument to tail"
          Ident "isNil" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarList l] -> return $ VarInteger (Var [(boolToInt (null l), ttPC)])
              _ -> error "Invalid argument to isNil"
          Ident "fst" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarPair (f, s)] -> return f
              _ -> error "Invalid argument to fst"
          Ident "snd" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarPair (f, s)] -> return s
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

memoizedCall :: RContext Mem -> Ident -> [Exp] -> State Mem VarValor
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

restrictVarValue :: State Mem VarValor -> PresenceCondition -> State Mem VarValor
restrictVarValue varState pc = do
  varValue <- varState
  let restrictedVarValue = varValue |||| pc
  return restrictedVarValue

restrictContext :: RContext Mem -> PresenceCondition -> RContext Mem
restrictContext context@(vcontext, fcontext, memoizedFunctionName) pc = (restrictedVContext, fcontext, memoizedFunctionName)
  where
    restrictedVContext = [(vId, restrictVarValue v pc) | (vId, v) <- vcontext]

partition :: VarValor -> (PresenceCondition, PresenceCondition)
partition (VarInteger (Var lvint)) =
  foldr
    (\(v, pc) (pct, pcf) -> if v /= 0 then (pct \/ pc, pcf) else (pct, pcf \/ pc))
    (ffPC, ffPC)
    lvint
partition (VarBool (Var lvint)) =
  foldr
    (\(v, pc) (pct, pcf) -> if v then (pct \/ pc, pcf) else (pct, pcf \/ pc))
    (ffPC, ffPC)
    lvint

applyRestrict :: RContext Mem -> PresenceCondition -> PresenceCondition -> Exp -> Exp -> State Mem VarValor
applyRestrict context pct pcf eT eE = do
  restrictedEvalET <-  eval (restrictContext context pct) <.> return eT
  restrictedEvalEE <-  eval (restrictContext context pcf) <.> return eE
  return $ (restrictedEvalET |||| pct) ++++ (restrictedEvalEE |||| pcf)

