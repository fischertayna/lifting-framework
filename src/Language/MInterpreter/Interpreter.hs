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
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun, FuncKey(..))
import Memoization.Core.State (State(..), (<.>), evalState)
import Prelude hiding (lookup)
import Base.Types (Valor(..), Context)
import Base.Functions
  (
    applyBinaryOperator,
    applyUnaryOperator,
    applyIsPair,
    applyIsEqual,
    applyLt,
    applyIsMember,
    applyLength,
    applySortList,
    applyUnion,
    applyIntersection,
    applyDifference,
    boolToInt,
    lookup,
    update
  )
import Data.Hashable (hash, Hashable)
import Debug.Trace

type RContext = (VContext, FContext, [Ident])

type VContext = Context Ident Valor

type FContext = Context Ident Function

createFuncKey :: String -> [Valor] -> FuncKey
createFuncKey name args = FuncKey
  { funcName = name
  , funcArgsHash = hash args
  }

type Mem =  (KeyValueArray FuncKey Valor)

evalP :: Program -> [String] -> (Valor -> Mem -> (Valor, Mem))
evalP (Prog fs) memoizedFunctionNames =
   \input mem ->
      let memoizedIdents = map Ident memoizedFunctionNames
          initialFContext = updatecF ([(Ident "n", input)], [], memoizedIdents) fs
          context = initialFContext
      in eval context (Call (Ident "main") [EVar (Ident "n")]) mem


eval :: RContext -> Exp -> Mem -> (Valor, Mem)
eval context@(vcontext, fcontext, memoizedFunctionNames) x mem =
  case x of
    ECon exp0 exp1  ->
      let (val0, mem1) = eval context exp0 mem
          (val1, mem2) = eval context exp1 mem1
      in case (val0, val1) of
          (ValorStr s0, ValorStr s1) -> (applyBinaryOperator ValorStr s val0 val1 (++), mem2)
          (ValorList l0, ValorList l1) -> (ValorList (l0 ++ l1), mem2)
          _ -> error "Type error in concatenation"
    (EAdd exp0 exp1) -> applyBinaryOp ValorInt i context exp0 exp1 mem (+)
    (ESub exp0 exp1) -> applyBinaryOp ValorInt i context exp0 exp1 mem (-)
    (EMul exp0 exp1) -> applyBinaryOp ValorInt i context exp0 exp1 mem (*)
    (EDiv exp0 exp1) -> applyBinaryOp ValorInt i context exp0 exp1  mem div
    (EOr exp0 exp1)  -> applyBinaryOp ValorBool b context exp0 exp1 mem (||)
    (EAnd exp0 exp1) -> applyBinaryOp ValorBool b context exp0 exp1 mem (&&)
    (ENot exp1)      -> applyUnaryOp ValorBool b context exp1 mem not
    EInt n           -> (ValorInt n, mem)
    EStr s           -> (ValorStr s, mem)
    ETrue            -> (ValorBool True, mem)
    EFalse           -> (ValorBool False, mem)
    EVar vId         -> (fromJust $ lookup vcontext vId, mem)
    EList ls         ->
      let (vals, mem') = foldl (\(acc, m) e -> let (v, m') = eval context e m in (acc ++ [v], m')) ([], mem) ls
      in (ValorList vals, mem')
    EPair p1 p2 ->
      let (v1, mem1) = eval context p1 mem
          (v2, mem2) = eval context p2 mem1
      in (ValorPair (v1, v2), mem2)
    EIf e eT eE -> 
      let (cond, mem1) = eval context e mem
      in case cond of
        ValorInt 0 -> eval context eE mem1
        _ -> eval context eT mem1
    Call fId pExps -> case fId of
      Ident "head" -> (head ls, mem')
      Ident "tail" -> (ValorList (tail ls), mem')
      Ident "isNil" -> (ValorInt (boolToInt (null ls)), mem')
      Ident "fst" -> (f, mem')
      Ident "snd" -> (s, mem')
      Ident "isPair" -> (applyIsPair arg, mem')
      Ident "length" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
        in (applyLength v1, mem1) 
      Ident "isEqual" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
            (v2, mem2) = eval context (pExps !! 1) mem1
        in (applyIsEqual v1 v2, mem2)
      Ident "lt" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
            (v2, mem2) = eval context (pExps !! 1) mem1        
        in (applyLt v1 v2, mem2)
      Ident "sortList" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
        in (applySortList v1, mem1)
      Ident "isMember" -> 
        let (v, mem1) = eval context (pExps !! 0) mem
            (lst, mem2) = eval context (pExps !! 1) mem1
        in (applyIsMember v lst, mem2)
      Ident "union" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
            (v2, mem2) = eval context (pExps !! 1) mem1
        in (applyUnion v1 v2, mem2)
      Ident "intersection" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
            (v2, mem2) = eval context (pExps !! 1) mem1
        in (applyIntersection v1 v2, mem2)
      Ident "difference" -> 
        let (v1, mem1) = eval context (pExps !! 0) mem
            (v2, mem2) = eval context (pExps !! 1) mem1
        in (applyDifference v1 v2, mem2)
      _ ->
        let (paramValues, mem'') = foldl (\(vals, m) e -> let (v, m') = eval context e m in (vals ++ [v], m')) ([], mem) pExps
        in if fId `elem` memoizedFunctionNames
          then memoizedCall context fId paramValues mem''
          else regularCall context fId paramValues mem''
      where
        (arg, mem') = eval context (head pExps) mem
        ls = l arg
        (f,s) = p arg

applyBinaryOp :: (a -> Valor) -> (Valor -> a) -> RContext -> Exp -> Exp -> Mem  -> (a -> a -> a) -> (Valor, Mem)
applyBinaryOp cons f context exp1 exp2 mem op =
  let (v1, mem1) = eval context exp1 mem
      (v2, mem2) = eval context exp2 mem1
  in (applyBinaryOperator cons f v1 v2 op, mem2)

applyUnaryOp :: (a -> Valor) -> (Valor -> a) -> RContext -> Exp -> Mem -> (a -> a) -> (Valor, Mem)
applyUnaryOp cons f context exp mem op =
  let (v, mem') = eval context exp mem
  in (applyUnaryOperator cons f v op, mem')

regularCall :: RContext -> Ident -> [Valor] -> Mem -> (Valor, Mem)
regularCall context@(vcontext, fcontext, memoizedFunctionNames) fId paramValues mem =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
      paramBindings = zip decls paramValues
  in eval (paramBindings, fcontext, memoizedFunctionNames) fExp mem

memoizedCall :: RContext -> Ident -> [Valor] -> Mem -> (Valor, Mem)
memoizedCall context@(vcontext, fcontext, memoizedFunctionNames) fId paramValues mem =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
      name = getIdentString fId
      paramBindings = zip decls paramValues
      funcKey = createFuncKey name paramValues
  in runState (do
      retrieveOrRun name funcKey (\() -> do
        let (evalResult, mem') = eval (paramBindings, fcontext, memoizedFunctionNames) fExp mem
        return evalResult) 
    ) mem

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext, memoizedFunctionNames) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext, memoizedFunctionNames) fs
  where
    newFContext = update fcontext fId f
