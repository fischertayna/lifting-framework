module Language.VMemoInterpreter.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun, FuncKey(..))
import Memoization.Core.State (State(..), (<.>), evalState)
import Variability.VarTypes
  ( PresenceCondition,
    VarValor(..),
    Var (..),
    Context,
    substitute,
    negPC,
    ffPC,
    sat,
    unsat,
    doTraceOrResult,
    disj,
    mkBDDVar,
    notBDD,
    tt, ff,
    ttPC,
    valList,
    (+++),
    (++++), (||||),
    (|||), (/\), (\/),
    union
  )
import Prelude hiding (lookup)
import Variability.Functions
  (
    applyLength,
    applyBinaryOperator,
    applyUnaryOperator,
    applyEqualOperator,
    applyLtOperator,
    applySortList,
    applyUnion,
    applyIntersection,
    applyDifference,
    applyIsMember,
    partition,
    lookup,
    update,
    boolToInt
  )

import Data.Hashable (hash, Hashable)
import Debug.Trace (trace, traceM)

type RContext = (VContext, FContext, [Ident])

type VContext = Context Ident VarValor

type FContext = Context Ident Function

createFuncKey :: String -> [VarValor] -> FuncKey
createFuncKey name args = FuncKey
  { funcName = name
  , funcArgsHash = hash args
  }

type Mem =  (KeyValueArray FuncKey VarValor)

evalP :: Program -> [String] -> (VarValor -> Mem -> (VarValor, Mem))
evalP (Prog fs) memoizedFunctionNames =
  \input mem ->
    let memoizedIdents = map Ident memoizedFunctionNames
        initialFContext = updatecF ([(Ident "n", input)], [], memoizedIdents) fs
        context = initialFContext
    in eval context (Call (Ident "main") [EVar (Ident "n")]) mem

eval :: RContext -> Exp -> Mem -> (VarValor, Mem)
eval context@(vcontext, fcontext, memoizedFunctionNames) x mem = case x of
  EInt n -> (VarInteger (Var [(n, ttPC)]), mem)
  EStr s -> (VarString (Var [(s, ttPC)]), mem)
  ETrue  -> (VarBool (Var [(True, ttPC)]), mem)
  EFalse -> (VarBool (Var [(False, ttPC)]), mem)
  EVar vId -> (fromJust $ lookup vcontext vId, mem)
  ECon exp0 exp1  ->
    let (val0, mem1) = eval context exp0 mem
        (val1, mem2) = eval context exp1 mem1
    in case (val0, val1) of
        (VarString s0, VarString s1) -> (applyBinaryOperator VarString str val0 val1 (++), mem2)
        (VarList l0, VarList l1) -> (VarList (l0 ++ l1), mem2)
        _ -> error "Type error in concatenation"
  EAdd exp0 exp1 -> applyBinaryOp VarInteger int context exp0 exp1 mem (+)
  ESub exp0 exp1 -> applyBinaryOp VarInteger int context exp0 exp1 mem (-)
  EMul exp0 exp1 -> applyBinaryOp VarInteger int context exp0 exp1 mem (*)
  EDiv exp0 exp1 -> applyBinaryOp VarInteger int context exp0 exp1 mem div
  EOr  exp0 exp1 -> applyBinaryOp VarBool bool context exp0 exp1 mem (||)
  EAnd exp0 exp1 -> applyBinaryOp VarBool bool context exp0 exp1 mem (&&)
  ENot exp1      -> applyUnaryOp VarBool bool context exp1 mem not
  EPair p1 p2 ->
    let (v1, mem1) = eval context p1 mem
        (v2, mem2) = eval context p2 mem1
    in (VarPair (v1, v2), mem2)
  EList ls ->
    let (vals, mem') = foldl (\(acc, m) e -> let (v, m') = eval context e m in (acc ++ [v], m')) ([], mem) ls
    in (VarList vals, mem')
  EIf e eT eE ->
    let (cond, mem1) = eval context e mem
        (pct, pcf) = partition cond
    in if pct == ttPC
       then eval context eT mem1
       else if pct == ffPC
            then eval context eE mem1
            else let (valT, memT) = eval (restrictContext context pct) eT mem1
                     (valE, memE) = eval (restrictContext context pcf) eE memT
                 in ((valT |||| pct) ++++ (valE |||| pcf), memE)
  Call fId pExps -> case fId of
    Ident "head" -> (head ls, mem')
    Ident "tail" ->  (VarList (tail ls), mem')
    Ident "isNil" -> (VarInteger (Var [(boolToInt (null ls), ttPC)]), mem')
    Ident "fst" -> (f, mem')
    Ident "snd" -> (s, mem')
    Ident "isPair" -> case arg of
        VarPair _ -> (VarInteger (Var [(1, ttPC)]), mem')
        _ -> (VarInteger (Var [(0, ttPC)]), mem')
    Ident "length" -> 
      let (v1, mem1) = eval context (pExps !! 0) mem
      in (applyLength v1, mem1)
    Ident "isEqual" -> 
      let (v1, mem1) = eval context (pExps !! 0) mem
          (v2, mem2) = eval context (pExps !! 1) mem1
      in (applyEqualOperator v1 v2, mem2)
    Ident "sortList" -> 
      let (v1, mem1) = eval context (pExps !! 0) mem
      in (applySortList v1, mem1)
    Ident "isMember" -> 
      let (v, mem1) = eval context (pExps !! 0) mem
          (lst, mem2) = eval context (pExps !! 1) mem1
      in (applyIsMember v lst, mem2)
    Ident "lt" -> 
      let (v1, mem1) = eval context (pExps !! 0) mem
          (v2, mem2) = eval context (pExps !! 1) mem1
      in (applyLtOperator v1 v2, mem2)
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
      ls = list arg
      (f,s) = pair arg

applyBinaryOp :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext -> Exp -> Exp -> Mem  -> (a -> a -> a) -> (VarValor, Mem)
applyBinaryOp cons f context exp1 exp2 mem op =
  let (v1, mem1) = eval context exp1 mem
      (v2, mem2) = eval context exp2 mem1
  in (applyBinaryOperator cons f v1 v2 op, mem2)

applyUnaryOp :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext -> Exp -> Mem -> (a -> a) -> (VarValor, Mem)
applyUnaryOp cons f context exp mem op =
  let (v, mem') = eval context exp mem
  in (applyUnaryOperator cons f v op, mem')

restrictContext :: RContext -> PresenceCondition -> RContext
restrictContext context@(vcontext, fcontext, memoizedFunctionNames) pc = (restrictedVContext, fcontext, memoizedFunctionNames)
  where
    restrictedVContext = [(vId, restrictedVarValue) | (vId, v) <- vcontext, let restrictedVarValue = v |||| pc]

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext, memoizedFunctionNames) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext, memoizedFunctionNames) fs
  where
    newFContext = update fcontext fId f

regularCall :: RContext -> Ident -> [VarValor] -> Mem -> (VarValor, Mem)
regularCall context@(vcontext, fcontext, memoizedFunctionNames) fId paramValues mem =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
      paramBindings = zip decls paramValues
  in eval (paramBindings, fcontext, memoizedFunctionNames) fExp mem

memoizedCall :: RContext -> Ident -> [VarValor] -> Mem -> (VarValor, Mem)
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

