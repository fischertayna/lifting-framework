module Language.VInterpreter.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Variability.VarTypes
  ( PresenceCondition,
    Var (..),
    VarValor(..),
    Context,
    ffPC,
    ttPC,
    valList,
    (++++), (||||),
  )
import Prelude hiding (lookup)
import Variability.Functions
  (
    applyBinaryOperator,
    applyUnaryOperator,
    applyEqualOperator,
    applyLtOperator,
    applySortList,
    applyUnion,
    applyDifference,
    applyIsMember,
    partition,
    lookup,
    update,
    boolToInt
  )

type RContext = (VContext, FContext)

type VContext = Context Ident VarValor

type FContext = Context Ident Function

evalPV :: Program -> VarValor -> VarValor
evalPV (Prog fs) input = eval context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

eval :: RContext -> Exp -> VarValor
eval context@(vcontext, fcontext) x = case x of
  EInt n -> VarInteger (Var [(n, ttPC)])
  ECon exp0 exp1  ->
    let val0 = eval context exp0
        val1 = eval context exp1
    in case (val0, val1) of
        (VarString s0, VarString s1) -> applyBinaryOperator VarString str (eval context exp0) (eval context exp1) (++)
        (VarList l0, VarList l1) -> VarList (l0 ++ l1)
        _ -> error "Type error in concatenation"
  EAdd exp0 exp1 -> applyBinaryOperator VarInteger int (eval context exp0) (eval context exp1) (+)
  ESub exp0 exp1 -> applyBinaryOperator VarInteger int (eval context exp0) (eval context exp1) (-)
  EMul exp0 exp1 -> applyBinaryOperator VarInteger int (eval context exp0) (eval context exp1) (*)
  EDiv exp0 exp1 -> applyBinaryOperator VarInteger int (eval context exp0) (eval context exp1) div
  EOr exp0 exp1  -> applyBinaryOperator VarBool bool (eval context exp0) (eval context exp1) (||)
  EAnd exp0 exp1 -> applyBinaryOperator VarBool bool (eval context exp0) (eval context exp1) (&&)
  ENot exp1  -> applyUnaryOperator VarBool bool (eval context exp1) not
  EVar vId -> fromJust $ lookup vcontext vId
  EStr s -> VarString (Var [(s, ttPC)])
  ETrue -> VarBool (Var [(True, ttPC)])
  EFalse -> VarBool (Var [(False, ttPC)])
  EPair p1 p2 -> VarPair (eval context p1, eval context p2)
  EList ls -> VarList (map (eval context) ls)
  Call id pExps -> case id of
    Ident "head" -> head ls
    Ident "tail" ->  VarList (tail ls)
    Ident "isNil" -> VarInteger (Var [(boolToInt (null ls), ttPC)])
    Ident "fst" -> f
    Ident "snd" -> s
    Ident "isPair" -> case arg of
        VarPair _ -> VarInteger (Var [(1, ttPC)])
        _ -> VarInteger (Var [(0, ttPC)])
    Ident "isEqual" -> applyEqualOperator (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "sortList" -> applySortList (eval context (pExps !! 0))
    Ident "isMember" -> applyIsMember (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "lt" -> applyLtOperator (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "union" -> applyUnion (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "difference" -> applyDifference (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident func -> eval (paramBindings, fcontext) fExp
    where
      arg = eval context (head pExps)
      ls = list arg
      (f,s) = pair arg
      (Fun _ _ decls fExp) = fromJust $ lookup fcontext id
      paramBindings = zip decls (map (eval context) pExps)
  EIf e eT eE ->
    if pct == ttPC
      then eval context eT
      else
        if pct == ffPC
          then eval context eE
          else (restrictedEvalET |||| pct) ++++ (restrictedEvalEE |||| pcf)
    where
      (pct, pcf) = partition (eval context e)
      restrictedEvalET = eval (restrictContext context pct) eT
      restrictedEvalEE = eval (restrictContext context pcf) eE

restrictContext :: RContext -> PresenceCondition -> RContext
restrictContext (vcontext, fcontext) pc = (restrictedVContext, fcontext)
  where
    restrictedVContext = [(vId, restrictedVInt) | (vId, v) <- vcontext, let restrictedVInt = v |||| pc]


updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f