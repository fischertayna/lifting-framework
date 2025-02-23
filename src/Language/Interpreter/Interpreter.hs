module Language.Interpreter.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Prelude hiding (lookup)
import Data.List (sortBy)
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

type RContext = (VContext, FContext)

type VContext = Context Ident Valor

type FContext = Context Ident Function

evalP :: Program -> Valor -> Valor
evalP (Prog fs) input = eval context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

eval :: RContext -> Exp -> Valor
eval context@(vcontext, fcontext) x = case x of
  ECon exp0 exp1 -> 
    let val0 = eval context exp0
        val1 = eval context exp1
    in case (val0, val1) of
        (ValorStr s0, ValorStr s1) -> applyBinaryOperator ValorStr s (eval context exp0) (eval context exp1) (++)
        (ValorList l0, ValorList l1) -> ValorList (l0 ++ l1)
        _ -> error "Type error in concatenation"
  -- ECon exp0 exp1  -> ValorStr (s (eval context exp0) ++ s (eval context exp1))
  EAdd exp0 exp1  -> applyBinaryOperator ValorInt i (eval context exp0) (eval context exp1) (+)
  ESub exp0 exp1  -> applyBinaryOperator ValorInt i (eval context exp0) (eval context exp1) (-)
  EMul exp0 exp1  -> applyBinaryOperator ValorInt i (eval context exp0) (eval context exp1) (*)
  EDiv exp0 exp1  -> applyBinaryOperator ValorInt i (eval context exp0) (eval context exp1) div
  EOr exp0 exp1   -> applyBinaryOperator ValorBool b (eval context exp0) (eval context exp1) (||)
  EAnd exp0 exp1  -> applyBinaryOperator ValorBool b (eval context exp0) (eval context exp1)  (&&)
  ENot exp1        -> applyUnaryOperator ValorBool b (eval context exp1) not
  EStr s          -> ValorStr s
  ETrue           -> ValorBool True
  EFalse          -> ValorBool False
  EInt n          -> ValorInt n
  EVar vId -> fromJust $ lookup vcontext vId
  EIf cond expi expe -> case eval context cond of
    ValorInt 0 -> eval context expe
    _ -> eval context expi
  EList list -> ValorList (map (eval context) list)
  EPair p1 p2 -> ValorPair (eval context p1, eval context p2)
  Call id pExps -> case id of
    Ident "head" -> head list
    Ident "tail" ->  ValorList (tail list)
    Ident "isNil" -> ValorInt (boolToInt (null list))
    Ident "fst" -> f
    Ident "snd" -> s
    Ident "isPair" -> applyIsPair arg
    Ident "length" -> applyLength (eval context (pExps !! 0))
    Ident "isEqual" -> applyIsEqual (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "lt" -> applyLt (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "sortList" -> applySortList (eval context (pExps !! 0))
    Ident "isMember" -> applyIsMember (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "union" -> applyUnion (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "intersection" -> applyIntersection (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident "difference" -> applyDifference (eval context (pExps !! 0)) (eval context (pExps !! 1))
    Ident func -> eval (paramBindings, fcontext) fExp
    where
      arg = eval context (head pExps)
      list = l arg
      (f,s) = p arg
      (Fun _ _ decls fExp) = fromJust $ lookup fcontext id
      paramBindings = zip decls (map (eval context) pExps)

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f
