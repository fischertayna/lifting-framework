module Language.VInterpreter.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Prelude hiding (lookup)

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

type VContext = Context Ident VarInt

type FContext = Context Ident Function

type PresenceCondition = Bool

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t] deriving (Show)

type VarInt = Var Integer

valList :: VarInt -> [Val Integer]
valList (Var ls) = ls

applyOperator :: RContext -> Exp -> Exp -> (Integer -> Integer -> Integer) -> VarInt
applyOperator context exp0 exp1 op =
  Var
    [ (i1 `op` i2, pc1 && pc2)
      | (i1, pc1) <- valList (evalV context exp0),
        (i2, pc2) <- valList (evalV context exp1),
        sat (pc1 && pc2)
    ]

sat :: PresenceCondition -> Bool
sat b = b == truePC

unsat :: PresenceCondition -> Bool
unsat b = b == falsePC

evalPV :: Program -> VarInt -> VarInt
evalPV (Prog fs) input = evalV context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

(\/) :: PresenceCondition -> PresenceCondition -> PresenceCondition
a \/ b = a || b

(/\) :: PresenceCondition -> PresenceCondition -> PresenceCondition
a /\ b = a && b

truePC :: PresenceCondition
truePC = True

falsePC :: PresenceCondition
falsePC = False

union :: VarInt -> VarInt -> VarInt
union (Var ls1) (Var ls2) = Var $ ls1 ++ ls2

evalV :: RContext -> Exp -> Var Integer
evalV context@(vcontext, fcontext) x = case x of
  EInt n -> Var [(n, True)]
  EAdd exp0 exp1 -> applyOperator context exp0 exp1 (+)
  ESub exp0 exp1 -> applyOperator context exp0 exp1 (-)
  EMul exp0 exp1 -> applyOperator context exp0 exp1 (*)
  EDiv exp0 exp1 -> applyOperator context exp0 exp1 div
  EVar vId -> fromJust $ lookup vcontext vId
  Call fId pExps -> evalV (paramBindings, fcontext) fExp
    where
      (Fun _ decls fExp) = fromJust $ lookup fcontext fId
      paramBindings = zip decls (map (evalV context) pExps)
  EIf e eT eE ->
    if pct == truePC
      then evalV context eT
      else
        if pct == falsePC
          then evalV context eE
          else (restrictedEvalET ||| pct) +++ (restrictedEvalEE ||| pcf)
    where
      (pct, pcf) = partition (evalV context e)
      restrictedEvalET = evalV (restrictContext context pct) eT
      restrictedEvalEE = evalV (restrictContext context pcf) eE

partition :: VarInt -> (PresenceCondition, PresenceCondition)
partition (Var lvint) =
  foldr
    (\(v, pc) (pct, pcf) -> (pct || ((v /= 0) && pc), pcf || ((v == 0) && pc)))
    (False, False)
    lvint

restrictContext :: RContext -> PresenceCondition -> RContext
restrictContext (vcontext, fcontext) pc = (restrictedVContext, fcontext)
  where
    restrictedVContext = [(vId, restrictedVInt) | (vId, vInt) <- vcontext, let restrictedVInt = vInt ||| pc]

(|||) :: VarInt -> PresenceCondition -> VarInt
(Var listPCv) ||| pcR = Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc && pcR, sat pc'])

(+++) :: VarInt -> VarInt -> VarInt
(Var lvint1) +++ (Var lvint2) = Var (lvint1 ++ lvint2)

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

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext) (f@(Fun fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f
