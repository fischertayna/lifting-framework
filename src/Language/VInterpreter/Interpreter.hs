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
    ffPC,
    sat,
    ttPC,
    valList,
    (+++),
    (|||), (/\), (\/),
  )
import Prelude hiding (lookup)

data VarValor
    = VarInteger { i :: VarInt
        }
    | VarList
        { l :: [VarValor]
        }
    | VarPair
        { p :: (VarValor, VarValor)
        }
    deriving (Show)

type VarInt = Var Integer

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

type VContext = Context Ident VarValor

type FContext = Context Ident Function

evalV :: RContext -> Exp -> VarValor
evalV context@(vcontext, fcontext) x = case x of
  EInt n -> VarInteger (Var [(n, ttPC)])
  EAdd exp0 exp1 -> applyOperator context exp0 exp1 (+)
  ESub exp0 exp1 -> applyOperator context exp0 exp1 (-)
  EMul exp0 exp1 -> applyOperator context exp0 exp1 (*)
  EDiv exp0 exp1 -> applyOperator context exp0 exp1 div
  EVar vId -> fromJust $ lookup vcontext vId
  EPair p1 p2 -> VarPair (evalV context p1, evalV context p2)
  EList list -> VarList (map (evalV context) list)
  Call id pExps -> case id of
    Ident "head" -> head list
    Ident "tail" ->  VarList (tail list)
    Ident "isNil" -> VarInteger (Var [(boolToInt (null list), ttPC)])
    Ident "fst" -> f
    Ident "snd" -> s
    Ident func -> evalV (paramBindings, fcontext) fExp
    where
      arg = evalV context (head pExps)
      list = l arg
      (f,s) = p arg
      (Fun _ decls fExp) = fromJust $ lookup fcontext id
      paramBindings = zip decls (map (evalV context) pExps)
  EIf e eT eE ->
    if pct == ttPC
      then evalV context eT
      else
        if pct == ffPC
          then evalV context eE
          else (restrictedEvalET |||| pct) ++++ (restrictedEvalEE |||| pcf)
    where
      (pct, pcf) = partition (evalV context e)
      restrictedEvalET = evalV (restrictContext context pct) eT
      restrictedEvalEE = evalV (restrictContext context pcf) eE

boolToInt :: Bool -> Integer
boolToInt b
  | not b = 0
  | otherwise = 1

evalPV :: Program -> VarValor -> VarValor
evalPV (Prog fs) input = evalV context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

applyOperator :: RContext -> Exp -> Exp -> (Integer -> Integer -> Integer) -> VarValor
applyOperator context exp0 exp1 op =
  VarInteger (Var
    [ (i1 `op` i2, pc1 /\ pc2)
      | (i1, pc1) <- valList (i (evalV context exp0)),
        (i2, pc2) <- valList (i (evalV context exp1)),
        sat (pc1 /\ pc2)
    ])

restrictContext :: RContext -> PresenceCondition -> RContext
restrictContext (vcontext, fcontext) pc = (restrictedVContext, fcontext)
  where
    restrictedVContext = [(vId, VarInteger  restrictedVInt) | (vId, vInt) <- vcontext, let restrictedVInt = i vInt ||| pc]

partition :: VarValor -> (PresenceCondition, PresenceCondition)
partition (VarInteger (Var lvint)) =
  foldr
    (\(v, pc) (pct, pcf) -> if v /= 0 then (pct \/ pc, pcf) else (pct, pcf \/ pc))
    (ffPC, ffPC)
    lvint

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


(++++) :: VarValor -> VarValor -> VarValor
(VarInteger lvint1) ++++ (VarInteger lvint2) = VarInteger (lvint1 +++ lvint2)

(||||) :: VarValor -> PresenceCondition -> VarValor
(VarInteger (Var listPCv)) |||| pcR = VarInteger (Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))