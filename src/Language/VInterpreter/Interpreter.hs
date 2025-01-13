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
    Prop,
    Var (..),
    VarValor(..),
    ffPC,
    sat,
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
import Debug.Trace
import qualified Data.Text as T

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

type VContext = Context Ident VarValor

type FContext = Context Ident Function

propAS :: Prop
propAS = mkBDDVar "A"

propBS :: Prop
propBS = mkBDDVar "B"

atbtS :: Prop
atbtS = propAS /\ propBS

atbfS :: Prop
atbfS = propAS /\ notBDD propBS

afbtS :: Prop
afbtS = notBDD propAS /\ propBS

afbfS :: Prop
afbfS = notBDD propAS /\ notBDD propBS

substitutionsS :: [(String, String)]
substitutionsS =
    [ (show atbtS, " atbt")
    , (show afbtS, " afbt")
    , (show atbfS, " atbf")
    , (show afbfS, " afbf")
    , (show tt, " tt")
    , (show ff, " ff")
    , (show ttPC, " ttPC")
    , (show ffPC, " ffPC")
    , (show propAS, " A")
    , (show (notBDD propAS), " ~A")
    ]

replaceStringS :: String -> String -> String -> String
replaceStringS old new text = T.unpack (T.replace (T.pack old) (T.pack new) (T.pack text))

substituteS :: String -> [(String, String)] -> String
substituteS text substitutions =
    let replaceAll :: [(String, String)] -> String -> String
        replaceAll [] txt = txt
        replaceAll ((key, value):xs) txt =
            replaceAll xs (replaceStringS key value txt)
    
    in replaceAll substitutions text

evalV :: RContext -> Exp -> VarValor
evalV context@(vcontext, fcontext) x = case x of
  EInt n -> VarInteger (Var [(n, ttPC)])
  ECon exp0 exp1  ->
    let val0 = evalV context exp0
        val1 = evalV context exp1
    in case (val0, val1) of
        (VarString s0, VarString s1) -> applyBinaryOperator VarString str context exp0 exp1 (++)
        (VarList l0, VarList l1) -> VarList (l0 ++ l1)
        _ -> error "Type error in concatenation"
  EAdd exp0 exp1 -> applyBinaryOperator VarInteger int context exp0 exp1 (+)
  ESub exp0 exp1 -> applyBinaryOperator VarInteger int context exp0 exp1 (-)
  EMul exp0 exp1 -> applyBinaryOperator VarInteger int context exp0 exp1 (*)
  EDiv exp0 exp1 -> applyBinaryOperator VarInteger int context exp0 exp1 div
  EOr exp0 exp1  -> applyBinaryOperator VarBool bool context exp0 exp1 (||)
  EAnd exp0 exp1 -> applyBinaryOperator VarBool bool context exp0 exp1 (&&)
  ENot exp1  -> applyUnaryOperator VarBool bool context exp1 not
  EVar vId -> fromJust $ lookup vcontext vId
  EStr s -> VarString (Var [(s, ttPC)])
  ETrue -> VarBool (Var [(True, ttPC)])
  EFalse -> VarBool (Var [(False, ttPC)])
  EPair p1 p2 -> VarPair (evalV context p1, evalV context p2)
  EList ls -> VarList (map (evalV context) ls)
  Call id pExps -> case id of
    Ident "head" -> head ls
    Ident "tail" ->  VarList (tail ls)
    Ident "isNil" -> VarInteger (Var [(boolToInt (null ls), ttPC)])
    Ident "fst" -> f
    Ident "snd" -> s
    Ident "isPair" -> case arg of
        VarPair _ -> VarInteger (Var [(1, ttPC)])
        _ -> VarInteger (Var [(0, ttPC)])
    Ident "isEqual" -> applyEqualOperator context (pExps !! 0) (pExps !! 1)
    Ident "lt" -> applyLtOperator context (pExps !! 0) (pExps !! 1)
    Ident "union" -> applyUnion context (pExps !! 0) (pExps !! 1)
    Ident "difference" -> applyDifference context (pExps !! 0) (pExps !! 1)
    Ident func -> evalV (paramBindings, fcontext) fExp
    where
      arg = evalV context (head pExps)
      ls = list arg
      (f,s) = pair arg
      (Fun _ _ decls fExp) = fromJust $ lookup fcontext id
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

applyEqualOperator :: RContext -> Exp -> Exp -> VarValor
applyEqualOperator context exp0 exp1 =
  let v0 = evalV context exp0
      v1 = evalV context exp1
  in case (v0, v1) of
      (VarInteger vi0, VarInteger vi1) ->
        VarInteger $ Var
          [ (boolToInt (a == b), pc0 /\ pc1)
          | (a, pc0) <- valList vi0
          , (b, pc1) <- valList vi1
          , sat (pc0 /\ pc1)
          ]
      (VarBool vb0, VarBool vb1) ->
        VarInteger $ Var
          [ (boolToInt (a == b), pc0 /\ pc1)
          | (a, pc0) <- valList vb0
          , (b, pc1) <- valList vb1
          , sat (pc0 /\ pc1)
          ]
      (VarString vs0, VarString vs1) ->
        VarInteger $ Var
          [ (boolToInt (a == b), pc0 /\ pc1)
          | (a, pc0) <- valList vs0
          , (b, pc1) <- valList vs1
          , sat (pc0 /\ pc1)
          ]
      _ -> VarInteger (Var [(0, ttPC)])

applyLtOperator :: RContext -> Exp -> Exp -> VarValor
applyLtOperator context exp0 exp1 =
  let v0 = evalV context exp0
      v1 = evalV context exp1
  in case (v0, v1) of
      (VarInteger vi0, VarInteger vi1) ->
        VarInteger $ Var
          [ (boolToInt (a < b), pc0 /\ pc1)
          | (a, pc0) <- valList vi0
          , (b, pc1) <- valList vi1
          , sat (pc0 /\ pc1)
          ]
      (VarBool vb0, VarBool vb1) ->
        VarInteger $ Var
          [ (boolToInt (a < b), pc0 /\ pc1)
          | (a, pc0) <- valList vb0
          , (b, pc1) <- valList vb1
          , sat (pc0 /\ pc1)
          ]
      (VarString vs0, VarString vs1) ->
        VarInteger $ Var
          [ (boolToInt (a < b), pc0 /\ pc1)
          | (a, pc0) <- valList vs0
          , (b, pc1) <- valList vs1
          , sat (pc0 /\ pc1)
          ]
      _ -> VarInteger (Var [(0, ttPC)])

applyUnion :: RContext -> Exp -> Exp -> VarValor
applyUnion context exp0 exp1 =
  let v0 = evalV context exp0
      v1 = evalV context exp1
  in case (v0, v1) of
      (VarList l0, VarList l1) ->
        VarList (unionLists l0 l1)
      _ -> error "Union should only be used to lists"

unionLists :: [VarValor] -> [VarValor] -> [VarValor]
unionLists xs l
  = foldl
      (\ l x
         -> if elemInList x l
            then l
            else x : l)
      l xs

elemInList :: VarValor -> [VarValor] -> Bool
elemInList _ [] = False
elemInList x (y:ys)
  | x == y    = True
  | otherwise = elemInList x ys

applyDifference :: RContext -> Exp -> Exp -> VarValor
applyDifference context exp0 exp1 =
  let v0 = evalV context exp0
      v1 = evalV context exp1
  in case (v0, v1) of
      (VarList l0, VarList l1) ->
        VarList (differenceLists l0 l1)
      _ -> error "Difference should only be used to lists"

differenceLists :: [VarValor] -> [VarValor] -> [VarValor]
differenceLists xs ys = filter (\x -> not (elemInList x ys)) xs

applyBinaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext -> Exp -> Exp -> (a -> a -> a) -> VarValor
applyBinaryOperator cons f context exp0 exp1 op =
  cons (Var
    [ (v1 `op` v2, pc1 /\ pc2)
      | (v1, pc1) <- valList (f (evalV context exp0)),
        (v2, pc2) <- valList (f (evalV context exp1)),
        sat (pc1 /\ pc2)
    ])

applyUnaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext -> Exp -> (a -> a) -> VarValor
applyUnaryOperator cons f context exp op =
  cons (Var
    [ (op v, pc1)
      | (v, pc1) <- valList (f (evalV context exp))
    ])

restrictContext :: RContext -> PresenceCondition -> RContext
restrictContext (vcontext, fcontext) pc = (restrictedVContext, fcontext)
  where
    restrictedVContext = [(vId, restrictedVInt) | (vId, v) <- vcontext, let restrictedVInt = v |||| pc]

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
updatecF (vcontext, fcontext) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f
