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
import Debug.Trace
import Data.List (sortBy)
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

eval :: RContext Mem -> State Mem (Exp -> State Mem VarValor)
eval context@(vcontext, fcontext, memoizedFunctionName) =
  return
    ( \x -> case x of
        (ECon exp0 exp1) -> do
          val0 <- eval context <.> return exp0
          val1 <- eval context <.> return exp1
          case (val0, val1) of
            (VarString s0, VarString s1) -> applyBinaryOperator VarString str context exp0 exp1 (++)
            (VarList l0, VarList l1) -> return $ VarList (l0 ++ l1)
            _ -> error "Type error in concatenation"
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
          Ident "isPair" -> do
            vals <- mapM (\e -> eval context <.> return e) pExps
            case vals of
              [VarPair _] -> return $ VarInteger (Var [(1, ttPC)])
              _ -> return $ VarInteger (Var [(0, ttPC)])
          Ident "isEqual" -> applyEqualOperator context (pExps !! 0) (pExps !! 1)
          Ident "sortList" -> applySortList context (pExps !! 0)
          Ident "lt" -> applyLtOperator context (pExps !! 0) (pExps !! 1)
          Ident "union" -> applyUnion context (pExps !! 0) (pExps !! 1)
          Ident "difference" -> applyDifference context (pExps !! 0) (pExps !! 1)
          _ ->
            if fId == memoizedFunctionName
              then memoizedCall context fId pExps
              else
                ( let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
                  in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
                      in eval (paramBindings, fcontext, memoizedFunctionName) <.> return fExp
                )
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

applyEqualOperator :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyEqualOperator context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = case (v0, v1) of
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
  return $ result

applyLtOperator :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyLtOperator context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = case (v0, v1) of
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
  return $ result

applySortList :: RContext Mem -> Exp -> State Mem VarValor
applySortList context exp = do
  v0 <- (eval context <.> return exp)
  let result = case v0 of
        VarList vals -> VarList $ sortBy compareVarValor vals
        _ -> error "sortList expects a VarList"
  return $ result


compareVarValor :: VarValor -> VarValor -> Ordering
compareVarValor (VarInteger (Var ((v1, _):_))) (VarInteger (Var ((v2, _):_))) = compare v1 v2
compareVarValor (VarString (Var ((s1, _):_))) (VarString (Var ((s2, _):_))) = compare s1 s2
compareVarValor (VarBool (Var ((b1, _):_))) (VarBool (Var ((b2, _):_))) = compare (boolToInt b1) (boolToInt b2)
compareVarValor (VarPair (k1, v1)) (VarPair (k2, v2)) =
  case compareVarValor k1 k2 of
    EQ -> compareVarValor v1 v2
    result -> result
compareVarValor (VarInteger (Var [])) _ = error "VarInteger has no values"
compareVarValor (VarString (Var [])) _ = error "VarString has no values"
compareVarValor (VarBool (Var [])) _ = error "VarBool has no values"
compareVarValor _ (VarInteger (Var [])) = error "VarInteger has no values"
compareVarValor _ (VarString (Var [])) = error "VarString has no values"
compareVarValor _ (VarBool (Var [])) = error "VarBool has no values"
compareVarValor v1 v2 =
  error $ "Cannot compare elements of types: " ++ show v1 ++ " and " ++ show v2


doTrace :: (Show a, Show b) => String -> a -> b -> b -> b
doTrace msg val1 val2 result = doTraceOrResult False msg val1 val2 result

applyUnion :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyUnion context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = case (v0, v1) of
                (VarList l0, VarList l1) ->
                  VarList (unionLists l0 l1)
                _ -> error "Union should only be used to lists"
  return $ result

unionLists :: [VarValor] -> [VarValor] -> [VarValor]
unionLists [] ys = ys
unionLists (x:xs) ys =
  let updatedYs = replaceOrAdd x ys
  in unionLists xs updatedYs

replaceOrAdd :: VarValor -> [VarValor] -> [VarValor]
replaceOrAdd x [] = [x]
replaceOrAdd x (y:ys)
  | x == y = (y : ys)
  | areEqualIgnoringPresence x y =
      let presenceX = extractPresence x
          presenceY = extractPresence y
      in case comparePresence presenceX presenceY of
        Merge    -> doTrace "merge elements: " x (y:ys) (combinePresence x y : ys)
        Coexist  -> doTrace "coexist: " x (y:ys) (x : y : ys)        
  | otherwise = doTrace "otherwise" x (y:ys) (y : replaceOrAdd x ys)
data PresenceComparison = Merge | Coexist

comparePresence :: PresenceCondition -> PresenceCondition -> PresenceComparison
comparePresence pcX pcY
  | unsat (pcX /\ pcY) = Coexist
  | otherwise = Merge

extractPresence :: VarValor -> PresenceCondition
extractPresence (VarString (Var [(_, pc)])) = pc
extractPresence (VarBool (Var [(_, pc)])) = pc
extractPresence (VarInteger (Var [(_, pc)])) = pc
extractPresence (VarList _) = ttPC
extractPresence (VarPair _) = ttPC

combinePresence :: VarValor -> VarValor -> VarValor
combinePresence (VarString (Var [(v1, pc1)])) (VarString (Var [(v2, pc2)]))
  | v1 == v2 = VarString (Var [(v1, pc1 \/ pc2)])
combinePresence (VarBool (Var [(v1, pc1)])) (VarBool (Var [(v2, pc2)]))
  | v1 == v2 = VarBool (Var [(v1, pc1 \/ pc2)])
combinePresence (VarInteger (Var [(v1, pc1)])) (VarInteger (Var [(v2, pc2)]))
  | v1 == v2 = VarInteger (Var [(v1, pc1 \/ pc2)])
combinePresence (VarPair (p11, p12)) (VarPair (p21, p22)) =
  VarPair (combinePresence p11 p21, combinePresence p12 p22)
combinePresence _ _ = error "Cannot combine different types of VarValor"

areEqualIgnoringPresence :: VarValor -> VarValor -> Bool
areEqualIgnoringPresence (VarString (Var [(s1, _)])) (VarString (Var [(s2, _)])) = s1 == s2
areEqualIgnoringPresence (VarBool (Var [(s1, _)])) (VarBool (Var [(s2, _)])) = s1 == s2
areEqualIgnoringPresence (VarInteger (Var [(s1, _)])) (VarInteger (Var [(s2, _)])) = s1 == s2
areEqualIgnoringPresence (VarPair (p11, p12)) (VarPair (p21, p22)) = (p11 == p21 && p12 == p22)
areEqualIgnoringPresence (VarList l1) (VarList l2) = (l1 == l2)
areEqualIgnoringPresence v1 v2 = v1 == v2

applyDifference :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyDifference context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = case (v0, v1) of
              (VarList l0, VarList l1) ->
                VarList (differenceLists l0 l1)
              _ -> error "Difference should only be used to lists"
  return $ result

differenceLists :: [VarValor] -> [VarValor] -> [VarValor]
differenceLists xs ys =
  let subtracted = map (`subtractFromList` ys) xs
  in filter (not . isEmptyVarValor) subtracted

subtractFromList :: VarValor -> [VarValor] -> VarValor
subtractFromList (VarPair (k1, v1)) ys =
  case filter (\(VarPair (k2, _)) -> areEqualIgnoringPresence k1 k2) ys of
    [] -> VarPair (k1, v1)
    matches ->
      let newK = subtractVals k1 (map (\(VarPair (k, _)) -> k) matches)
          newV = subtractVals v1 (map (\(VarPair (_, v)) -> v) matches)
      in if isEmptyVarValor newK && isEmptyVarValor newV
         then VarList []
         else VarPair (newK, newV)
subtractFromList var _ = var

subtractVals :: VarValor -> [VarValor] -> VarValor
subtractVals (VarString (Var vals1)) vars =
  let vals2 = concatMap (\(VarString (Var v)) -> v) vars
      result = map (\(v1, pc1) -> 
                      let adjustedPC = pc1 /\ negPC (disj (map snd vals2))
                      in (v1, adjustedPC)) vals1
      filteredResult = filter (not . unsat . snd) result
   in VarString (Var filteredResult)

isEmptyVarValor :: VarValor -> Bool
isEmptyVarValor (VarString (Var vals)) = null vals
isEmptyVarValor (VarList vals) = all isEmptyVarValor vals
isEmptyVarValor (VarPair (k, v)) = isEmptyVarValor k || isEmptyVarValor v
isEmptyVarValor _ = False

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

