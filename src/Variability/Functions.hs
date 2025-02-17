module Variability.Functions where

import Data.Maybe (fromJust)
import Variability.VarTypes
  ( PresenceCondition,
    Prop,
    Var (..),
    VarValor(..),
    Context,
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
import qualified Data.Text as T
import Data.List (sortBy)

boolToInt :: Bool -> Integer
boolToInt b
  | not b = 0
  | otherwise = 1

applyEqualOperator :: VarValor -> VarValor -> VarValor
applyEqualOperator v0 v1 =
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
            (VarPair p1, VarPair p2) ->
                VarInteger (Var [(boolToInt (p1 == p2), ttPC)])
            (VarList l1, VarList l2) ->
                VarInteger (Var [(boolToInt (l1 == l2), ttPC)])
            _ -> VarInteger (Var [(0, ttPC)])
    in doTraceOrResult False "eq: " v0 v1 result

applyLtOperator :: VarValor -> VarValor -> VarValor
applyLtOperator v0 v1 =
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
    in doTraceOrResult False "lt: " v0 v1 result

applySortList :: VarValor -> VarValor
applySortList v0 =
  let result = case v0 of
          VarList vals -> VarList $ sortBy compareVarValor vals
          _ -> error "sortList expects a VarList"
  in doTraceOrResult False "sortList: " v0 (VarList[]) result


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

applyUnion :: VarValor -> VarValor -> VarValor
applyUnion v0 v1 =
  let result = case (v0, v1) of
                    (VarList l0, VarList l1) ->
                      VarList (unionLists l0 l1)
                    _ -> error "Union should only be used to lists"
     in doTraceOrResult False "union: " v0 v1 result

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

applyDifference :: VarValor -> VarValor -> VarValor
applyDifference v0 v1 =
  let result = case (v0, v1) of
                  (VarList l0, VarList l1) ->
                    VarList (differenceLists l0 l1)
                  _ -> error "Difference should only be used to lists"
      in doTraceOrResult False "difference: " v0 v1 result

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
isEmptyVarValor (VarBool (Var vals)) = null vals
isEmptyVarValor (VarInteger (Var vals)) = null vals
isEmptyVarValor (VarList vals) = all isEmptyVarValor vals
isEmptyVarValor (VarPair (k, v)) = isEmptyVarValor k || isEmptyVarValor v
-- isEmptyVarValor _ = False

applyIsMember :: VarValor -> VarValor -> VarValor
applyIsMember element (VarList lst) =
  let result = VarInteger $ Var
        [ ( boolToInt (sat (pc /\ disj (map (checkElem (val, pc)) lst)) )
          , pc
          )
        | (val, pc) <- valList (wrapAsVar element)
        ]
  in doTraceOrResult False "isMember: " element (VarList lst) result
applyIsMember _ _ = error "isMember should be used with a VarList as the second argument"

checkElem :: (VarValor, PresenceCondition) -> VarValor -> PresenceCondition
checkElem (v, pc) item =
  disj [ pc /\ pc' | (vi, pc') <- valList (wrapAsVar item)
                   , areEqualIgnoringPresence v vi
         ]

-- Updated wrapAsVar: For atomic types, preserve each alternative’s presence condition.
wrapAsVar :: VarValor -> Var VarValor
wrapAsVar (VarInteger (Var vs)) = Var [ (VarInteger (Var [(n, pc)]), pc) | (n, pc) <- vs ]
wrapAsVar (VarBool (Var vs))    = Var [ (VarBool (Var [(b, pc)]), pc) | (b, pc) <- vs ]
wrapAsVar (VarString (Var vs))  = Var [ (VarString (Var [(s, pc)]), pc) | (s, pc) <- vs ]
wrapAsVar (VarList v)           = Var [(VarList v, ttPC)]  -- lists & pairs can remain with ttPC
wrapAsVar (VarPair (v1, v2))    = Var [(VarPair (v1, v2), ttPC)]

applyIntersection :: VarValor -> VarValor -> VarValor
applyIntersection v0 v1 =
  let result = case (v0, v1) of
                  (VarList l0, VarList l1) ->
                    VarList (intersectionLists l0 l1)
                  _ -> error "Intersection should only be used on lists"
  in doTraceOrResult False "intersection: " v0 v1 result

intersectionLists :: [VarValor] -> [VarValor] -> [VarValor]
intersectionLists [] _ = []
intersectionLists (x:xs) ys =
  case findMatching x ys of
    Just y ->
      let merged = mergePresence x y
      in if isEmptyVarValor merged
         then intersectionLists xs ys
         else merged : intersectionLists xs ys
    Nothing -> intersectionLists xs ys

findMatching :: VarValor -> [VarValor] -> Maybe VarValor
findMatching x ys =
  case [ y | y <- ys, areEqualIgnoringPresence x y ] of
       (y:_) -> Just y
       [] -> Nothing

-- Merging alternatives for VarString, VarBool, and VarInteger now considers all alternatives.
mergePresence :: VarValor -> VarValor -> VarValor
mergePresence (VarString (Var vals1)) (VarString (Var vals2)) =
  let merged = [ (s, pc1 /\ pc2)
               | (s, pc1) <- vals1
               , (s2, pc2) <- vals2
               , s == s2
               , sat (pc1 /\ pc2)
               ]
  in if null merged then VarList [] else VarString (Var merged)
mergePresence (VarBool (Var vals1)) (VarBool (Var vals2)) =
  let merged = [ (b, pc1 /\ pc2)
               | (b, pc1) <- vals1
               , (b2, pc2) <- vals2
               , b == b2
               , sat (pc1 /\ pc2)
               ]
  in if null merged then VarList [] else VarBool (Var merged)
mergePresence (VarInteger (Var vals1)) (VarInteger (Var vals2)) =
  let merged = [ (n, pc1 /\ pc2)
               | (n, pc1) <- vals1
               , (n2, pc2) <- vals2
               , n == n2
               , sat (pc1 /\ pc2)
               ]
  in if null merged then VarList [] else VarInteger (Var merged)
mergePresence (VarPair (p1, p2)) (VarPair (q1, q2)) =
  let merged1 = mergePresence p1 q1
      merged2 = mergePresence p2 q2
  in if isEmptyVarValor merged1 || isEmptyVarValor merged2
     then VarList [] else VarPair (merged1, merged2)
mergePresence (VarList l1) (VarList l2) =
  let mergedList = intersectionLists l1 l2
  in if null mergedList then VarList [] else VarList mergedList
mergePresence _ _ = VarList []

applyBinaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> VarValor -> VarValor -> (a -> a -> a) -> VarValor
applyBinaryOperator cons f vv1 vv2 op =
  cons (Var
    [ (v1 `op` v2, pc1 /\ pc2)
      | (v1, pc1) <- valList (f vv1),
        (v2, pc2) <- valList (f vv2),
        sat (pc1 /\ pc2)
    ])

applyUnaryOperator :: (Var a -> VarValor) -> (VarValor -> Var a) -> VarValor -> (a -> a) -> VarValor
applyUnaryOperator cons f v0 op =
  cons (Var
    [ (op v, pc1)
      | (v, pc1) <- valList (f v0)
    ])

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
