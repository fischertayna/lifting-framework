module Base.Functions where

import Base.Types (Valor(..), Context)
import Prelude hiding (lookup)
import Data.List (sortBy)
import Data.Maybe (fromJust)

applyBinaryOperator :: (a -> Valor) -> (Valor -> a) -> Valor -> Valor -> (a -> a -> a) -> Valor
applyBinaryOperator cons f v0 v1 op =
  cons (f v0 `op` f v1)

applyUnaryOperator :: (a -> Valor) -> (Valor -> a) -> Valor -> (a -> a) -> Valor
applyUnaryOperator cons f v0 op =
  cons (op (f v0))

applyIsPair :: Valor -> Valor
applyIsPair arg =
    case arg of
      ValorPair _ -> ValorInt 1
      _ -> ValorInt 0

applyIsEqual :: Valor -> Valor -> Valor
applyIsEqual v0 v1 = ValorInt (boolToInt (v0 == v1))

applyLt :: Valor -> Valor -> Valor
applyLt v0 v1 = ValorInt (boolToInt (v0 < v1))

applyIsMember :: Valor -> Valor -> Valor
applyIsMember v0 ls =
  case ls of
      ValorList vals -> ValorInt (boolToInt (elem v0 vals))
      _ -> error "isMember expects a VarList as second argument"

applyLength :: Valor -> Valor
applyLength v0 =
  case v0 of
      ValorList vals -> ValorInt (toInteger (length vals))
      _ -> error "length expects a VarList"

applySortList :: Valor -> Valor
applySortList v0 =
  case v0 of
      ValorList vals -> ValorList $ sortBy compareVarValor vals
      _ -> error "sortList expects a VarList"

compareVarValor :: Valor -> Valor -> Ordering
compareVarValor (ValorPair (k1, v1)) (ValorPair (k2, v2)) =
  case compareVarValor k1 k2 of
    EQ -> compareVarValor v1 v2
    result -> result
compareVarValor v1 v2 = compare v1 v2

applyUnion :: Valor -> Valor -> Valor
applyUnion v0 v1 =
  case (v0, v1) of
      (ValorList l0, ValorList l1) ->
        ValorList (unionLists l0 l1)
      _ -> error "Union should only be used to lists"

unionLists :: [Valor] -> [Valor] -> [Valor]
unionLists [] ys = ys
unionLists (x:xs) ys =
  let updatedYs = replaceOrAdd x ys
  in unionLists xs updatedYs

replaceOrAdd :: Valor -> [Valor] -> [Valor]
replaceOrAdd x [] = [x]
replaceOrAdd x (y:ys)
  | x == y = (y : ys)      
  | otherwise = (y : replaceOrAdd x ys)

elemInList :: Valor -> [Valor] -> Bool
elemInList _ [] = False
elemInList x (y:ys)
  | x == y    = True
  | otherwise = elemInList x ys

applyIntersection :: Valor -> Valor -> Valor
applyIntersection v0 v1 =
  case (v0, v1) of
      (ValorList l0, ValorList l1) ->
        ValorList (intersectionLists l0 l1)
      _ -> error "Union should only be used to lists"

intersectionLists :: [Valor] -> [Valor] -> [Valor]
intersectionLists [] _ = []
intersectionLists (x:xs) ys
  | elemInList x ys = x : intersectionLists xs ys
  | otherwise = intersectionLists xs ys

applyDifference :: Valor -> Valor -> Valor
applyDifference v0 v1 =
  case (v0, v1) of
      (ValorList l0, ValorList l1) ->
        ValorList (differenceLists l0 l1)
      _ -> error "Difference should only be used on lists"

differenceLists :: [Valor] -> [Valor] -> [Valor]
differenceLists xs ys = filter (\x -> not (elemInList x ys)) xs

boolToInt :: Bool -> Integer
boolToInt b
  | not b = 0
  | otherwise = 1

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