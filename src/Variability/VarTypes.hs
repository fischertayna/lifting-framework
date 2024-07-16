{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module Variability.VarTypes where

import Cudd.Cudd (DDManager, DDNode, bAnd, bNot, bOr, cuddInit, readLogicZero, readOne, ithVar, nodeReadIndex)
import qualified Data.HashTable.IO as H
import GHC.IO (unsafePerformIO)
import Data.Hashable ( Hashable(hashWithSalt) )
import qualified Data.List as L
import GHC.StableName (makeStableName)
import Control.Exception (assert)
import Data.List (sort)

type HashTable k v = H.BasicHashTable k v

instance Hashable DDNode where
    {-# INLINE hashWithSalt #-}
    hashWithSalt :: Int -> DDNode -> Int
    hashWithSalt s d = hashWithSalt s (nodeReadIndex d)

newtype Prop = Prop
  { b :: DDNode
  }

instance Eq Prop where
  {-# INLINE (==) #-}
  (==) :: Prop -> Prop -> Bool
  (Prop b0) == (Prop b1) = b0 == b1

instance Ord Prop where
    (<=) :: Prop -> Prop -> Bool
    (Prop b0) <= (Prop b1) = nodeReadIndex b0 <= nodeReadIndex b1

instance Hashable Prop where
    {-# INLINE hashWithSalt #-}
    hashWithSalt :: Int -> Prop -> Int
    hashWithSalt s (Prop b) = hashWithSalt s b

instance Show Prop where
    {-# INLINE show #-}
    show :: Prop -> String
    show (Prop b) = show b

manager :: Cudd.Cudd.DDManager
manager = cuddInit

{-# INLINE tt #-}
tt :: Prop
tt = newBDD (readOne manager)

{-# INLINE ff #-}
ff :: Prop
ff = newBDD (readLogicZero manager)

{-# INLINE newBDD #-}
newBDD :: DDNode -> Prop
newBDD = Prop

mkBDDVar :: String -> Prop
mkBDDVar name =
  let i = lookupVar name
      r = ithVar manager i
   in newBDD r

{-# INLINE andBDD #-}
andBDD :: Prop -> Prop -> Prop
andBDD p0@(Prop b0) p1@(Prop b1)
  | p0 == ff = ff
  | p1 == ff = ff
  | p0 == tt = p1
  | p1 == tt = p0
  | otherwise = newBDD $ bAnd manager b0 b1

{-# INLINE orBDD #-}
orBDD :: Prop -> Prop -> Prop
orBDD p0@(Prop b0) p1@(Prop b1)
  | p0 == tt = tt
  | p1 == tt = tt
  | p0 == ff = p1
  | p1 == ff = p0
  | otherwise = newBDD $ bOr manager b0 b1

{-# INLINE notBDD #-}
notBDD :: Prop -> Prop
notBDD p@(Prop b)
  | p == tt = ff
  | p == ff = tt
  | otherwise = newBDD $ bNot manager b

sat :: Prop -> Bool
sat p = p /= ff

unsat :: Prop -> Bool
unsat p = p == ff

var2index :: HashTable String Int
{-# NOINLINE var2index #-}
var2index = unsafePerformIO H.new

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
  i <- H.lookup var2index v
  case i of
    Nothing -> do
      i' <- htSize var2index
      !d0 <- H.insert var2index v i'
      return $ i'
    Just i' -> return i'

htSize :: (Eq k, Hashable k) => HashTable k v -> IO Int
htSize h = do
    xs <- H.toList h
    return $ length xs

disj :: [Prop] -> Prop
disj = foldr orBDD ff

type PresenceCondition = Prop

type Context = PresenceCondition

type PCExpr = Prop

{-# INLINE (/\) #-}
(/\) :: Prop -> Prop -> Prop
(/\) = andBDD

{-# INLINE (\/) #-}
(\/) :: Prop -> Prop -> Prop
(\/) = orBDD

{-# INLINE negPC #-}
negPC :: Prop -> Prop
negPC = notBDD

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t]

instance (Eq t, Ord t) => Eq (Var t) where
    (Var vals1) == (Var vals2) = and $ zipWith eqVal (sort vals1) (sort vals2)
      where
        eqVal (v1, pc1) (v2, pc2) = v1 == v2 && pc1 == pc2

findVal :: t -> [Val t] -> (t -> t -> Bool) -> [Val t]
findVal _ [] _ = []
findVal v ((x,pc):xs) cmp = if cmp v x then (x,pc) : rest else rest
    where rest = findVal v xs cmp

{-# INLINE phelem #-}
phelem :: t -> [t] -> (t -> t -> Bool) -> Bool
phelem v xs cmp = any (cmp v) xs


groupVals_ :: [Val t] -> [t] -> (t -> t -> Bool) -> [Val t]
groupVals_ [] _ _ = []
groupVals_ ((x,xpc):xs) ds cmp =
    if phelem x ds cmp then rest else
        let ms = findVal x xs cmp
            pc = disj (xpc:map snd ms)
        in  (x,pc) : rest
    where rest = groupVals_ xs (x:ds) cmp

groupVals :: [Val t] -> (t -> t -> Bool) -> [Val t]
groupVals xs = groupVals_ xs []

{-# INLINE (===) #-}
(===) :: a -> a -> Bool
(!x) === (!y) = unsafePerformIO $ do 
    nx <- makeStableName $! x 
    ny <- makeStableName $! y 
    return (nx == ny)


compact :: Var t -> Var t
compact (Var v) = Var (groupVals v (===))

instance Show a => Show (Var a) where
    show :: Show a => Var a -> String
    show v' =
        let (Var v) = compact v'
        in "{" ++ L.intercalate ", " (map show v) ++ "}"

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (f ^| ttPC)

instance Applicative Var where
    pure :: a -> Var a
    pure  = (^| ttPC)
    (<*>) :: Var (a -> b) -> Var a -> Var b
    (<*>) = apply

disjInv :: Var t -> Bool
disjInv v'@(Var v) = all (\((_, pc1), (_, pc2)) -> unsat (pc1 /\ pc2)) (pairs v)

compInv :: Var t -> Bool
compInv (Var v) =
  foldr (\(_, pc) pc' -> pc \/ pc') ffPC v == ttPC

mkVar :: t -> PresenceCondition -> Var t
{-# INLINE mkVar #-}
mkVar v pc = Var [(v, pc)]

mkVars :: [(t, PresenceCondition)] -> Var t
mkVars = Var

(^|) :: t -> PresenceCondition -> Var t
x ^| pc = mkVar x pc

infixl 9 ^|

union :: Var t -> Var t -> Var t
union x@(Var a) y@(Var b) =
  let result = Var (a ++ b)
   in result

unions :: [Var t] -> Var t
unions = foldr union (Var [])

{-# INLINE apply_ #-}
apply_ :: Val (a -> b) -> Var a -> Var b
apply_ (fn, !fnpc) x'@(Var x) =
  mkVars $ [(fn v, pc') | (v, !pc) <- x, let !pc' = fnpc /\ pc, sat pc']

{-# INLINE apply #-}
apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x =
  assert (disjInv f) $
    assert (disjInv x) $
      unions [apply_ f x | f <- fn]

pairs :: [t] -> [(t, t)]
pairs [] = []
pairs xs = zip xs (tail xs)

ttPC :: Prop
ttPC = tt

ffPC :: Prop
ffPC = ff

valList :: Var a -> [Val a]
valList (Var ls) = ls

(|||) :: Var a -> PresenceCondition -> Var a
(Var listPCv) ||| pcR = Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc'])

(+++) :: Var a -> Var a -> Var a
(Var lvint1) +++ (Var lvint2) = Var (lvint1 ++ lvint2)

(++++) :: VarValor -> VarValor -> VarValor
(VarInteger lvint1) ++++ (VarInteger lvint2) = VarInteger (lvint1 +++ lvint2)
(VarBool lvint1) ++++ (VarBool lvint2) = VarBool (lvint1 +++ lvint2)
(VarString lvint1) ++++ (VarString lvint2) = VarString (lvint1 +++ lvint2)

(||||) :: VarValor -> PresenceCondition -> VarValor
(VarInteger (Var listPCv)) |||| pcR = VarInteger (Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))
(VarString (Var listPCv)) |||| pcR = VarString (Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))
(VarBool (Var listPCv)) |||| pcR = VarBool(Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))

data VarValor
    = VarInteger { int :: Var Integer
        }
    | VarBool
        { bool :: Var Bool
        }
    | VarString
        { str :: Var String
        }
    | VarList
        { list :: [VarValor]
        }
    | VarPair
        { pair :: (VarValor, VarValor)
        }
    deriving (Show, Eq)
