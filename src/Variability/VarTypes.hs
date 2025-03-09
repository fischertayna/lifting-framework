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
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import qualified Data.Text as T
import Text.Read (readMaybe)

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

instance Read Prop where
    readsPrec _ input = [(mkBDDVar input, "")]

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

instance (Read t) => Read (Var t) where
    readsPrec _ input =
        case readMaybe input of
            Just vals -> [(Var vals, "")]
            Nothing   -> []

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (f ^| ttPC)

instance Applicative Var where
    pure :: a -> Var a
    pure  = (^| ttPC)
    (<*>) :: Var (a -> b) -> Var a -> Var b
    (<*>) = apply

-- instance Monad Var where
--   (>>=) :: Var a -> (a -> Var b) -> Var b
--   (Var vs) >>= f = unions [applyOnVal f v | v <- vs]
--     where
--       applyOnVal :: (a -> Var b) -> Val a -> Var b
--       applyOnVal f (a, pca) = let (Var bv) = f a in mkVars [(b, pcr) | (b, pcb) <- bv, let pcr = pca /\ pcb, sat pcr]

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

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

atbt :: Prop
atbt = propA /\ propB

atbf :: Prop
atbf = propA /\ notBDD propB

afbt :: Prop
afbt = notBDD propA /\ propB

afbf :: Prop
afbf = notBDD propA /\ notBDD propB

substitutions :: [(String, String)]
substitutions =
    [ (show atbt, " atbt")
    , (show afbt, " afbt")
    , (show atbf, " atbf")
    , (show afbf, " afbf")
    , (show tt, " tt")
    , (show ff, " ff")
    , (show ttPC, " ttPC")
    , (show ffPC, " ffPC")
    , (show propA, " A")
    , (show (notBDD propA), " ~A")
    ]

replaceString :: String -> String -> String -> String
replaceString old new text = T.unpack (T.replace (T.pack old) (T.pack new) (T.pack text))

substituteWithSubstitutions :: [(String, String)] -> String -> String
substituteWithSubstitutions substitutions text =
    let replaceAll :: [(String, String)] -> String -> String
        replaceAll [] txt = txt
        replaceAll ((key, value):xs) txt =
            replaceAll xs (replaceString key value txt)
    
    in replaceAll substitutions text

substitute :: String -> String
substitute text = substituteWithSubstitutions substitutions text

(++++) :: VarValor -> VarValor -> VarValor
(VarInteger lvint1) ++++ (VarInteger lvint2)
  | isEmpty lvint1 = doTrace "Integer: lvint1 is empty" (VarInteger lvint1) (VarInteger lvint2) (VarInteger lvint2)
  | isEmpty lvint2 = doTrace "Integer: lvint2 is empty" (VarInteger lvint1) (VarInteger lvint2) (VarInteger lvint1)
  | otherwise      = doTrace "Integer: combining" (VarInteger lvint1) (VarInteger lvint2) (VarInteger (lvint1 +++ lvint2))
(VarBool lvint1) ++++ (VarBool lvint2)
  | isEmpty lvint1 = doTrace "Bool: lvint1 is empty" (VarBool lvint1) (VarBool lvint2) (VarBool lvint2)
  | isEmpty lvint2 = doTrace "Bool: lvint2 is empty" (VarBool lvint1) (VarBool lvint2) (VarBool lvint1)
  | otherwise      = doTrace "Bool: combining" (VarBool lvint1) (VarBool lvint2) (VarBool (lvint1 +++ lvint2))
(VarString lvint1) ++++ (VarString lvint2)
  | isEmpty lvint1 = doTrace "String: lvint1 is empty" (VarString lvint1) (VarString lvint2) (VarString lvint2)
  | isEmpty lvint2 = doTrace "String: lvint2 is empty" (VarString lvint1) (VarString lvint2) (VarString lvint1)
  | otherwise      = doTrace "String: combining" (VarString lvint1) (VarString lvint2) (VarString (lvint1 +++ lvint2))
(VarList list1) ++++ (VarList list2)
  | isEmptyVarList list1 && isEmptyVarList list2 = doTrace "List: both lists empty" (VarList list1) (VarList list2) (VarList [])
  | isEmptyVarList list1 = doTrace "List: list1 is empty" (VarList list1) (VarList list2) (VarList list2)
  | isEmptyVarList list2 = doTrace "List: list2 is empty" (VarList list1) (VarList list2) (VarList list1)
  | otherwise = 
      let paddedList1 = padToLength list1 list2
          paddedList2 = padToLength list2 list1
          combined = zipWith handleEmpty paddedList1 paddedList2
      in doTrace "List: combining lists" (VarList list1) (VarList list2) (VarList combined)
--  | otherwise = doTrace "List: combining lists" (VarList list1) (VarList list2) (VarList (list1 ++ list2))
--  | otherwise = doTrace "List: combining lists" (VarList list1) (VarList list2) (VarList [handleEmpty v1 v2 | v1 <- list1, v2 <- list2])
(VarPair (v1, v2)) ++++ (VarPair (w1, w2)) =
  doTrace "Pair: combining" (VarPair (v1, v2)) (VarPair (w1, w2)) (VarPair (handleEmpty v1 w1, handleEmpty v2 w2))
v1 ++++ v2 = error $ "Mismatched types for ++++ operator: " ++ show v1 ++ " and " ++ show v2

-- Helper function for tracing
doTrace :: (Show a, Show b) => String -> a -> b -> b -> b
doTrace msg val1 val2 result = doTraceOrResult False msg val1 val2 result

doTraceOrResult :: (Show a, Show b) => Bool -> String -> a -> b -> b -> b
doTraceOrResult active msg val1 val2 result
  | active = trace ("\n\nTrace: \n" ++ msg ++ "\n val1: " ++ substitute (show val1) ++ "\n val2: " ++ substitute (show val2) ++ "\n result: " ++ substitute (show result)) result
  | otherwise = result

isEmpty :: Var a -> Bool
isEmpty (Var vals) = null vals

isEmptyVarList :: [VarValor] -> Bool
isEmptyVarList = all isEmptyVarValor

isEmptyVarValor :: VarValor -> Bool
isEmptyVarValor (VarInteger (Var vals)) = null vals
isEmptyVarValor (VarBool (Var vals)) = null vals
isEmptyVarValor (VarString (Var vals)) = null vals
isEmptyVarValor (VarList []) = True
isEmptyVarValor (VarList _) = False
isEmptyVarValor (VarPair (v1, v2)) = isEmptyVarValor v1 && isEmptyVarValor v2

padToLength :: [VarValor] -> [VarValor] -> [VarValor]
padToLength xs ys = xs ++ replicate (length ys - length xs) emptyVarValor

emptyVarValor :: VarValor
emptyVarValor = VarList []

handleEmpty :: VarValor -> VarValor -> VarValor
handleEmpty v1 v2
  | isEmptyVarValor v1 = v2
  | isEmptyVarValor v2 = v1
  | otherwise          = v1 ++++ v2

(||||) :: VarValor -> PresenceCondition -> VarValor
(VarInteger (Var listPCv)) |||| pcR = VarInteger (Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))
(VarString (Var listPCv)) |||| pcR = VarString (Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))
(VarBool (Var listPCv)) |||| pcR = VarBool(Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc']))
(VarList list) |||| pcR = VarList [v |||| pcR | v <- list]
(VarPair (v1, v2)) |||| pcR = VarPair (v1 |||| pcR, v2 |||| pcR)

data VarValor = VarInteger { int :: Var Integer }
              | VarBool { bool :: Var Bool }
              | VarString { str :: Var String }
              | VarList { list :: [VarValor] }
              | VarPair { pair :: (VarValor, VarValor) }
    deriving (Show, Eq, Read)

instance Hashable VarValor where
  hashWithSalt salt (VarInteger i) = hashWithSalt salt i
  hashWithSalt salt (VarBool b)    = hashWithSalt salt b
  hashWithSalt salt (VarString s)  = hashWithSalt salt s
  hashWithSalt salt (VarList vs)   = hashWithSalt salt vs
  hashWithSalt salt (VarPair (v1, v2)) = salt `hashWithSalt` v1 `hashWithSalt` v2

instance (Hashable t, Ord t) => Hashable (Var t) where
  hashWithSalt salt (Var vals) = salt `hashWithSalt` vals

-- Other instances

showPCs :: Var a -> String
showPCs (Var v) = "{" ++ L.intercalate ", " (map (\(_, pc) -> show pc) v) ++ "}"

type Context k v = [(k, v)]