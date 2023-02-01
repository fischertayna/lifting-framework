module Variability.VarTypes where

type PresenceCondition = Bool

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t] deriving (Show)

type VarInt = Var Integer

valList :: VarInt -> [Val Integer]
valList (Var ls) = ls

sat :: PresenceCondition -> Bool
sat b = b == truePC

unsat :: PresenceCondition -> Bool
unsat b = b == falsePC

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

partition :: VarInt -> (PresenceCondition, PresenceCondition)
partition (Var lvint) =
  foldr
    (\(v, pc) (pct, pcf) -> (pct || ((v /= 0) && pc), pcf || ((v == 0) && pc)))
    (False, False)
    lvint

(|||) :: VarInt -> PresenceCondition -> VarInt
(Var listPCv) ||| pcR = Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc && pcR, sat pc'])

(+++) :: VarInt -> VarInt -> VarInt
(Var lvint1) +++ (Var lvint2) = Var (lvint1 ++ lvint2)
