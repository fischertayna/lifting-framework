module Variability.VarTypes where

type PresenceCondition = Bool

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t] deriving (Show)

valList :: Var a -> [Val a]
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

union :: Var a -> Var a -> Var a
union (Var ls1) (Var ls2) = Var $ ls1 ++ ls2

unions :: [Var t] -> Var t
unions = foldr union (Var [])

(|||) :: Var a -> PresenceCondition -> Var a
(Var listPCv) ||| pcR = Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc && pcR, sat pc'])

(+++) :: Var a -> Var a -> Var a
(Var lvint1) +++ (Var lvint2) = Var (lvint1 ++ lvint2)

apply_ :: Val (a -> b) -> Var a -> Var b
apply_ (fn, fnpc) x'@(Var x) =
  Var [(fn v, pc') | (v, !pc) <- x, let pc' = fnpc /\ pc, sat pc']

apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x = unions [apply_ f x | f <- fn]