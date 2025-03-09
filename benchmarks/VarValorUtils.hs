module VarValorUtils where

import Base.Types (Valor(..))
import Variability.VarTypes

import Debug.Trace (trace)

flattenVarValor :: VarValor -> [(PresenceCondition, Valor)]
flattenVarValor (VarInteger (Var xs)) =
    [ (pc, trace ("Integer PC: " ++ show pc ++ ", Valor: " ++ show (ValorInt i)) (ValorInt i))
    | (i, pc) <- xs, sat pc ]

flattenVarValor (VarBool (Var xs)) =
    [ (pc, trace ("Bool PC: " ++ show pc ++ ", Valor: " ++ show (ValorBool b)) (ValorBool b))
    | (b, pc) <- xs, sat pc ]

flattenVarValor (VarString (Var xs)) =
    [ (pc, trace ("String PC: " ++ show pc ++ ", Valor: " ++ show (ValorStr s)) (ValorStr s))
    | (s, pc) <- xs, sat pc ]

flattenVarValor (VarList elems) =
    [ (pc, trace ("List PC: " ++ show pc ++ ", ValorList: " ++ show vals) (ValorList vals))
    | listVariants <- sequence (map flattenVarValor elems)
    , let pcs = map fst listVariants
    , let vals = map snd listVariants
    , let pc = foldr1 (/\) pcs
    , sat pc
    ]

flattenVarValor (VarPair (v1, v2)) =
    [ let val = ValorPair (val1, val2)
       in (pc1 /\ pc2, trace ("Pair PC: " ++ show (pc1 /\ pc2) ++ ", Valor: " ++ show val) val)
    | (pc1, val1) <- flattenVarValor v1
    , (pc2, val2) <- flattenVarValor v2
    , sat (pc1 /\ pc2)
    ]


extractValues :: VarValor -> [Valor]
extractValues v =
    let res = map snd (flattenVarValor v)
    in trace ("extractValues INPUT: " ++ show v ++ "\nOUTPUT: " ++ show res) res

