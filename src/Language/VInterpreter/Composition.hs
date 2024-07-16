module Language.VInterpreter.Composition where

import Prelude
import Data.List (transpose)
import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)
import Language.VInterpreter.Interpreter
import Variability.VarTypes (PresenceCondition, VarValor(..), Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- Função chunksOf para VarValor
chunksOfVar :: Int -> VarValor -> [VarValor]
chunksOfVar i (VarInteger (Var vals)) = map (VarInteger . Var) (chunksOf i vals)
chunksOfVar i (VarBool (Var vals)) = map (VarBool . Var) (chunksOf i vals)
chunksOfVar i (VarString (Var vals)) = map (VarString . Var) (chunksOf i vals)
chunksOfVar i (VarList vals) = map VarList (transpose [chunksOfVar i v | v <- vals])
chunksOfVar i (VarPair (v1, v2)) = [VarPair (x, y) | x <- chunksOfVar i v1, y <- chunksOfVar i v2]

uVarValores :: [VarValor] -> VarValor
uVarValores varValores = case head varValores of
    VarInteger _ -> VarInteger (Var (foldl (\acc (VarInteger (Var vals)) -> acc ++ vals) [] varValores))
    VarString _ -> VarString (Var (foldl (\acc (VarString (Var vals)) -> acc ++ vals) [] varValores))
    VarBool _ -> VarBool (Var (foldl (\acc (VarBool (Var vals)) -> acc ++ vals) [] varValores))

evalPVChunk :: Program -> [VarValor] -> [VarValor]
evalPVChunk _ [] = [] -- Caso base: lista vazia
evalPVChunk p (x:xs) = evalPV p x : evalPVChunk p xs