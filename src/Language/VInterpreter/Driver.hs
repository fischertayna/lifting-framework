module Language.VInterpreter.Driver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)
import Language.VInterpreter.Interpreter
import Variability.VarTypes (PresenceCondition, Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import Prelude
import Data.List (transpose)

main :: IO ()
main = do
  interact calc
  putStrLn ""

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

inputInt, inputBool, inputString, inputList, inputPair :: VarValor
inputInt = VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)])
inputBool = VarBool (Var [(True, atbt), ( False, afbt),  (False, atbf), (False, afbf)])
inputString = VarString (Var [("abc", atbt), ( "def", afbt),  ("ghi", atbf), ("jkl", afbf)])
inputList = VarList [
                      VarInteger (Var [(8, atbt), ( 5, afbt), (0, atbf), (1, afbf)]),
                      VarInteger (Var [(2, atbt), ( 1, afbt), (4, atbf), (6, afbf)]),
                      VarInteger (Var [(3, atbt), ( 2, afbt), (5, atbf), (2, afbf)])
                    ]
inputPair = VarPair (VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)]), VarInteger (Var [(2, atbt), ( 1, afbt),  (4, atbf), (6, afbf)]))

chunkSize :: Int
chunkSize = 2

calc :: String -> String
calc s =
  let Ok p = pProgram (myLexer s)
  --  in show $ evalPV p inputList
   in show $ uVarValores (evalPVChunk p (chunksOfVar chunkSize inputList))

evalPVChunk :: Program -> [VarValor] -> [VarValor]
evalPVChunk _ [] = [] -- Caso base: lista vazia
evalPVChunk p (x:xs) = evalPV p x : evalPVChunk p xs