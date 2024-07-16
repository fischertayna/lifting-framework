module Language.VInterpreter.Driver where

import Language.Frontend.AbsLanguage
import Language.Frontend.ErrM
import Language.Frontend.LexLanguage ()
import Language.Frontend.ParLanguage (myLexer, pProgram)
import Language.VInterpreter.Interpreter
import Variability.VarTypes (PresenceCondition, VarValor(..), Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import Language.VInterpreter.Composition

main :: IO ()
main = do
  interact calc
  putStrLn ""

pcA :: Prop
pcA = mkBDDVar "A"

varInt, varList :: VarValor
varInt = VarInteger (Var [(2, pcA), (3, notBDD pcA)])
varList = VarList [
                      VarInteger (Var [(8, pcA), ( 5, notBDD pcA)]),
                      VarInteger (Var [(2, pcA), ( 1, notBDD pcA)]),
                      VarInteger (Var [(3, pcA), ( 2, notBDD pcA)])
                    ]

chunkSize :: Int
chunkSize = 2

executeProg :: String -> VarValor -> VarValor
executeProg prog input =
  let Ok p = pProgram (myLexer prog)
  in (evalPV p input)

executeProgInChunks :: Int -> String -> VarValor -> VarValor
executeProgInChunks chunkSize prog input =
  let Ok p = pProgram (myLexer prog)
  in (uVarValores (evalPVChunk p (chunksOfVar chunkSize input)))

calc :: String -> String
calc s =
  let r = executeProg s varInt
  in show $ r

calcInChunks :: Int -> String -> String
calcInChunks chunkSize s =
  let r = executeProgInChunks chunkSize s varList
  in show $ r