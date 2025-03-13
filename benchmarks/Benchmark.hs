module Benchmark where

import ComplexExamples
import WhileLang.WhileEncoder
import Control.DeepSeq (deepseq, NFData(..))
import System.Mem (performGC)
import System.IO
import qualified Data.Map as Map
import qualified Language.Interpreter.Driver as BaseInterpreter
import qualified Language.MInterpreter.Driver as MInterpreter
import qualified Language.VInterpreter.Driver as VInterpreter
import qualified Language.VMemoInterpreter.Driver as VMemoInterpreter
import Base.Types (Valor(..))
import Variability.VarTypes (VarValor(..), Var(..), Val, PresenceCondition, sat, andBDD, valList, ttPC)
import Memoization.Core.Memory (KeyValueArray, FuncKey(..), resetCounters, sumCounters)
import Memoization.Core.State (State(..))
import Paths_lifting_framework (getDataFileName)
import System.Directory (doesFileExist, createDirectoryIfMissing, getFileSize)
import System.IO.Error (tryIOError)
import Debug.Trace (trace)
import VarValorUtils (flattenVarValor, extractValues)
import System.FilePath (takeBaseName)

outputDir = "benchmark_output/"

-- Interpreters
data Interpreter = Base | Variational | Memoized | VMemoized
    deriving (Show, Eq)

type ExecutionResult = (String) -- only output result value string

newtype MemState v = MemState (KeyValueArray FuncKey v)
    deriving (Show, Read)

analyses :: [(String, [String])]
analyses = [ ("src/Language/Analysis/DFA/reachingDefinitions.lng", 
              ["labels", "flow", "fv", "assignments", "init", "final", "findBlock", "makeSetOfFV", "killRD", "genRD", "filterFlow"]),
             ("src/Language/Analysis/DFA/liveVariables.lng", 
              ["getVarFromAexp", "getVarFromBexp", "labels", "flowR", "flow", "fv", "init", "final", "findBlock", "killLV", "genLV", "filterFlow"]),
             ("src/Language/Analysis/DFA/availableExpressions.lng", 
              ["nonTrivialExpression", "labels", "flow", "fv", "init", "final", "findBlock", "killAE", "genAE", "filterFlow"]),
             ("src/Language/Analysis/DFA/veryBusyExpressions.lng", 
              ["labels", "flow", "flowR", "fv", "init", "final", "findBlock", "killVB", "genVB", "filterFlow"])
           ]

runAnalysis :: Interpreter -> (String, [String]) -> Program -> Either (MemState Valor) (MemState VarValor) -> IO (ExecutionResult, Either (MemState Valor) (MemState VarValor))
runAnalysis interpreter (analysisFile, memoizedFunctions) program memState = do
    filePath <- getDataFileName analysisFile
    content <- readFile filePath
    performGC
    let (result, newMemState) = case (interpreter, memState) of
            (Base, Left mem)      -> let (r, m) = evaluateInterpreter Base content program mem memoizedFunctions in (r, Left m)
            (Memoized, Left mem)  -> let (r, m) = evaluateInterpreter Memoized content program mem memoizedFunctions in (r, Left m)
            (Variational, Right mem) -> let (r, m) = evaluateInterpreterVariational Variational content program mem memoizedFunctions in (r, Right m)
            (VMemoized, Right mem)  -> let (r, m) = evaluateInterpreterVariational VMemoized content program mem memoizedFunctions in (r, Right m)
            _ -> error ("Invalid memory state type for the chosen interpreter: " ++ show interpreter ++ ", " ++ show memState)
    return ((result), newMemState)

evaluateInterpreter :: Interpreter -> String -> Program -> MemState Valor -> [String] -> (String, MemState Valor)
evaluateInterpreter Base analysis program _ _ = 
    let encodedInputs = encodeVariability program
        results = map (BaseInterpreter.executeProg analysis) encodedInputs
    in (show results, MemState [])
evaluateInterpreter Memoized analysis program (MemState memState) memoizedFunctions = 
    let encodedInputs = encodeVariability program
        (results, finalMemState) = foldl (\(accResults, accMem) input -> 
            let (res, newMem) = MInterpreter.executeProg memoizedFunctions accMem analysis input
            in (accResults ++ [res], newMem)) ([], memState) encodedInputs
    in (show results, MemState finalMemState)

evaluateInterpreterVariational :: Interpreter -> String -> Program -> MemState VarValor -> [String] -> (String, MemState VarValor)
evaluateInterpreterVariational Variational analysis program _ _ = 
    let varResult = VInterpreter.executeProg analysis (encodeStmt program)
    in (show varResult, MemState [])
evaluateInterpreterVariational VMemoized analysis program (MemState memState) memoizedFunctions = 
    let (varResult, newMemState) = VMemoInterpreter.executeProg memoizedFunctions memState analysis (encodeStmt program)
    in (show varResult, MemState newMemState)

memoryFileName :: Interpreter -> String -> String
memoryFileName Memoized label = outputDir ++ "memoized_state_" ++ label ++ ".dat"
memoryFileName VMemoized label = outputDir ++ "vmemoized_state_" ++ label ++ ".dat"
memoryFileName _ _ = ""

saveMemoryState :: (Read v, Show v) => Interpreter -> String -> MemState v -> IO ()
saveMemoryState Memoized label memState = writeFile (memoryFileName Memoized label) (show memState)
saveMemoryState VMemoized label memState = writeFile (memoryFileName VMemoized label) (show memState)
saveMemoryState _ _ _ = return ()

loadMemoryState :: (Read v, Show v) => Interpreter -> String -> Bool -> IO (MemState v)
loadMemoryState Memoized label resetMem = do
    let file = memoryFileName Memoized label
    exists <- doesFileExist file
    if exists
        then do
            contentResult <- tryIOError (readFile file)
            case contentResult of
                Left _ -> return (MemState [])
                Right content -> case reads content of
                    [(mem, "")] -> return (if resetMem then resetAfterLoad mem else mem)
                    _ -> return (MemState [])
        else return (MemState [])
loadMemoryState VMemoized label resetMem = do
    let file = memoryFileName VMemoized label
    exists <- doesFileExist file
    if exists
        then do
            contentResult <- tryIOError (readFile file)
            case contentResult of
                Left _ -> return (MemState [])
                Right content -> case reads content of
                    [(mem, "")] -> return (if resetMem then resetAfterLoad mem else mem)
                    _ -> return (MemState [])
        else return (MemState [])
loadMemoryState _ _ _ = return (MemState [])


resetAfterLoad :: MemState v -> MemState v
resetAfterLoad (MemState memState) = MemState (snd (resetCounters `runState` memState))

resetAfterLoadVar :: MemState VarValor -> MemState VarValor
resetAfterLoadVar (MemState memState) = MemState (snd (resetCounters `runState` memState))
