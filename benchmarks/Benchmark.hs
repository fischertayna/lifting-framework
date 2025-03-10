module Benchmark where

import WhileLang.ComplexExamples
import WhileLang.RunningExample
import WhileLang.WhileEncoder
import System.CPUTime
import Control.DeepSeq (deepseq, NFData(..))
import System.Mem (performGC)
import System.IO
import Data.Time.Clock
import qualified Data.Map as Map
import qualified Language.Interpreter.Driver as BaseInterpreter
import qualified Language.MInterpreter.Driver as MInterpreter
import qualified Language.VInterpreter.Driver as VInterpreter
import qualified Language.VMemoInterpreter.Driver as VMemoInterpreter
import Base.Types (Valor(..))
import Variability.VarTypes (VarValor(..), Var(..), Val, PresenceCondition, sat, andBDD, valList, ttPC)
import Memoization.Core.Memory (KeyValueArray, FuncKey(..), resetCounters, sumCounters)
import Memoization.Core.State (State(..))
import GHC.Stats (getRTSStats, RTSStats(..))
import Paths_lifting_framework (getDataFileName)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO.Error (tryIOError)
import Debug.Trace (trace)
import VarValorUtils (flattenVarValor, extractValues)
import Control.Concurrent (threadDelay)

outputDir = "benchmark_output/"

-- Interpreters
data Interpreter = Base | Variational | Memoized | VMemoized
    deriving (Show, Eq)

type ExecutionResult = (Double, Integer, Int, Int, String) -- (Runtime, Memory, Cache Miss, Cache Hits, Result)

newtype MemState v = MemState (KeyValueArray FuncKey v)
    deriving (Show, Read)

-- List of analyses with their respective memoized function names
analyses :: [(String, [String])]
analyses = [ ("src/Language/Examples/DFA/reachingDefinitions.lng", 
              ["labels", "flow", "fv", "assignments", "init", "final", "findBlock", "makeSetOfFV", "killRD", "genRD", "filterFlow"]),
             ("src/Language/Examples/DFA/liveVariables.lng", 
              ["getVarFromAexp", "getVarFromBexp", "labels", "flowR", "flow", "fv", "init", "final", "findBlock", "killLV", "genLV", "filterFlow"]),
             ("src/Language/Examples/DFA/availableExpressions.lng", 
              ["nonTrivialExpression", "labels", "flow", "fv", "init", "final", "findBlock", "killAE", "genAE", "filterFlow"]),
             ("src/Language/Examples/DFA/veryBusyExpressions.lng", 
              ["labels", "flow", "flowR", "fv", "init", "final", "findBlock", "killVB", "genVB", "filterFlow"])
           ]

-- Run analysis using appropriate interpreter
runAnalysis :: Interpreter -> (String, [String]) -> Program -> Either (MemState Valor) (MemState VarValor) -> IO (ExecutionResult, Either (MemState Valor) (MemState VarValor))
runAnalysis interpreter (analysisFile, memoizedFunctions) program memState = do
    filePath <- getDataFileName analysisFile
    content <- readFile filePath
    performGC
    startTime <- getCPUTime
    let (result, newMemState) = case (interpreter, memState) of
            (Base, Left mem)      -> let (r, m) = evaluateInterpreter Base content program mem memoizedFunctions in (r, Left m)
            (Memoized, Left mem)  -> let (r, m) = evaluateInterpreter Memoized content program mem memoizedFunctions in (r, Left m)
            (Variational, Right mem) -> let (r, m) = evaluateInterpreterVariational Variational content program mem memoizedFunctions in (r, Right m)
            (VMemoized, Right mem)  -> let (r, m) = evaluateInterpreterVariational VMemoized content program mem memoizedFunctions in (r, Right m)
            _ -> error ("Invalid memory state type for the chosen interpreter: " ++ show interpreter ++ ", " ++ show memState)
    endTime <- getCPUTime
    let runtime = fromIntegral (endTime - startTime) / (10^6) -- runtime in microseconds
    memoryUsage <- getMemoryUsage
    let (cacheMiss, cacheHits) = case newMemState of
            Left mem -> getMemoizationStats Memoized mem
            Right mem -> getMemoizationStats VMemoized mem
    return ((runtime, memoryUsage, cacheMiss, cacheHits, result), newMemState)

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "Expected Left value"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right value"

evaluateInterpreter :: Interpreter -> String -> Program -> MemState Valor -> [String] -> (String, MemState Valor)
evaluateInterpreter Base analysis program _ _ = 
    let encodedInputs = encodeVariability program
        results = map (BaseInterpreter.executeProg analysis) encodedInputs
    in (show results, MemState [])
evaluateInterpreter Memoized analysis program (MemState memState) memoizedFunctions = 
    let encodedInputs = encodeVariability program
        (results, finalMemState) = foldl (\(accResults, accMem) input -> 
            let (res, newMem) = MInterpreter.executeProg memoizedFunctions accMem analysis input
            in (accResults ++ [res], newMem)
          ) ([], memState) encodedInputs
    in (show results, MemState finalMemState)

evaluateInterpreterVariational :: Interpreter -> String -> Program -> MemState VarValor -> [String] -> (String, MemState VarValor)
evaluateInterpreterVariational Variational analysis program _ _ = 
    let varResult = VInterpreter.executeProg analysis (encodeStmt program)
    in (show varResult, MemState [])
evaluateInterpreterVariational VMemoized analysis program (MemState memState) memoizedFunctions = 
    let (varResult, newMemState) = VMemoInterpreter.executeProg memoizedFunctions memState analysis (encodeStmt program)
    in (show varResult, MemState newMemState)

getMemoryUsage :: IO Integer
getMemoryUsage = do
    stats <- getRTSStats
    return (fromIntegral (max_live_bytes stats))

sumCountersResult :: KeyValueArray FuncKey v -> Int
sumCountersResult memState = fst (sumCounters `runState` memState)

getMemoizationStats :: Interpreter -> MemState v -> (Int, Int)
getMemoizationStats Memoized (MemState memState) = (length memState, sumCountersResult memState)
getMemoizationStats VMemoized (MemState memState) = (length memState, sumCountersResult memState)
getMemoizationStats _ _ = (0, 0)

memoryFileName :: Interpreter -> String -> String
memoryFileName Memoized label = outputDir ++ "memoized_state_" ++ label ++ ".dat"
memoryFileName VMemoized label = outputDir ++ "vmemoized_state_" ++ label ++ ".dat"
memoryFileName _ _ = ""

saveMemoryState :: (Read v, Show v) => Interpreter -> String -> MemState v -> IO ()
saveMemoryState Memoized label memState = writeFile (memoryFileName Memoized label) (show memState)
saveMemoryState VMemoized label memState = writeFile (memoryFileName VMemoized label) (show memState)
saveMemoryState _ _ _ = return ()

loadMemoryState :: (Read v, Show v) => Interpreter -> String -> IO (MemState v)
loadMemoryState Memoized label = do
    let file = memoryFileName Memoized label
    exists <- doesFileExist file
    if exists
        then do
            contentResult <- tryIOError (readFile file)
            case contentResult of
                Left _ -> return (MemState [])
                Right content ->
                    case reads content of
                        [(mem, "")] -> return (resetAfterLoad mem)
                        _ -> return (MemState [])
        else return (MemState [])

loadMemoryState VMemoized label = do
    let file = memoryFileName VMemoized label
    exists <- doesFileExist file
    if exists
        then do
            contentResult <- tryIOError (readFile file)
            case contentResult of
                Left _ -> return (MemState [])
                Right content ->
                    case reads content of
                        [(mem, "")] -> return (resetAfterLoad mem)
                        _ -> return (MemState [])
        else return (MemState [])
loadMemoryState _ _ = return (MemState [])

resetAfterLoad :: MemState v -> MemState v
resetAfterLoad (MemState memState) = MemState (snd (resetCounters `runState` memState))

formatRuntime :: Double -> String
formatRuntime r = show r ++ " µs"

writeResults :: [Interpreter] -> [(String, [ExecutionResult], [ExecutionResult])] -> IO ()
writeResults interpreters results = do
    createDirectoryIfMissing True outputDir
    withFile (outputDir ++ "benchmark_metrics.csv") WriteMode $ \metricsHandle ->
      withFile (outputDir ++ "benchmark_results.csv") WriteMode $ \resultsHandle -> do
        hPutStrLn metricsHandle "Program,Interpreter,Version,Runtime,Memory,CacheMiss,CacheHits"
        hPutStrLn resultsHandle "Program,Interpreter,Version,Results"
        mapM_ (writeEntries metricsHandle resultsHandle) results
  where
    writeEntries metricsHandle resultsHandle (name, res1, res2) =
        let baseResults1 = case lookup Base (zip interpreters res1) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
            baseResults2 = case lookup Base (zip interpreters res2) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
        in mapM_ (\(interpreter, res1Entry, res2Entry) ->
                   writeEntry metricsHandle resultsHandle name interpreter res1Entry res2Entry baseResults1 baseResults2
                 ) (zip3 interpreters res1 res2)

    writeEntry metricsHandle resultsHandle name interpreter
               (runtime1, memory1, cacheMiss1, cacheHits1, results1)
               (runtime2, memory2, cacheMiss2, cacheHits2, results2)
               baseResults1 baseResults2 = do
        -- let baseComparison1 = show (results1 == baseResults1)
        --     baseComparison2 = show (results2 == baseResults2)
        -- Write metrics
        hPutStrLn metricsHandle $ name ++ "," ++ show interpreter ++ ",V1," ++ formatRuntime runtime1 ++ "," ++ show memory1 ++ "," ++ show cacheMiss1 ++ "," ++ show cacheHits1
        hPutStrLn metricsHandle $ name ++ "," ++ show interpreter ++ ",V2," ++ formatRuntime runtime2 ++ "," ++ show memory2 ++ "," ++ show (cacheMiss2 - cacheMiss1) ++ "," ++ show cacheHits2
        -- Write results
        hPutStrLn resultsHandle $ name ++ "," ++ show interpreter ++ ",V1," ++ show results1
        hPutStrLn resultsHandle $ name ++ "," ++ show interpreter ++ ",V2," ++ show results2

runExperiments :: IO ()
runExperiments = do
    createDirectoryIfMissing True outputDir
    let programs = [("Running-Example", running_example_variability, running_example_variability_v2),
                    ("Deep Loop", deep_loop, deep_loop_v2),
                    ("Nested Loop", nested_variability, nested_variability_v2),
                    ("Interprocedural", interprocedural_sim, interprocedural_sim_v2),
                    ("Recursion Sim", factorial_rec_sim, factorial_rec_sim_v2),
                    ("Arithmetic Heavy", arithmetic_heavy, arithmetic_heavy_v2)]
    
    let interpreters = [Base, Variational, Memoized, VMemoized]

    results <- mapM (\(name, prog1, prog2) -> do
        putStrLn $ "Running " ++ name ++ "..."
        let label1 = "reachingDefinitions_" ++ name ++ "_V1"
        let label2 = "reachingDefinitions_" ++ name ++ "_V2"

        res1 <- mapM (\i -> do
            memState <- case i of
                Base -> return (Left (MemState []))
                Memoized -> Left <$> loadMemoryState Memoized label1
                Variational -> return (Right (MemState []))
                VMemoized -> Right <$> loadMemoryState VMemoized label1
            runAnalysis i (head analyses) prog1 memState
            ) interpreters

        mapM_ (\(i, (_, mem)) -> case (i, mem) of
            (Memoized, Left m) -> saveMemoryState Memoized label1 m
            (VMemoized, Right m) -> saveMemoryState VMemoized label1 m
            _ -> return ()
            ) (zip interpreters res1)

        res2 <- mapM (\i -> do
            memState <- case i of
                Base -> return (Left (MemState []))
                Memoized -> Left <$> loadMemoryState Memoized label1
                Variational -> return (Right (MemState []))
                VMemoized -> Right <$> loadMemoryState VMemoized label1
            runAnalysis i (head analyses) prog2 memState
            ) interpreters

        mapM_ (\(i, (_, mem)) -> case (i, mem) of
            (Memoized, Left m) -> saveMemoryState Memoized label2 m
            (VMemoized, Right m) -> saveMemoryState VMemoized label2 m
            _ -> return ()
            ) (zip interpreters res2)

        return (name, map fst res1, map fst res2)) programs

    writeResults interpreters results