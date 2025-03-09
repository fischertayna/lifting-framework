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
import Memoization.Core.Memory (KeyValueArray, FuncKey(..))
import GHC.Stats (getRTSStats, RTSStats(..))
import Paths_lifting_framework (getDataFileName)
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)
import Debug.Trace (trace)
import VarValorUtils (flattenVarValor, extractValues)

-- Interpreters
data Interpreter = Base | Variational | Memoized | VMemoized
    deriving (Show, Eq)

type ExecutionResult = (NominalDiffTime, Integer, Int, Int, String) -- (Runtime, Memory, Cache Hits, Cache Reuse, Result)

newtype MemState v = MemState (KeyValueArray FuncKey v)
    deriving (Show, Read)
    

-- List of analyses with their respective memoized function names
analyses :: [(String, [String])]
analyses = [ ("src/Language/Examples/DFA/reachingDefinitions.lng", 
              ["labels", "flow", "fv", "assignments", "init", "final", "findBlock", "makeSetOfFV", "killRD", "genRD", "filterFlow"]),
            --  ("src/Language/Examples/DFA/liveVariables.lng", 
            --   ["getVarFromAexp", "getVarFromBexp", "labels", "flowR", "flow", "fv", "init", "final", "findBlock", "killLV", "genLV", "filterFlow"]),
            --  ("src/Language/Examples/DFA/availableExpressions.lng", 
            --   ["nonTrivialExpression", "labels", "flow", "fv", "init", "final", "findBlock", "killAE", "genAE", "filterFlow"]),
            --  ("src/Language/Examples/DFA/veryBusyExpressions.lng", 
            --   ["labels", "flow", "flowR", "fv", "init", "final", "findBlock", "killVB", "genVB", "filterFlow"])
           ]

-- Run analysis using appropriate interpreter
runAnalysis :: Interpreter -> (String, [String]) -> Program -> Either (MemState Valor) (MemState VarValor) -> IO (ExecutionResult, Either (MemState Valor) (MemState VarValor))
runAnalysis interpreter (analysisFile, memoizedFunctions) program memState = do
    filePath <- getDataFileName analysisFile
    content <- readFile filePath
    performGC -- Ensure garbage collection before timing
    startTime <- getCurrentTime
    let (result, newMemState) = case (interpreter, memState) of
            (Base, Left mem)      -> let (r, m) = evaluateInterpreter Base content program mem memoizedFunctions in (r, Left m)
            (Memoized, Left mem)  -> let (r, m) = evaluateInterpreter Memoized content program mem memoizedFunctions in (r, Left m)
            (Variational, Right mem) -> let (r, m) = evaluateInterpreterVariational Variational content program mem memoizedFunctions in (r, Right m)
            (VMemoized, Right mem)  -> let (r, m) = evaluateInterpreterVariational VMemoized content program mem memoizedFunctions in (r, Right m)
            _ -> error ("Invalid memory state type for the chosen interpreter: " ++ show interpreter ++ ", " ++ show memState)
    endTime <- getCurrentTime
    let runtime = diffUTCTime endTime startTime
    memoryUsage <- getMemoryUsage
    let (cacheHits, cacheReuse) = case newMemState of
            Left mem -> getMemoizationStats Memoized mem
            Right mem -> getMemoizationStats VMemoized mem
    return ((runtime, memoryUsage, cacheHits, cacheReuse, result), newMemState)

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


-- Collect memory usage
getMemoryUsage :: IO Integer
getMemoryUsage = do
    stats <- getRTSStats
    return (fromIntegral (max_live_bytes stats))

-- Function to track memoization statistics
getMemoizationStats :: Interpreter -> MemState v -> (Int, Int)
getMemoizationStats Memoized (MemState memState) = (length memState, 0)
getMemoizationStats VMemoized (MemState memState) = (length memState, 0)
getMemoizationStats _ _ = (0, 0)

memoryFileName :: Interpreter -> String -> String
memoryFileName Memoized label = "memoized_state_" ++ label ++ ".dat"
memoryFileName VMemoized label = "vmemoized_state_" ++ label ++ ".dat"
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
                Right content -> case reads content of
                    [(mem, "")] -> return mem
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
                Right content -> case reads content of
                    [(mem, "")] -> return mem
                    _ -> return (MemState [])
        else return (MemState [])
loadMemoryState _ _ = return (MemState [])


-- Function to write results
writeResults :: [Interpreter] -> [(String, [ExecutionResult], [ExecutionResult])] -> IO ()
writeResults interpreters results = do
    withFile "benchmark_results.csv" WriteMode $ \h -> do
        hPutStrLn h "Program,Interpreter,Version,Runtime,Memory,CacheHits,CacheReuse,Results vs Base, Results"
        mapM_ (writeEntries h) results
  where
    writeEntries h (name, res1, res2) =
        let baseResults1 = case lookup Base (zip interpreters res1) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
            baseResults2 = case lookup Base (zip interpreters res2) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
        in mapM_ (\(interpreter, res1Entry, res2Entry) -> writeEntry h name interpreter res1Entry res2Entry baseResults1 baseResults2)
                 (zip3 interpreters res1 res2)

    writeEntry h name interpreter (runtime1, memory1, cacheHits1, cacheReuse1, results1)
                               (runtime2, memory2, cacheHits2, cacheReuse2, results2) baseResults1 baseResults2 =
        let baseComparison1 = show (results1 == baseResults1)
            baseComparison2 = show (results1 == baseResults2)
        in hPutStrLn h $ name ++ "," ++ show interpreter ++ ",V1," ++ show runtime1 ++ "," ++ show memory1 ++ "," ++ show cacheHits1 ++ "," ++ show cacheReuse1 ++ "," ++ baseComparison1 ++ "," ++ show results1 ++ "\n" ++
                     name ++ "," ++ show interpreter ++ ",V2," ++ show runtime2 ++ "," ++ show memory2 ++ "," ++ show cacheHits2 ++ "," ++ show cacheReuse2 ++ "," ++ baseComparison2 ++ ","  ++ show results2

-- Run experiments for all interpreters and analyses
runExperiments :: IO ()
runExperiments = do
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
                Base -> Left <$> loadMemoryState Base label1
                Memoized -> Left <$> loadMemoryState Memoized label1
                Variational -> Right <$> loadMemoryState Variational label1
                VMemoized -> Right <$> loadMemoryState VMemoized label1
            runAnalysis i (head analyses) prog1 memState
            ) interpreters

        mapM_ (\(i, (_, mem)) -> case mem of
            Left m -> saveMemoryState Memoized label1 m
            Right m -> saveMemoryState VMemoized label1 m
            ) (zip interpreters res1)

        res2 <- mapM (\i -> do
            memState <- case i of
                Base -> Left <$> loadMemoryState Base label2
                Memoized -> Left <$> loadMemoryState Memoized label2
                Variational -> Right <$> loadMemoryState Variational label2
                VMemoized -> Right <$> loadMemoryState VMemoized label2
            runAnalysis i (head analyses) prog2 memState
            ) interpreters

        return (name, map fst res1, map fst res2)) programs

    writeResults interpreters results